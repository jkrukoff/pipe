%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pipe).
-compile(inline).

-define(MATCH_ERROR(Error), (element(1, Error) == error)).
-define(SPEC_MONAD(For), -spec For(Fun :: monoid(), Value :: term()) -> term()).

-type monoid() :: fun((term()) -> term()).
-type monad() :: fun((term(), monoid()) -> term()).

%% API
-export([always/2,
         ignore/2,
         apply/2,
         if_ok/2,
         if_not_error/2,
         if_not_throw/2,
         if_not_exception/2,
         compose/2,
         via/2,
         pipe/3,
         line/2,
         ok/2,
         not_error/2,
         not_throw/2,
         not_exception/2]).

-export_type([monad/0]).

%%%===================================================================
%%% API
%%%===================================================================

?SPEC_MONAD(always).
always(Fun, Value) ->
    Fun(Value).

?SPEC_MONAD(ignore).
ignore(Fun, Value) ->
    Fun(Value),
    Value.

?SPEC_MONAD(apply).
apply(Fun, Value) when is_list(Value) ->
    erlang:apply(Fun, Value);
apply(Fun, Value) when is_tuple(Value) ->
    erlang:apply(Fun, tuple_to_list(Value)).

?SPEC_MONAD(if_ok).
if_ok(Fun, Value) ->
    case Value of
        {ok, Ok} ->
            Fun(Ok);
        _ ->
            Value
    end.

?SPEC_MONAD(if_not_error).
if_not_error(Fun, Value) ->
    case Value of
        {ok, Ok} ->
            Fun(Ok);
        Error when ?MATCH_ERROR(Error) ->
            Error;
        _ ->
            Fun(Value)
    end.

?SPEC_MONAD(if_not_throw).
if_not_throw(Fun, Value) ->
    Try = fun (Unwrapped) ->
        try Fun(Unwrapped)
        catch
            throw:Reason ->
                exception_as_error(throw, Reason)
        end
    end,
    case Value of
        {ok, Ok} ->
            Try(Ok);
        Error when ?MATCH_ERROR(Error) ->
            Error;
        _ ->
            Try(Value)
    end.

?SPEC_MONAD(if_not_exception).
if_not_exception(Fun, Value) ->
    case Value of
        {ok, Ok} ->
            try Fun(Ok)
            catch
                Class:Reason ->
                    exception_as_error(Class, Reason)
            end;
        Error when ?MATCH_ERROR(Error) ->
            Error;
        _ ->
            try Fun(Value)
            catch
                Class:Reason ->
                    exception_as_error(Class, Reason)
            end
    end.

-spec compose(monad(), monad()) -> monad().
compose(Monad1, Monad2) ->
    fun (Fun, Value) ->
            Monad1(fun (Inner) -> Monad2(Fun, Inner) end, Value)
    end.

via([_ | _] = Apply, Start) ->
    [Head | Tail] = lists:reverse(Apply),
    Composed = lists:foldl(fun compose/2, Head, Tail),
    fun (Value) -> Composed(Start, Value) end.

-spec pipe([monad()] | monad(), term(), [[fun((...) -> term())]] | [monoid()]) -> term().
pipe([_ | _] = Apply, Start, Funs) ->
    [Head | Tail] = lists:reverse(Apply),
    pipe(lists:foldl(fun compose/2, Head, Tail), Start, Funs);
pipe(Apply, Start, Funs) ->
    do_pipe(Apply, Start, Funs).

line(Start, Funs) ->
    do_pipe(fun always/2, Start, Funs).

ok(Start, Funs) ->
    do_pipe(fun if_ok/2, Start, Funs).

not_error(Start, Funs) ->
    do_pipe(fun if_not_error/2, Start, Funs).

not_throw(Start, Funs) ->
    do_pipe(fun if_not_throw/2, Start, Funs).

not_exception(Start, Funs) ->
    do_pipe(fun if_not_exception/2, Start, Funs).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

exception_as_error(Class, Reason) ->
    {error, {Class, Reason}}.

do_pipe(_Apply, Value, []) ->
    Value;
do_pipe(Apply, Value, [Fun | Funs]) ->
    do_pipe(Apply, Apply(Fun, Value), Funs).

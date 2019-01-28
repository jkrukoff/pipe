%%%-------------------------------------------------------------------
%%% @doc
%%% A set of specialized folds for piping a value through a list of
%%% functions.
%%% @end
%%%-------------------------------------------------------------------
-module(pipe).
-compile(inline).

-define(MATCH_ERROR(Error),
        (element(1, Error) == error)).
-define(SPEC_MONAD(For),
        -spec For(Fun :: monoid(), Value :: term()) -> term()).
-define(SPEC_PARTIAL_PIPE(For),
        -spec For(Start :: term(), Funs :: [monoid()]) -> term()).

-type monoid() :: fun((term()) -> term()).
-type monad() :: fun((monoid(), term()) -> term()).

%% API
-export([always/2,
         ignore/2,
         apply/2,
         if_ok/2,
         if_not_error/2,
         if_not_throw/2,
         if_not_exception/2,
         compose/1,
         via/2,
         pipe/3,
         line/2,
         ok/2,
         not_error/2,
         not_throw/2,
         not_exception/2]).

-export_type([monoid/0,
              monad/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% A function application that always executes the given function.
%% This is also the identity function.
%% @end
?SPEC_MONAD(always).
always(Fun, Value) ->
    Fun(Value).

%% @doc
%% A function application that ignores the return value of the given
%% function, instead passing the value through unchanged. Useful for
%% functions which are executed solely for their side effects.
%% @end
?SPEC_MONAD(ignore).
ignore(Fun, Value) ->
    Fun(Value),
    Value.

%% @doc
%% A function application that expands a list or tuple into multiple
%% arguments to apply to the given function. A generic adapter for
%% when single argument functions are difficult to derive.
%% @end
-spec apply(Fun :: fun((...) -> term()), Value :: term()) -> term().
apply(Fun, Value) when is_list(Value) ->
    erlang:apply(Fun, Value);
apply(Fun, Value) when is_tuple(Value) ->
    erlang:apply(Fun, tuple_to_list(Value)).

%% @doc
%% A function application that only applies the given function when
%% the value is an {ok, _} tuple, otherwise the given value is
%% returned unchanged. Best for conditional execution over well
%% behaved functions that always return {ok, _} or {error, _}
%% tuples.
%%
%% The value inside the {ok, _} tuple is unwrapped and used as the
%% argument to the called function.
%% @end
?SPEC_MONAD(if_ok).
if_ok(Fun, Value) ->
    case Value of
        {ok, Ok} ->
            Fun(Ok);
        _ ->
            Value
    end.

%% @doc
%% A function application that only applies the given function when
%% the value is not an {error, _} tuple, otherwise the given value
%% is returned unchanged. Best for conditional execution over
%% functions with a variety of return formats, where a best effort is
%% required.
%%
%% The value inside an {ok, _} tuple is unwrapped and used as the
%% argument to the called function, otherwise the value is used as is.
%% @end
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

%% @doc
%% A function application that only applies the given function when
%% the value is not an {error, _} tuple, otherwise the given value
%% is returned unchanged. Best for conditional execution over poorly
%% behaved functions which throw exceptions for expected failures.
%%
%% If the called function throws an exception, it will be caught and
%% converted to the form {error, {throw, Reason}}.
%%
%% The value inside an {ok, _} tuple is unwrapped and used as the
%% argument to the called function, otherwise the value is used as is.
%% @end
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

%% @doc
%% A function application that only applies the given function when
%% the value is not an {error, _} tuple, otherwise the given value
%% is returned unchanged. Best for when exceptions must not stop
%% execution and the calling code is prepared to match on and handle
%% caught exceptions.
%%
%% If the called function throws an exception of any type (including
%% exit signals), it will be caught and converted to the form {error,
%% {Class, Reason}}.
%%
%% The value inside an {ok, _} tuple is unwrapped and used as the
%% argument to the called function, otherwise the value is used as is.
%% @end
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

%% @doc
%% Combine a list of function applications into a single function
%% application.
%% @end
-spec compose(Monads :: [monad()]) -> monad().
compose([]) ->
    fun always/2;
compose([_ | _] = Monads) ->
    [Head | Tail] = lists:reverse(Monads),
    lists:foldl(fun compose/2, Head, Tail).

%% @doc
%% Combine a list of function applications with an unapplied single
%% argument function to create a pipeline stage that can have
%% customized behaviour.
%%
%% When used with a pipe function such as pipe:line/2, allows for each
%% stage to have custom error handling.
%% @end
-spec via(Apply :: [monad()], Start :: monoid()) -> monoid().
via(Apply, Start) ->
    Composed = compose(Apply),
    fun (Value) -> Composed(Start, Value) end.

%% @doc
%% The generic pipe function. This works as a specialized fold. The
%% initial value is used as an argument to the first unapplied
%% function in the given list, the result of that used as the argument
%% to the next function and so on. The returned value is the result of
%% the final function.
%%
%% Control over if each function is to be applied is given to the list
%% of function applications, which decide what to do based on the
%% current value to be applied. Function applications execute in left
%% to right order, with the leftmost being the first able to control
%% execution.
%%
%% To customize an individual pipe stage, use pipe:via/2. To pipe over
%% multiple argument functions, use pipe:apply/2.
%% @end
%% @see pipe:via/2
%% @see pipe:apply/2
-spec pipe(Apply :: [monad()] | monad(),
           Start :: term(),
           Funs :: [monoid()]) -> term().
pipe([_ | _] = Apply, Start, Funs) ->
    pipe(compose(Apply), Start, Funs);
pipe(Apply, Start, Funs) ->
    do_pipe(Apply, Start, Funs).

%% @doc
%% A pipe function which executes each function in turn, regardless of
%% the value returned.
%% @end
%% @see pipe:always/2
?SPEC_PARTIAL_PIPE(line).
line(Start, Funs) ->
    do_pipe(fun always/2, Start, Funs).

%% @doc
%% A pipe function which executes each function only when the returned
%% value is of the form {ok, _}. Ensure the initial value is also of
%% that form, otherwise none of the functions will be executed.
%% @end
%% @see pipe:if_ok/2
?SPEC_PARTIAL_PIPE(ok).
ok(Start, Funs) ->
    do_pipe(fun if_ok/2, Start, Funs).

%% @doc
%% A pipe function which executes each function only when the returned
%% value is not of the form {error, _}.
%% @end
%% @see pipe:if_not_error/2
?SPEC_PARTIAL_PIPE(not_error).
not_error(Start, Funs) ->
    do_pipe(fun if_not_error/2, Start, Funs).

%% @doc
%% A pipe function which executes each function only when the returned
%% value is not of the form {error, _}. Thrown exceptions are caught
%% and converted into error tuples.
%% @end
%% @see pipe:if_not_throw/2
?SPEC_PARTIAL_PIPE(not_throw).
not_throw(Start, Funs) ->
    do_pipe(fun if_not_throw/2, Start, Funs).

%% @doc
%% A pipe function which executes each function only when the returned
%% value is not of the form {error, _}. All exceptions are caught
%% and converted into error tuples.
%% @end
%% @see pipe:if_not_exception/2
?SPEC_PARTIAL_PIPE(not_exception).
not_exception(Start, Funs) ->
    do_pipe(fun if_not_exception/2, Start, Funs).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

compose(Monad1, Monad2) ->
    fun (Fun, Value) ->
            Monad1(fun (Inner) -> Monad2(Fun, Inner) end, Value)
    end.

exception_as_error(Class, Reason) ->
    {error, {Class, Reason}}.

do_pipe(_Apply, Value, []) ->
    Value;
do_pipe(Apply, Value, [Fun | Funs]) ->
    do_pipe(Apply, Apply(Fun, Value), Funs).

%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_pipe).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

always_test() ->
    ?assertEqual(value, pipe:always(identity(), value)).

ignore_test() ->
    ?assertEqual(value, pipe:ignore(constant(computed), value)).

apply_test() ->
    ?assertEqual(1, pipe:apply(fun erlang:'-'/2, {3, 2})).

if_ok_test_() ->
    error_monad_tests(fun pipe:if_ok/2,
                      computed,
                      value,
                      {error, value}) ++
    [?_assertThrow(exception,
                   pipe:if_ok(throws(exception), {ok, value})),
     ?_assertError(exception, 
                   pipe:if_ok(errors(exception), {ok, value}))].

if_not_error_test_() ->
    error_monad_tests(fun pipe:if_not_error/2,
                      computed,
                      computed,
                      {error, value}) ++
    [?_assertThrow(exception,
                   pipe:if_not_error(throws(exception), {ok, value})),
     ?_assertError(exception,
                   pipe:if_not_error(errors(exception), {ok, value}))].

if_not_throw_test_() ->
    error_monad_tests(fun pipe:if_not_throw/2,
                      computed,
                      computed,
                      {error, value}) ++
    [?_assertEqual({error, {throw, exception}},
                   pipe:if_not_throw(throws(exception), {ok, value})),
     ?_assertError(exception,
                   pipe:if_not_throw(errors(exception), {ok, value}))].

if_not_exception_test_() ->
    error_monad_tests(fun pipe:if_not_exception/2,
                      computed,
                      computed,
                      {error, value}) ++
    [?_assertEqual({error, {throw, exception}},
                   pipe:if_not_exception(throws(exception), {ok, value})),
     ?_assertEqual({error, {error, exception}},
                   pipe:if_not_exception(errors(exception), {ok, value}))].

compose_test() ->
    Composed = pipe:compose([fun pipe:if_ok/2, fun pipe:ignore/2]),
    ?assertEqual(value, Composed(constant(computed), {ok, value})).

via_test() ->
    Composed = pipe:via([fun pipe:if_ok/2, fun pipe:ignore/2],
                        constant(computed)),
    ?assertEqual(value, Composed({ok, value})).

pipe_test_() ->
    [{"single",
      ?_assertEqual(3, pipe:pipe(fun pipe:always/2,
                                 2,
                                 [not_commutative()]))},
     {"multiple",
      ?_assertEqual(5, pipe:pipe(fun pipe:always/2,
                                 value,
                                 [constant(2),
                                  not_commutative(),
                                  not_commutative()]))},
     {"multiple applies",
      ?_assertEqual(value, pipe:pipe([fun pipe:if_ok/2, fun pipe:ignore/2],
                                      {ok, value},
                                      [constant({ok, computed}),
                                       constant(computed)]))},
     {"apply ordering",
      ?_assertEqual({ok, value}, pipe:pipe([fun pipe:ignore/2, fun pipe:if_ok/2],
                                           {ok, value},
                                           [constant({ok, computed})]))},
     {"via",
      ?_assertEqual(value, pipe:pipe(fun pipe:if_ok/2,
                                     {ok, value},
                                     [pipe:via([fun pipe:ignore/2],
                                               constant({ok, computed})),
                                      constant(computed)]))},
     {"composable",
      ?_assertEqual({ok, value}, pipe:pipe(fun pipe:if_ok/2,
                                           pipe:pipe(fun pipe:if_ok/2,
                                                     {ok, value},
                                                     [ok()]),
                                           [ok()]))}].

pipe_line_test() ->
    ?assertEqual(computed,
                 pipe:line(value,
                           [identity(), constant(computed), identity()])).

pipe_ok_test_() ->
    error_pipe_tests(fun pipe:ok/2,
                     {ok, computed},
                     computed,
                     {error, computed}).

pipe_not_error_test_() ->
    error_pipe_tests(fun pipe:not_error/2,
                     {ok, computed},
                     {ok, computed},
                     {error, computed}).

pipe_not_throw_test_() ->
    error_pipe_tests(fun pipe:not_throw/2,
                     {ok, computed},
                     {ok, computed},
                     {error, computed}).

pipe_not_exception_test_() ->
    error_pipe_tests(fun pipe:not_exception/2,
                     {ok, computed},
                     {ok, computed},
                     {error, computed}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

identity() ->
    fun (X) -> X end.

constant(Value) ->
    fun (_) -> Value end.

not_commutative() ->
    fun (X) -> X * 2 - 1 end.

throws(Value) ->
    fun (_) -> throw(Value) end.

errors(Value) ->
    fun (_) -> erlang:error(Value) end.

ok() ->
    fun (Value) -> {ok, Value} end.

error_monad_tests(Monad, OkExpected, BareExpected, ErrorExpected) ->
    For = io_lib:format("~w ", [Monad]),
    [{lists:flatten([For, "ok"]),
      ?_assertEqual(OkExpected, Monad(constant(computed), {ok, value}))},
     {lists:flatten([For, "bare"]),
      ?_assertEqual(BareExpected, Monad(constant(computed), value))},
     {lists:flatten([For, "error"]),
      ?_assertEqual(ErrorExpected, Monad(constant(computed), {error, value}))}].

error_pipe_tests(Pipe, OkExpected, BareExpected, ErrorExpected) ->
    For = io_lib:format("~w ", [Pipe]),
    [{lists:flatten([For, "ok"]),
      ?_assertEqual(OkExpected,
                    Pipe({ok, value}, [constant({ok, computed}), ok(), ok()]))},
     {lists:flatten([For, "bare"]),
      ?_assertEqual(BareExpected,
                    Pipe({ok, value}, [constant(computed), ok(), ok()]))},
     {lists:flatten([For, "error"]),
      ?_assertEqual(ErrorExpected,
                    Pipe({ok, value}, [constant({error, computed}), ok(), ok()]))}].

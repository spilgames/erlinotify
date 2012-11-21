-module(erlinotify_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:start(ets_manager),
    application:start(erlinotify).

cleanup(_Arg) ->
    application:stop(erlinotify),
    application:stop(ets_manager).

basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() -> erlinotify:watch("/tmp"),
              ?_assertMatch(ok,ok) end
    }.

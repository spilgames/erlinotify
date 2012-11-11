-module(erlinotify_nif).

-export([start/0,
         stop/1,
         init/0,
         add_watch/2,
         remove_watch/2]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    io:fwrite("~p~n",[self()]),
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

start() ->
    ?nif_stub.

stop(_Ref) ->
    ?nif_stub.

add_watch(_Ref, _Dirname) ->
    ?nif_stub.

remove_watch(_Ref, _Wd) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = start(),
    ?assertEqual({ok,1}, add_watch(Ref, "/tmp/")),
    ?assertEqual(ok, remove_watch(Ref, 1)),
    ?assertEqual(ok, stop(Ref)).


-endif.

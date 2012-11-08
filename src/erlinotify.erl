-module(erlinotify).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(log(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE},T])).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {port, fd, callback}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

watch(File) ->
  start(),
  gen_server:cast(?MODULE, {watch, File}).

unwatch(File) ->
  gen_server:cast(?MODULE, {unwatch, File}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Callback]) ->
    Port =open_port(),
    {ok,FD} = talk_to_port(Port, {open}),
    {ok, #state{port=Port, fd=FD, callback=Callback}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({watch, Watch}, State) ->
  {noreply, do_watch(Watch, State)};
handle_cast({unwatch, Unwatch}, State) ->
  {noreply, do_unwatch(Unwatch, State)};
handle_cast(Msg, State) ->
  ?log({unknown_message, Msg}),
  {noreply, State}.

handle_info({Port, {data, Msg}}, State = #state{port=Port, callback=Callback}) ->
  Callback(binary_to_term(Msg)),
  {noreply, State};
handle_info(Info, State) ->
  ?log({unknown_message, Info}),
  {noreply, State}.

terminate(_Reason, State) ->
  talk_to_port(State#state.port, {close, State#state.fd}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

open_port() ->
    Parts = [[filename:dirname(code:which(?MODULE)), "..", c_src],
             [code:priv_dir(inotify), bin]],
    try E = take_first(fun to_file/1, Parts),
          io:fwrite("using: ~p~n", [E]),
          open_port({spawn, E},[{packet, 2}, binary, exit_status])
    catch _:_ -> exit({inotify_binary_not_found, Parts})
    end.

to_file(Parts) ->
    true = filelib:is_regular(F=filename:join(Parts++[inotify])),
    F.

take_first(_,[]) -> exit({take_first, nothing_worked});
take_first(F,[H|T]) ->
    try F(H)
    catch _:_ -> take_first(F,T)
    end.

do_watch(File, State) ->
    try talk_to_port(State#state.port,{add, State#state.fd, File, all})
    of {ok, WD} -> State
    catch C:R ->
            ?log([{error_watching_file, File},{C, R}]),
            State
    end.

do_unwatch(File, State) ->
    try
        talk_to_port(State#state.port, {remove, State#state.fd, File}),
        State
    catch C:R ->
            ?log([{error_unwatching_file, File}, {C, R}]),
            State
    end.

talk_to_port(Port, Msg) ->
  try
      erlang:port_command(Port, term_to_binary(Msg)),
      receive {Port, {data, D = <<131,104,2,_/binary>>}} ->
              binary_to_term(D)
      after 1000 ->
              throw(talk_to_port_timeout)
      end
  catch _:R ->
          throw({talking_to_port_failed, {R, Port, Msg}})
  end.

-module(erlinotify).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(log(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE},T])).
%% TODO
%% Docs
%% Ets table to store Dir
%% Callback addition

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

%% @type state() = Record :: #state{ tablename = string(),
%%                                   fd = term(),
%%                                   callback= term() }.

-record(state, {tablename="erlinotify_ets", fd, callback}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([]) ->
    {ok,Fd} = erlinotify_nif:start(),
    io:fwrite(standard_error,"~p~n",[Fd]),
    X = fun(E) -> io:fwrite(standard_error,"~p~n",[E]) end,
    {ok, #state{fd=Fd, callback=X}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({watch, Watch}, State) ->
  {noreply, do_watch(Watch, State)};
handle_cast({unwatch, Unwatch}, State) ->
  {noreply, do_unwatch(Unwatch, State)};
handle_cast(Msg, State) ->
  ?log({unknown_message, Msg}),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({inotify_event, Wd, Type, Event, Cookie, Name}, State) ->
  CB = State#state.callback,
  CB({Wd, Type, Event, Cookie, Name}),
  {noreply, State};
handle_info(Info, State) ->
  ?log({unknown_message, Info}),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
    erlinotify_nif:stop(State#state.fd),
    {close, State#state.fd}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec (string(), state()) -> state()
%% @doc Makes a call to the nif to add a resource to
%% watch. Logs on error
do_watch(File, State) ->
    try erlinotify_nif:add_watch(State#state.fd, File)
    of {ok, _WD} -> State
    catch C:R ->
            ?log([{error_watching_file, File},{C, R}]),
            State
    end.

%% @spec (int(), state()) -> state()
%% @doc Makes a call to the nif to remove a resource to
%% watch. Logs on error
do_unwatch(Wd, State) ->
    try erlinotify_nif:remove_watch(State#state.fd, Wd),
        State
    catch C:R ->
            ?log([{error_unwatching_file, Wd}, {C, R}]),
            State
    end.


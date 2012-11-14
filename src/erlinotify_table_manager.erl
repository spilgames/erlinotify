-module(erlinotify_table_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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

%% @type state() = Record :: #state{ opts = term()
%%                                 }.

-record(state, {opts}).

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
    Opts = [set, protected, {keypos,1}, {heir,self()},
            {write_concurrency,false}, {read_concurrency,false}],
%%    Dir = ets:new(filenames, Opts),
%%    Wds = ets:new(watchdescriptors, Opts),
    {ok, #state{opts=Opts}}.

handle_call({get, dirname}, {Pid, _Tag}, State) ->
    %% create table if not exist
    %% else give back exisiting table to new process
    %% assign to pid
    {reply, ok, State};
handle_call({get, watchdescriptors}, {Pid, _Tag}, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% receive message from dead process. Note returned table.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


-module(worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([add/2]).
-export([set_state/2]).
-export([get_state/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(N) ->
    gen_server:start_link(?MODULE, [N], []).

add(Pid, N) -> gen_server:call(Pid, {add, N}).

set_state(Pid, NewState) -> gen_server:call(Pid, {set_state, NewState}).

get_state(Pid) -> gen_server:call(Pid, get_state).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	[N] = Args,
    {ok, N}.

handle_call({add, N}, _From, State) ->
	%% TODO fill code body.
    {reply, {ok, N}, State};

handle_call({set_state, NewState}, _From, _State) ->
	%% TODO fill code body.
    {reply, {ok, NewState} , NewState};

handle_call(get_state, _From, State) ->
	%% TODO fill code body.
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _N1) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


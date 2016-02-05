-module(worker).
-behaviour(gen_server).
-include("easymmo_ai.hrl").
-include("riak_core_vnode.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([stop/1]).

-export([add/2]).
-export([set_state/2]).
-export([get_state/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {luavm, chunk, counter}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(N) ->
    gen_server:start_link(?MODULE, [N], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

add(Pid, N) -> gen_server:call(Pid, {add, N}).

set_state(Pid, NewState) -> gen_server:call(Pid, {set_state, NewState}).

get_state(Pid) -> gen_server:call(Pid, get_state).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	[N] = Args,

	{ok, Chunk, _VM} = luerl:load("a = a + 1; return a;"),
	VM0 = luerl:init(),
	InitLua = lists:flatten(io_lib:format("a = ~p; return a", [N])),
	?PRINT({InitLua}),
	{_Ret1, VM1} = luerl:do(InitLua, VM0),

	%% setup worker gen_server State.
	State = #state{luavm = VM1, chunk = Chunk, counter = N},

    {ok, State}.

handle_call({add, N}, _From, State) ->
	#state{luavm = VM0, chunk = Chunk, counter = _Counter} = State,

	%% load add code
	%%AddLuaFile = filename:join([code:priv_dir(easymmo_ai), "lua", "add.lua"]),
	%%{ok, Chunk, VM1} = luerl:loadfile(AddLuaFile, VM0),

	%% run init.
	AddNLua = lists:flatten(io_lib:format("a = a+ ~p; return a", [N])),
	{Ret, VM1} = luerl:do(AddNLua, VM0),
	[H | T] = Ret,

	NewState = State#state{luavm = VM1, counter = N+1},

    {reply, {ok, H}, NewState};

handle_call({set_state, NewState}, _From, _State) ->
    {reply, {ok, NewState}, NewState};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(stop, _From, N1) ->
    {stop, normal, stopped, N1};

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


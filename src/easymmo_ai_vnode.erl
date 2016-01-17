-module(easymmo_ai_vnode).
-behaviour(riak_core_vnode).
-include("easymmo_ai.hrl").
-include("riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, pids}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition, pids=dict:new() }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    ?PRINT({ping, State}),
    {reply, {pong, State#state.partition}, State};

%% start blackboard service.
handle_command(blackboard_start, _Sender, State) ->
	{ok, Pid} = tiny_maxwell:start_link(),
	NewPids = dict:store(blackboard, Pid, State#state.pids),
    NewState = State#state{pids = NewPids},
    ?PRINT({blackboard_start, Pid}),
    {reply, {{blackboard_start, Pid}, State#state.partition}, NewState};

%% blackboard set_info.
handle_command({blackboard_set_info, Id, Value}, _Sender, State) ->
	Pid = dict:fetch(blackboard, State#state.pids),
	Result = tiny_maxwell:set_info(Pid, Id, Value),
    {reply, {{blackboard_set_info, Pid}, Result, State#state.partition}, State};

%% blackboard get_info.
handle_command({blackboard_get_info, Id}, _Sender, State) ->
	Pid = dict:fetch(blackboard, State#state.pids),
	Result = tiny_maxwell:get_info(Pid, Id),
    {reply, {{blackboard_get_info, Pid}, Result, State#state.partition}, State};

%% blackboard delete_info.
handle_command({blackboard_delete_info, Id}, _Sender, State) ->
	Pid = dict:fetch(blackboard, State#state.pids),
	Result = tiny_maxwell:delete_info(Pid, Id),
    {reply, {{blackboard_delete_info, Pid}, Result, State#state.partition}, State};

%% blackboard get_all_neighbors.
handle_command({blackboard_get_all_neighbors, Id}, _Sender, State) ->
	Pid = dict:fetch(blackboard, State#state.pids),
	Result = tiny_maxwell:get_all_neighbors(Pid, Id),
    {reply, {{blackboard_get_all_neighbors, Pid}, Result, State#state.partition}, State};


%% Name is new comer.
handle_command({addnew, Name, Code}, _Sender, State) ->
	{ok, Pid} = easymmo_ai_persona:start_link(Code),
	NewPids = dict:store(Name, Pid, State#state.pids),
    NewState = State#state{pids = NewPids},
    ?PRINT({addnew, Name, Code}),
    {reply, {{addnew, Pid}, State#state.partition}, NewState};

%% Find pid associated to Name
handle_command({button, Name, Button}, _Sender, State) ->
	Pid = dict:fetch(Name, State#state.pids),
	Result = easymmo_ai_persona:button(Pid, Button),
    {reply, {{button, Pid}, Result, State#state.partition}, State};

%% Find pid associated to Name
handle_command({get_state, Name}, _Sender, State) ->
	Pid = dict:fetch(Name, State#state.pids),
    {reply, {{get_state, Pid}, State#state.partition}, State};

%% Find pid associated to Name
handle_command({lookup, Name}, _Sender, State) ->
	Pid = dict:fetch(Name, State#state.pids),
    {reply, {{lookup, Pid}, State#state.partition}, State};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

%% get list of binnames
object_list(State) ->
	dict:fetch_keys(State#state.pids).

convtokey(BinName) ->
	{<<"easymmo_ai">>, BinName}.

gotvalue(BinName, State) ->
	local_get_state(BinName, State).

%% see http://jp.basho.com/posts/technical/understanding-riak_core-building-handoff/
%% object_list function returns a list of the keys to fold over.
%% write object_list by yourself.
handle_handoff_command(?FOLD_REQ{foldfun=VisitFun, acc0=Acc0}, _Sender, State) ->
	ObjectBinNames = object_list(State),
	%% eliding details for now. Don't worry, we'll get to them shortly.
	Do = fun(BinName, AccIn) ->
		K = convtokey(BinName),
		?PRINT(K),
		V = gotvalue(BinName, State),
		?PRINT(V),
		%%Data = term_to_binary({K, V}),
		Data = V, %% don't apply XXX_to_binary here.
		?PRINT(Data),
		AccOut = VisitFun(K, Data, AccIn),
		?PRINT(AccOut),
		AccOut
	end,
	Final = lists:foldl(Do, Acc0, ObjectBinNames),
	{reply, Final, State};

handle_handoff_command(_Message, _Sender, State) ->
	{noreply, State}.


handoff_starting(_TargetNode, State) ->
    ?PRINT({handoff_starting, _TargetNode}),
    {true, State}.

handoff_cancelled(State) ->
    ?PRINT({handoff_cancelled, none}),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    ?PRINT({handoff_finished, _TargetNode}),
    {ok, State}.

handle_handoff_data(Data, State) ->
	{{<<"easymmo_ai">>, BinName} , WorkerState} = binary_to_term(Data),
	?PRINT({BinName, WorkerState}),
	NewState = case dict:find(BinName, State#state.pids) of
		{ok, _ExistingPid} ->
			State;
		error ->
			?PRINT({BinName, WorkerState}),
			start_and_set_state({BinName, WorkerState}, State)
	end,
	{reply, ok, NewState}.

%% returns State
start_and_set_state({BinName, WorkerState}, State) ->
	?PRINT({start_and_set_state, BinName, WorkerState}),
	{ok, Pid} = worker:start_link(),
	{ok, _NewWorkerState} = worker:set_state(Pid, WorkerState),
	NewPids = dict:store(BinName, Pid, State#state.pids),
	NewState = State#state{pids = NewPids},
	NewState.

local_get_state(BinName, State) ->
	Pid = dict:fetch(BinName, State#state.pids),
	{ok, Result} = worker:get_state(Pid),
	Result.

encode_handoff_item(BinKey, Value) ->
	term_to_binary({BinKey, Value}).

is_empty(State) ->
	{dict:size(State#state.pids) == 0, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    ?PRINT({handle_coverage, _Req, _KeySpaces, _Sender}),
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    ?PRINT({handle_exit, _Pid, _Reason}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


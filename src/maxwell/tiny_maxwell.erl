-module(tiny_maxwell).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-export([test/0]).

-record(info, {id, map, x, y}).
-record(state, {by_id, by_map}).


%%
%% Public APIs
%%

test() ->
	start_link(),
	Info1_1_1 = #info{id=1, map=m1, x=101, y=101},
	Info1_1_2 = #info{id=1, map=m1, x=102, y=102},
	Info1_2_1 = #info{id=1, map=m2, x=101, y=101},
	Info2_1_1 = #info{id=2, map=m1, x=101, y=101},
	Info2_2_2 = #info{id=2, map=m2, x=102, y=101},
	set_info(1, Info1_1_1),
	io:format("~p~n", [get_all_neighbors(1)]),
	set_info(1, Info1_1_2),
	io:format("~p~n", [get_all_neighbors(1)]),
	set_info(2, Info2_1_1),
	io:format("~p~n", [get_all_neighbors(1)]).

get_info(Id) ->
	{ok, V} = gen_server:call(?MODULE, {get_info, Id}) ,
	V.

get_info(Id, Default) ->
	{ok, V} = gen_server:call(?MODULE, {get_info, Id}) ,
	{ok, case V of 
		undefined -> Default;
		V -> V
	end}.

set_info(Id, NewLocation) ->
	gen_server:call(?MODULE, {set_info, Id, NewLocation}).

delete_info(Id) ->
	gen_server:call(?MODULE, {delete_info, Id}).

get_all_neighbors(Id) ->
	gen_server:call(?MODULE, {get_all_neighbors, Id}).


%%
%% Internal
%%

make_state(ByIdDict, ByMapDict) ->
	#state{by_id = ByIdDict, by_map = ByMapDict}.


%% set new 
update_all_state(undefined, NewValue, State)
	when is_record(NewValue, info) ->

	#state{by_id = ByIdDict, by_map = ByMapDict} = State,
	Id = NewValue#info.id,
	MapId = NewValue#info.map,

	InfoList = get_info_list_by_map(MapId, State),
	NewInfoList = lists:keystore(Id, 2, InfoList, NewValue),

	NewByMapDict = dict:store(MapId, NewInfoList, ByMapDict),
	NewByIdDict = dict:store(Id, NewValue, ByIdDict),

	NewState = #state{by_id = NewByIdDict, by_map = NewByMapDict},
	NewState;

%% move in same map.
update_all_state(OldValue, NewValue, State)
	when is_record(OldValue, info),
		is_record(NewValue, info),
		OldValue#info.map == NewValue#info.map ->

	#state{by_id = ByIdDict, by_map = ByMapDict} = State,
	Id = NewValue#info.id,
	MapId = NewValue#info.map,

	InfoList = get_info_list_by_map(MapId, State),
	NewInfoList = lists:keystore(Id, 2, InfoList, NewValue),

	NewByMapDict = dict:store(MapId, NewInfoList, ByMapDict),
	NewByIdDict = dict:store(Id, NewValue, ByIdDict),

	NewState = #state{by_id = NewByIdDict, by_map = NewByMapDict},
	NewState;

%% move to another map.
update_all_state(OldValue, NewValue, State) ->

	#state{by_id = ByIdDict, by_map = ByMapDict} = State,
	Id = NewValue#info.id,

	NewMapId = NewValue#info.map,
	OldMapId = OldValue#info.map,


	NewInfoList1 = get_info_list_by_map(NewMapId, State),
	NewInfoList2 = lists:keystore(Id, 2, NewInfoList1, NewValue),

	OldInfoList1 = get_info_list_by_map(OldMapId, State),
	OldInfoList2 = lists:keydelete(Id, 2, OldInfoList1),

	ByMapDict1 = dict:store(NewMapId, NewInfoList2, ByMapDict),
	ByMapDict2 = dict:store(OldMapId, OldInfoList2, ByMapDict1),
	NewByIdDict = dict:store(Id, NewValue, ByIdDict),

	NewState = #state{by_id = NewByIdDict, by_map = ByMapDict2},
	NewState.

get_map_by_id(Id, State) ->
	case dict:find(Id, State#state.by_id) of
		{ok, Info} -> Info#info.map;
		error -> undefined
	end.

get_info_by_id(Id, State) ->
	case dict:find(Id, State#state.by_id) of
		{ok, Value} -> Value;
		error -> undefined
	end.

get_info_list_by_map(MapId, State) ->
	case dict:find(MapId, State#state.by_map) of
		{ok, Value} -> Value;
		error -> []
	end.
%%
%% Callbacks
%%
%% State holds 2 raw dicts.
%% by_id : key = id, value = one info record. 
%% by_map : key = map id, value = list of info. 
%% 
init(_Args) -> {ok, #state{by_id = dict:new(), by_map = dict:new()}}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call({get_all_neighbors, Id}, _From, State) ->
	MapId = get_map_by_id(Id, State),
	Result = get_info_list_by_map(MapId, State),
	{reply, {ok, Result}, State};

handle_call({get_info, Id}, _From, State) ->
	{reply, get_info_by_id(Id, State), State};

handle_call({set_info, Id, Value}, _From, State) when Value#info.id == Id ->
	OldInfo = get_info_by_id(Id, State), %% get old value
	NewState = update_all_state(OldInfo, Value, State), %% call recalc
	{reply, ok, NewState}.

handle_cast({delete, Id}, State) ->
	{noreply, State}.



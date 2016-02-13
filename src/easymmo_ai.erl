-module(easymmo_ai).
-include("easymmo_ai.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	test/0,
	lua_sample/0,

	blackboard_start/0,
	blackboard_set_info/2,
	blackboard_get_info/1,
	blackboard_delete_info/1,
	blackboard_get_all_neighbors/1,

	new/1,
	add/2,
	get_state/1,
	set_state/2

	]).

-ignore_xref([
	test/0,
	lua_sample/0,

	blackboard_start/0,
	blackboard_set_info/2,
	blackboard_get_info/1,
	blackboard_delete_info/1,
	blackboard_get_all_neighbors/1,

	new/1,
	add/2,
	get_state/1,
	set_state/2

	]).

-record(info, {id, map, x, y}).
%%-record(state, {by_id, by_map}).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional

test() ->
	blackboard_start(),
	Info1_1_1 = #info{id=1, map=m1, x=101, y=101},
	Info1_1_2 = #info{id=1, map=m1, x=102, y=102},
	Info1_2_1 = #info{id=1, map=m2, x=101, y=101},
	Info2_1_1 = #info{id=2, map=m1, x=101, y=101},
	Info2_2_2 = #info{id=2, map=m2, x=102, y=101},
	blackboard_set_info(1, Info1_1_1),
	io:format("1_1_1 ~p~n", [blackboard_get_all_neighbors(1)]),
	blackboard_set_info(1, Info1_1_2),
	io:format("1_1_2 ~p~n", [blackboard_get_all_neighbors(1)]),
	blackboard_set_info(2, Info2_1_1),
	io:format("1 and 2 ~p~n", [blackboard_get_all_neighbors(1)]),
	blackboard_set_info(2, Info2_2_2),
	io:format("2_2_2 ~p~n", [blackboard_get_info(2)]),
	blackboard_set_info(1, Info1_2_1),
	io:format("1 and 2 ~p~n", [blackboard_get_all_neighbors(1)]),
	blackboard_delete_info(1),
	io:format("not exist 1 in world ~p~n", [blackboard_get_all_neighbors(1)]),
	io:format("2_2_2 ~p~n", [blackboard_get_all_neighbors(2)]) .

lua_sample() ->
	% separately parse, then execute
	State0 = luerl:init(),
	State1 = luerl:load_module([ei], luerl_easymmo_ai_base, State0),
	{ok, Chunk, State2} = luerl:loadfile(
		filename:join([code:priv_dir(easymmo_ai), "lua", "sample.lua"]),
		State1),
	{Ret, _State3} = luerl:do(Chunk, State2),
	Ret.


%% @doc Pings a random vnode to make sure communication is functional

blackboard_start() ->
    DocIdx = riak_core_util:chash_key({<<"system">>, <<"blackboard">>}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, blackboard_start, easymmo_ai_vnode_master).

blackboard_get_info(Id) ->
    DocIdx = riak_core_util:chash_key({<<"system">>, <<"blackboard">>}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {blackboard_get_info, Id}, easymmo_ai_vnode_master).

blackboard_set_info(Id, Value) ->
    DocIdx = riak_core_util:chash_key({<<"system">>, <<"blackboard">>}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {blackboard_set_info, Id, Value}, easymmo_ai_vnode_master).

blackboard_delete_info(Id) ->
    DocIdx = riak_core_util:chash_key({<<"system">>, <<"blackboard">>}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {blackboard_delete_info, Id}, easymmo_ai_vnode_master).

blackboard_get_all_neighbors(Id) ->
    DocIdx = riak_core_util:chash_key({<<"system">>, <<"blackboard">>}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {blackboard_get_all_neighbors, Id}, easymmo_ai_vnode_master).

new(Name) ->
    DocIdx = riak_core_util:chash_key({<<"easymmo_ai">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {new, Name}, easymmo_ai_vnode_master).

add(Name, N) ->
    DocIdx = riak_core_util:chash_key({<<"easymmo_ai">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {add, Name, N}, easymmo_ai_vnode_master).

get_state(Name) ->
    DocIdx = riak_core_util:chash_key({<<"easymmo_ai">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {get_state, Name}, easymmo_ai_vnode_master).

set_state(Name, NewWorkerState) ->
    DocIdx = riak_core_util:chash_key({<<"easymmo_ai">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {set_state, Name, NewWorkerState}, easymmo_ai_vnode_master).


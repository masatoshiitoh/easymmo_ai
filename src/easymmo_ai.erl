-module(easymmo_ai).
-include("easymmo_ai.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	run/0,
	new_instance/1,
	register_script/2,
	action/2,
	get_current/0,

	ping/0,
	lock_addnew/2,
	lock_button/2,
	get_state/1,
	lookup/1
	]).

-ignore_xref([
	run/0,
	new_instance/1,
	register_script/2,
	action/2,
	get_current/0,

	ping/0,
	lock_addnew/2,
	lock_button/2,
	get_state/1,
	lookup/1
	]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional


run() ->
	% separately parse, then execute
	State0 = luerl:init(),
	State1 = luerl:load_module([easymmo_ai], luerl_easymmo_ai_base, State0),
	{ok, Chunk, State2} = luerl:loadfile(
		filename:join([code:priv_dir(easymmo_ai), "lua", "simple.lua"]),
		State1),
	{Ret, State3} = luerl:do(Chunk, State2),
	Ret.



%% add new instance.
%% returns name
new_instance(Type) ->
	Script = get_script(Type),
	Name = gen_name(Type),
	start_new_instance(Name, Script),
	Name.

get_script(Type) ->
	"print(\"hello\")".

gen_name(Type) ->
	"name01".

start_new_instance(Name, Script) ->
	0.

%% add or update lua script with ai type name.
%% returns ok or ng
%% use luerl:load
register_script(Type, Script) ->ng.

%% send event to instance (event is processed by Lua script. easymmo_ai doesn't care event content.)
%% returns ok or ng
action(Name, Action) ->ng.

%% receive new event from outflow tank.
%% returns list of instances
get_current() ->[].

%% receive new event from outflow tank.
%% returns ok or ng
start_watch(Dest) ->ng.
stop_watch(Dest) ->ng.

%% prepare blackboard
start_blackboard(BBName) -> ng.
clear_blackboard(BBName) -> ng.
get_blackboard(BBName) -> ng.

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, easymmo_ai_vnode_master).

get_state(Name) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {get_state, Name}, easymmo_ai_vnode_master).

lookup(Name) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {lookup, Name}, easymmo_ai_vnode_master).

lock_button(Name, Button) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, easymmo_ai),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {button, Name, Button}, easymmo_ai_vnode_master).

lock_addnew(Name, Code) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, easymmo_ai),
	riak_core_vnode_master:command(PrefList, {addnew, Name, Code}, easymmo_ai_vnode_master).


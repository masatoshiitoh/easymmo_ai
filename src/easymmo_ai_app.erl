-module(easymmo_ai_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case easymmo_ai_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, easymmo_ai_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(easymmo_ai_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(easymmo_ai_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(easymmo_ai, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

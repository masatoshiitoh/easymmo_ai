-module(tiny_maxwell).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).



%%
%% Public APIs
%%

get_info(Id, Default) ->
	gen_server:call(?MODULE, {get_info, Id, Default}) .

set_info(Id, NewLocation) ->
	gen_server:call(?MODULE, {set_info, Id, NewLocation}).

delete_info(Id) ->
	gen_server:call(?MODULE, {delete_info, Id}).

get_all_neighbors(Id) ->
	gen_server:call(?MODULE, {get_all_neighbors, Id}).

recalc_all_distances() ->
	gen_server:cast(?MODULE, recalc_all_distances).

%%
%% Internal
%%
%% State holds raw dict.
%%
init(_Args) -> {ok, dict:new()}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call({get_all_neighbors, Id}, _From, State) ->
	{reply, ok, State};

handle_call({get_info, Id, Default}, _From, State) ->
	Result = case dict:find(Id, State) of
		{ok, Value} -> Value;
		error -> Default
	end,
	{reply, Result, State};

handle_call({set_info, Id, Value}, _From, State) ->
	NewState = dict:update(Id, Value, State),
	{reply, ok, NewState}.

handle_cast({delete, Id}, State) ->
	{noreply, State};

handle_cast(recalc_all_distances, State) ->
	{noreply, State}.


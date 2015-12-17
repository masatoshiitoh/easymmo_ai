-module(easymmo_ai_persona).
-behaviour(gen_fsm).

-export([run/0]).

-export([stop/1]).
-export([button/2, get_state/1]).
-export([locked/2, open/2]).

-export([start_link/1]).
-export([init/1, terminate/3, code_change/4]).
-export([handle_event/3, handle_info/3, handle_sync_event/4]).

run() ->
	% execute a string
	luerl:do("print(\"Hello, Robert(o)!\")"),

	% execute a file
	%luerl:dofile("./hello.lua"),

	% separately parse, then execute
	State0 = luerl:init(),
	{ok, Chunk, State1} = luerl:load("print(\"Hello, Chunk!\")", State0),
	{_Ret, _NewState} = luerl:do(Chunk, State1),

	done.





stop(Pid) -> gen_fsm:send_all_state_event(Pid, stop).

button(Pid, String) -> gen_fsm:send_event(Pid, {button, String}).

get_state(Pid) -> gen_fsm:sync_send_all_state_event(Pid, get_state).



%% "button" event to "locked" state.
locked({button, String}, {SoFar, Code}) ->
    case SoFar ++ String of
        Code ->
			%% stacked button strings equals Code => change to "open" state (timeout 3 seconds)
            io:fwrite("open!!~n"),
            {next_state, open, {[], Code}, 3000};
        Incomplete when length(Incomplete)<length(Code) ->
			%% entering buttons => keep "locked" state with history(=Incomplete)
            {next_state, locked, {Incomplete, Code}};
        _Wrong ->
			%% enough length, but wrong => change to "locked" state, with empty history.
            {next_state, locked, {[], Code}}
    end.


%% "timeout" event to "open" state
open(timeout, StateData) ->
    io:fwrite("close!!~n"),
	%% timeout on "open" => change to "locked"
    {next_state, locked, StateData}.









start_link(Type) ->
	gen_fsm:start_link(easymmo_ai_persona, Type, []).

%% initialize FSM as "locked" state with Code
init(Code) -> {ok, locked, {[], Code}}.
%init(Type) -> {ok, loaded, {[], Type}}.

terminate(shutdown, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

handle_event(stop, _StateName, StateData) ->
	{stop, normal, StateData}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.


-module(luerl_easymmo_ai_base).

-export ([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).   %Shorten this

install(St) ->
    luerl_emul:alloc_table(table(), St).

table() -> [
	{<<"bar">>,{function,fun bar/2}},
	{<<"send_state">>, {function, fun get_all/2}},
	{<<"get_all">>, {function, fun get_all/2}},
	{<<"foo">>,{function,fun foo/2}}
].

send_state(StateName, St) ->
	{[0], St}.

get_all(A,St) ->
	{[0], St}.

bar(A,St) -> {[0], St}.
foo([A,B],St) -> {[<<"foo">>,A,B], St}.

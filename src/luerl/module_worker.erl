-module(module_worker).

-export ([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).   %Shorten this

install(St) ->
	luerl_emul:alloc_table(table(), St).

table() -> [
	{<<"get_all">>, {function, fun get_all/2}},
	{<<"ping">>,{function,fun ping/1}}].

get_all(A,St) -> {[0], St}.
ping(St) -> {[<<"ok">>], St}.

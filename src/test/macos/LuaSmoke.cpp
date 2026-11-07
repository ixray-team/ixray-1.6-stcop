extern "C"
{
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"
}
#include <cstdio>
#include <memory>

int main(int ArgumentCount, char** Arguments)
{
	std::unique_ptr<lua_State, decltype(&lua_close)> State(luaL_newstate(), lua_close);
	if (!State)
	{
		return 1;
	}
	luaL_openlibs(State.get());
	const char Source[] = "local ok, message = pcall(function() error('expected probe error') end); "
						  "assert(not ok and message:find('expected probe error')); "
						  "local total = 0; for i = 1, 10000 do total = total + i end; return total";
	if (luaL_dostring(State.get(), Source) != 0 || lua_tonumber(State.get(), -1) != 50005000)
	{
		fprintf(stderr, "FAIL: Lua protected errors or hot loop: %s\n", lua_tostring(State.get(), -1));
		return 2;
	}
	if (ArgumentCount > 1)
	{
		lua_createtable(State.get(), ArgumentCount - 1, 0);
		for (int Index = 1; Index < ArgumentCount; ++Index)
		{
			lua_pushstring(State.get(), Arguments[Index]);
			lua_rawseti(State.get(), -2, Index - 1);
		}
		lua_setglobal(State.get(), "arg");
		if (luaL_dofile(State.get(), Arguments[1]) != 0)
		{
			fprintf(stderr, "FAIL: Lua regression: %s\n", lua_tostring(State.get(), -1));
			return 3;
		}
	}
	puts("PASS: native Lua protected errors, hot loop and state cleanup");
	return 0;
}


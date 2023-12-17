#include "stdafx.h"

extern "C"
{
#include <lua/lua.h>
#include "lfs.h"

	int luaopen_marshal(lua_State* L);
}

void lua_init_ext(lua_State* L)
{
	luaopen_marshal(L);
	luaopen_lfs(L);
}
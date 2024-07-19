#pragma once

#if defined(XR_SCRIPTS_EXPORTS) || defined(XRSE_FACTORY_EXPORTS)
// initialize lua standard library functions 
struct luajit 
{
	static void open_lib(lua_State* L, pcstr module_name, lua_CFunction function)
	{
		lua_pushcfunction(L, function);
		lua_pushstring(L, module_name);
		lua_call(L, 1, 0);
	}
};

void lua_init_ext(lua_State* L);
#endif

SCRIPTS_API void DebbugerAttach();
SCRIPTS_API extern bool IsLDBGAttached;
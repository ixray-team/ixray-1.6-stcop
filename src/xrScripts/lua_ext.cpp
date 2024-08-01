#include "stdafx.h"
#include "script_engine.h"

#include "luapanda.h"

extern "C"
{
#include <lua/lua.h>
#include "lfs.h"
#include "luasocket/luasocket.h"

	int luaopen_marshal(lua_State* L);
	int luaopen_LuaXML_lib(lua_State* L);
	int luaopen_utf8(lua_State* L);
}

#include "lua_ext.h"

static bool LoadScriptToGlobal(lua_State* L, const char* name)
{
	string_path FileName;
	xr_string FixedFileName = "ixray_system\\" + xr_string(name);

	if (FS.exist(FileName, "$game_scripts$", FixedFileName.data()))
	{
		int	start = lua_gettop(L);
		IReader* l_tpFileReader = FS.r_open(FileName);

		string_path NameSpace;
		xr_strcpy(NameSpace, name);

		if (strext(NameSpace))
			*strext(NameSpace) = 0;

		if (luaL_loadbuffer(L, (const char*)l_tpFileReader->pointer(), l_tpFileReader->length(), NameSpace))
		{
			lua_settop(L, start);
			return false;
		}
		else
		{
			int errFuncId = -1;
			int	l_iErrorCode = lua_pcall(L, 0, 0, (-1 == errFuncId) ? 0 : errFuncId);
			if (l_iErrorCode)
			{
#ifdef DEBUG
				g_pScriptEngine->print_output(L, name, l_iErrorCode);
#endif
				lua_settop(L, start);
				return false;
			}
		}


		FS.r_close(l_tpFileReader);
	}
	else
	{
		return false;
	}

	return true;
};

void lua_init_ext(lua_State* L)
{
	luaopen_marshal(L);
	luaopen_lfs(L);
	luaopen_LuaXML_lib(L);
	luaopen_utf8(L);


	LoadScriptToGlobal(L, "global.lua");
	LoadScriptToGlobal(L, "dynamic_callbacks.lua");

	// Sockets
	luajit::open_lib(L, "socket.core", luaopen_socket_core);
	bool SocketTest = LoadScriptToGlobal(L, "socket.lua");

	// Panda
	if (SocketTest)
	{
		pdebug_init(L);
		LoadScriptToGlobal(L, "LuaPanda.lua");
	}
}

SCRIPTS_API bool IsLDBGAttached = false;
SCRIPTS_API void DebbugerAttach()
{
	const char* S = "debugger_attach()";
	shared_str m_script_name = "console command";
	int l_iErrorCode = luaL_loadbuffer(g_pScriptEngine->lua(), S, xr_strlen(S), "@console_command");

	if (!l_iErrorCode)
	{
		l_iErrorCode = lua_pcall(g_pScriptEngine->lua(), 0, 0, 0);
		if (l_iErrorCode)
		{
			g_pScriptEngine->print_output(g_pScriptEngine->lua(), *m_script_name, l_iErrorCode);
			return;
		}
	}

	g_pScriptEngine->print_output(g_pScriptEngine->lua(), *m_script_name, l_iErrorCode);
	IsLDBGAttached = true;
}

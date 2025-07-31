#include "stdafx.h"
#include "PluginLua.h"

#include "../../../xrScripts/linker.h"
#include "../../../xrScripts/script_engine.h"

#include <fstream>
#include <lua.h>

CPluginLua::CPluginLua()
{
	Type = EPluginType::Lua;
}

void CPluginLua::Run()
{
	lua_State* L = g_pScriptEngine->lua();

	if (luaL_dofile(L, Path.c_str()))
	{
		Msg("Error loading plugin: %s, code: %s", Name.c_str(), lua_tostring(L, -1));
		return;
	}

	lua_getglobal(L, "main");

	if (!lua_isfunction(L, -1))
	{
		Msg("Error loading entry function 'main' in %s", Name.c_str());
		return;
	}

	lua_pushstring(L, Scene->full_name.c_str());
	lua_pcall(L, 1, 1, 0);

	lua_pop(L, 1);
}

xr_string CPluginLua::ReadDesc() const
{
	std::ifstream file(Path.data());

	xr_string line;
	const std::string_view prefix = "-- desc: ";

	while (std::getline(file, line))
	{
		if (line.rfind(prefix, 0) == 0)
		{
			size_t pos = line.find(':');
			if (pos != std::string::npos && pos + 1 < line.size())
			{
				return line.substr(pos + 1);
			}
		}
	}

	return "Not found description!";
}
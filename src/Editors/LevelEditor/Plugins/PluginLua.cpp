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

	size_t ExtPos = Scene->full_name.find(".level");

	if (IsSimple() && ExtPos == xr_string::npos)
		return;

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

	if (IsSimple())
	{
		xr_string LevelDir = Scene->full_name.substr(0, ExtPos) + "\\";
		lua_pushstring(L, LevelDir.c_str());

		lua_pcall(L, 1, 1, 0);
	}
	else
	{
		for (auto& [Arg, Value] : InputArgsValues)
		{
			if (Arg == "level")
			{
				xr_string LevelDir = Scene->full_name.substr(0, ExtPos) + "\\";
				lua_pushstring(L, LevelDir.c_str());
			}
			else
			{
				lua_pushstring(L, Value);
			}
		}

		if (lua_pcall(L, InputArgsValues.size(), 1, 0))
		{
			Msg("! %s", lua_tostring(L, -1));
		}
	}

	lua_pop(L, 1);
}

xr_string CPluginLua::ReadDesc() const
{
	std::ifstream file(Path.data());

	xr_string line;
	xr_string Result = "Not found description!";

	const std::string_view prefix = "-- desc: ";
	const std::string_view prefix_input = "-- input: ";

	while (std::getline(file, line))
	{
		if (line.rfind(prefix, 0) == 0)
		{
			size_t pos = line.find(':');
			if (pos != std::string::npos && pos + 1 < line.size())
			{
				Result = line.substr(pos + 1);
			}
		}
		else if (line.rfind(prefix_input, 0) == 0)
		{
			std::regex pattern(R"(\[([^,]+), ([^\]]+)\])");

			std::smatch match;
			xr_string::const_iterator searchStart(line.cbegin());

			while (std::regex_search(searchStart, line.cend(), match, pattern))
			{
				InputArgsName[match[1].str().c_str()] = match[2].str().c_str();
				searchStart = match.suffix().first;
			}
		}
	}

	return Result;
}
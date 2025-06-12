#include "stdafx.h"
#include <random>
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

/******************** STRING ********************/
enum TrimDirection { TRIM_BOTH, TRIM_LEFT, TRIM_RIGHT };

int str_trim_common(lua_State* L, TrimDirection dir)
{
    size_t size;
    const char* front = luaL_checklstring(L, 1, &size);
    const char* end = &front[size - 1];

    if (dir == TRIM_BOTH || dir == TRIM_LEFT)
        while (size && std::isspace(*front)) ++front, --size;

    if (dir == TRIM_BOTH || dir == TRIM_RIGHT)
        while (size && std::isspace(*end)) --end, --size;

    lua_pushlstring(L, front, (size_t)(end - front) + 1);
    return 1;
}

int str_trim(lua_State* L) { return str_trim_common(L, TRIM_BOTH); }
int str_trim_l(lua_State* L) { return str_trim_common(L, TRIM_LEFT); }
int str_trim_r(lua_State* L) { return str_trim_common(L, TRIM_RIGHT); }

int str_trim_w(lua_State* L)
{
    const char* str = luaL_checkstring(L, 1);
    const char* start = str;
    const char* end = str;

    // Пропускаем начальные пробелы
    while (*start == ' ') ++start;

    // Пропускаем слово
    end = start;
    while (*end && *end != ' ') ++end;

    lua_pushlstring(L, start, end - start);
    return 1;
}

const luaL_Reg strlib[] = {
    {"trim", str_trim}, {"trim_l", str_trim_l}, {"trim_r", str_trim_r}, {"trim_w", str_trim_w}, {NULL, NULL}
};

int open_string(lua_State* L)
{
    luaL_openlib(L, LUA_STRLIBNAME, strlib, 0);
    return 0;
}

/******************** MATH ********************/
std::mt19937 intgen(std::random_device{}());

int gen_random_in_range(int a1, int a2)
{
    std::uniform_int_distribution<int> dist(a1, a2);
    return dist(intgen);
}

/******************** TABLE ********************/
inline int C_get_size(lua_State* L)
{
    int size = 0;
    lua_settop(L, 2); // Устанавливаем стек для доступа к таблице
    lua_pushnil(L);
    while (lua_next(L, 1))
    {
        ++size;
        lua_pop(L, 1);
    }
    return size;
}

int tab_keys(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_newtable(L);

    int index = 1;
    lua_pushnil(L);
    while (lua_next(L, 1))
    {
        lua_pushinteger(L, index++);
        lua_pushvalue(L, -2);
        lua_settable(L, -4);
        lua_pop(L, 1);
    }
    return 1;
}

int tab_values(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_newtable(L);

    int index = 1;
    lua_pushnil(L);
    while (lua_next(L, 1))
    {
        lua_pushinteger(L, index++);
        lua_pushvalue(L, -1);
        lua_settable(L, -4);
        lua_pop(L, 1);
    }
    return 1;
}

int get_size(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_pushinteger(L, C_get_size(L));
    return 1;
}

int get_random(lua_State* L)
{
    int size = C_get_size(L);
    if (size == 0) return 0;

    int random_index = gen_random_in_range(1, size);

    luaL_checktype(L, 1, LUA_TTABLE);
    lua_pushnil(L);

    int current_index = 0;
    while (lua_next(L, 1))
    {
        if (++current_index == random_index)
        {
            lua_pushvalue(L, -2); // Ключ
            lua_pushvalue(L, -1); // Значение
            return 2;
        }
        lua_pop(L, 1);
    }
    return 0;
}

const luaL_Reg tab_funcs[] = {
    {"keys", tab_keys}, {"values", tab_values}, {"size", get_size}, {"random", get_random}, {NULL, NULL}
};

int open_table(lua_State* L)
{
    luaL_openlib(L, LUA_TABLIBNAME, tab_funcs, 0);
    return 0;
}

bool LoadScriptToGlobal(lua_State* L, const char* name, bool KernelScript)
{
	string_path FileName;
	xr_string FixedFileName = name;

	if (KernelScript)
	{
		FixedFileName = "ixray_system\\" + FixedFileName;
	}

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
    open_string(L);
    open_table(L);
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

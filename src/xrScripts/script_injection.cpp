#include "pch_script.h"
#include "script_injection.h"
#include "script_injection_runtime.h"
#include <luabind/detail/class_rep.hpp>

namespace ScriptInjection
{
static char RegistryKey;

static int ReplaceReferences(lua_State* L)
{
	luaL_checktype(L, 1, LUA_TFUNCTION);
	luaL_checktype(L, 2, LUA_TFUNCTION);
	lua_replacefunction(L, 1, 2);
	lua_pushvalue(L, 2);
	return 1;
}

static int ClassTable(lua_State* L)
{
	// Do not reinterpret instance userdata as class_rep.
	if (lua_type(L, 1) != LUA_TUSERDATA || !lua_getmetatable(L, 1))
	{
		return 0;
	}
	lua_pushliteral(L, "__luabind_classrep");
	lua_rawget(L, -2);
	const bool IsClass = lua_toboolean(L, -1) != 0;
	lua_pop(L, 2);
	if (!IsClass)
	{
		return 0;
	}
	auto* Class = static_cast<luabind::detail::class_rep*>(lua_touserdata(L, 1));
	if (Class->get_class_type() != luabind::detail::class_rep::lua_class)
	{
		return 0;
	}
	Class->get_table(L);
	return 1;
}

bool Initialize(lua_State* L)
{
	if (luaL_loadbuffer(L, Runtime, sizeof(Runtime) - 1, "@engine/script_injection") != 0)
	{
		return false;
	}
	lua_pushcfunction(L, ClassTable);
	lua_pushcfunction(L, ReplaceReferences);
	if (lua_pcall(L, 2, 1, 0) != 0)
	{
		return false;
	}
	const char* Exports[] = {
		"override_or_create_method",
		"override_or_create_local_variable",
		"get_local_variables",
		"get_local_functions"
	};
	for (const char* Name : Exports)
	{
		lua_getfield(L, -1, Name);
		lua_setfield(L, LUA_GLOBALSINDEX, Name);
	}
	lua_pushlightuserdata(L, &RegistryKey);
	lua_insert(L, -2);
	lua_rawset(L, LUA_REGISTRYINDEX);
	return true;
}

int LoadNamespace(lua_State* L, const char* Header, const char* Source, size_t Size, const char* Name)
{
	lua_pushlightuserdata(L, &RegistryKey);
	lua_rawget(L, LUA_REGISTRYINDEX);
	if (!lua_istable(L, -1))
	{
		lua_pop(L, 1);
		lua_pushliteral(L, "script injection runtime is not initialized");
		return LUA_ERRRUN;
	}
	const int RuntimeIndex = lua_gettop(L);
	lua_getfield(L, RuntimeIndex, "Prepare");
	lua_pushstring(L, Header);
	lua_pushlstring(L, Source, Size);
	int Status = lua_pcall(L, 2, 1, 0);
	if (Status == 0)
	{
		size_t Length;
		const char* Prepared = lua_tolstring(L, -1, &Length);
		Status = luaL_loadbuffer(L, Prepared, Length, Name);
		lua_remove(L, -2);
		if (Status == 0)
		{
			lua_getfield(L, RuntimeIndex, "Begin");
			Status = lua_pcall(L, 1, 1, 0);
		}
	}
	lua_remove(L, RuntimeIndex);
	return Status;
}
} // namespace ScriptInjection

// Some extensions to Lua 5.1

#ifndef LUA_EXTENSIONS_HPP_INCLUDED
#define LUA_EXTENSIONS_HPP_INCLUDED

#include <luabind/class_info.hpp>

// Port of Lua5.2 luaL_tolstring()
// Based on https://github.com/keplerproject/lua-compat-5.2
// Converts any Lua value at the given index to a C string in a reasonable format.
// The resulting string is pushed onto the stack and also returned by the function.
// If len is not NULL, the function also sets *len with the string length.
// TODO: Does NOT support __tostring metamethod.

const char* lua52L_tolstring(lua_State* L, int idx, size_t* len) {
	int t = lua_type(L, idx);

	// if (!luaL_callmeta(L, idx, "__tostring")) {	//TODO: currently crash, re-implement in future.
	if (true) {
		switch (t) {
		case LUA_TNIL:
			lua_pushliteral(L, "nil");
			break;
		case LUA_TSTRING:
			lua_pushliteral(L, "'");
			lua_pushvalue(L, idx);
			lua_pushliteral(L, "'");
			break;
		case LUA_TNUMBER:
			lua_pushvalue(L, idx);
			break;
		case LUA_TBOOLEAN:
			if (lua_toboolean(L, idx))
				lua_pushliteral(L, "true");
			else
				lua_pushliteral(L, "false");
			break;
		case LUA_TUSERDATA:
			{
				// Print luabind::object class name
				luabind::argument pLuaArgument = luabind::argument(luabind::from_stack(L, idx));
				lua_pushstring(L, luabind::get_class_info(pLuaArgument).name.c_str());
				lua_pushliteral(L, "&");
			}
			break;
		default:
			lua_pushfstring(L, "%s: %p", lua_typename(L, t),
				lua_topointer(L, idx));
			break;
		}
	}
	return lua_tolstring(L, -1, len);
}

#endif


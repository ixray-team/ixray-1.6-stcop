#pragma once

#include <stddef.h>

struct lua_State;

namespace ScriptInjection
{
// Leaves an error on the stack on failure; otherwise preserves the stack.
bool Initialize(lua_State* L);
// Like luaL_loadbuffer: leaves either the executable chunk or an error.
int LoadNamespace(lua_State* L, const char* Header, const char* Source, size_t Size, const char* Name);
} // namespace ScriptInjection

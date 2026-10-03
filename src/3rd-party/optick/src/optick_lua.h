#pragma once

// Optick Lua integration, API compatible with Tracy's TracyLua.hpp.
// Include this file after the Lua headers.
//
// In Lua code:
//   optick.ZoneBegin()            -- begin a zone for the current function
//   optick.ZoneBeginN(name)       -- begin a zone with a custom name
//   optick.ZoneBeginS(depth)      -- same as ZoneBegin (Lua callstacks are not captured)
//   optick.ZoneBeginNS(name,depth)-- same as ZoneBeginN
//   optick.ZoneEnd()              -- end the current zone
//   optick.Message(text)          -- emit a profiler message
//
// Optick::LuaHook can be chained into an existing lua_sethook callback to
// profile every Lua call/return automatically.
// Optick::LuaRemove strips optick.* / tracy.* calls from a script buffer.

#include "optick.h"

#if USE_OPTICK

#include <stdint.h>
#include <string.h>
#include <string>
#include <unordered_map>
#include <mutex>

namespace Optick
{
	namespace LuaDetail
	{
		// Number of zones pushed by Lua on the current thread that were not popped yet.
		// Keeps Optick's per-thread push/pop stack balanced even if a capture stops mid-zone.
		inline int& ZoneDepth()
		{
			static thread_local int depth = 0;
			return depth;
		}

		// Lua zone names are already fully formatted (file, lines, function), so they are
		// registered with IS_CUSTOM_NAME - otherwise the GUI strips everything from the
		// first bracket as if it were a C++ function argument list.
		inline EventDescription* GetDescription(const char* name, const char* source, unsigned long line)
		{
			static std::unordered_map<std::string, EventDescription*> cache;
			static std::mutex lock;

			std::string key = name != nullptr ? name : "Lua";

			std::lock_guard<std::mutex> guard(lock);
			std::unordered_map<std::string, EventDescription*>::iterator it = cache.find(key);
			if (it != cache.end())
				return it->second;

			EventDescription* description = EventDescription::Create(
				key.c_str(), source, line,
				Color::Null, 0,
				(uint8_t)(EventDescription::IS_CUSTOM_NAME | EventDescription::COPY_NAME_STRING));

			cache.insert(std::make_pair(key, description));
			return description;
		}

		inline void BeginZone(lua_State* L, const char* customName)
		{
			if (!IsActive())
				return;

			lua_Debug dbg;
			const char* name = customName;
			const char* source = nullptr;
			unsigned long line = 0;

			if (lua_getstack(L, 1, &dbg) != 0 && lua_getinfo(L, "Snl", &dbg) != 0)
			{
				source = dbg.source ? dbg.source : dbg.short_src;
				line = (unsigned long)dbg.currentline;
				if (name == nullptr)
					name = dbg.name ? dbg.name : dbg.short_src;
			}

			EventDescription* description = GetDescription(name, source, line);
			Event::Push(*description);
			++ZoneDepth();
		}

		inline void BeginZoneFromHook(lua_Debug* ar)
		{
			if (ar == nullptr || !IsActive())
				return;

			const char* name = ar->name ? ar->name : ar->short_src;
			const char* source = ar->source ? ar->source : ar->short_src;
			EventDescription* description = GetDescription(name, source, (unsigned long)ar->currentline);
			Event::Push(*description);
			++ZoneDepth();
		}

		inline void EndZone()
		{
			if (ZoneDepth() > 0)
			{
				Event::Pop();
				--ZoneDepth();
			}
		}

		inline int CZoneBegin(lua_State* L) { BeginZone(L, nullptr); return 0; }
		inline int CZoneBeginN(lua_State* L) { BeginZone(L, lua_tostring(L, 1)); return 0; }
		inline int CZoneEnd(lua_State*) { EndZone(); return 0; }
		inline int CZoneText(lua_State*) { return 0; }
		inline int CZoneName(lua_State*) { return 0; }
		inline int CMessage(lua_State* L) { OPTICK_UNUSED(L); return 0; }
		inline int CSectionEnter(lua_State* L) { lua_pushinteger(L, 0); return 1; }
		inline int CSectionLeave(lua_State*) { return 0; }

		inline char* FindEnd(char* ptr)
		{
			unsigned int cnt = 1;
			while (cnt != 0)
			{
				if (*ptr == '(') cnt++;
				else if (*ptr == ')') cnt--;
				ptr++;
			}
			return ptr;
		}
	}

	inline void LuaRegister(lua_State* L)
	{
		lua_newtable(L);
		lua_pushcfunction(L, LuaDetail::CZoneBegin);    lua_setfield(L, -2, "ZoneBegin");
		lua_pushcfunction(L, LuaDetail::CZoneBeginN);   lua_setfield(L, -2, "ZoneBeginN");
		lua_pushcfunction(L, LuaDetail::CZoneBegin);    lua_setfield(L, -2, "ZoneBeginS");
		lua_pushcfunction(L, LuaDetail::CZoneBeginN);   lua_setfield(L, -2, "ZoneBeginNS");
		lua_pushcfunction(L, LuaDetail::CZoneEnd);      lua_setfield(L, -2, "ZoneEnd");
		lua_pushcfunction(L, LuaDetail::CZoneText);     lua_setfield(L, -2, "ZoneText");
		lua_pushcfunction(L, LuaDetail::CZoneName);     lua_setfield(L, -2, "ZoneName");
		lua_pushcfunction(L, LuaDetail::CMessage);      lua_setfield(L, -2, "Message");
		lua_pushcfunction(L, LuaDetail::CSectionEnter); lua_setfield(L, -2, "SectionEnter");
		lua_pushcfunction(L, LuaDetail::CSectionLeave); lua_setfield(L, -2, "SectionLeave");
		lua_setglobal(L, "optick");

		// Compatibility alias so that scripts written for Tracy keep working.
		lua_getglobal(L, "tracy");
		bool hasTracy = !lua_isnil(L, -1);
		lua_pop(L, 1);
		if (!hasTracy)
		{
			lua_getglobal(L, "optick");
			lua_setglobal(L, "tracy");
		}
	}

	inline void LuaHook(lua_State* L, lua_Debug* ar)
	{
		OPTICK_UNUSED(L);

		if (ar == nullptr)
			return;

		if (ar->event == LUA_HOOKCALL)
		{
			LuaDetail::BeginZoneFromHook(ar);
		}
		else if (ar->event == LUA_HOOKRET
#ifdef LUA_HOOKTAILRET
			|| ar->event == LUA_HOOKTAILRET
#endif
			)
		{
			LuaDetail::EndZone();
		}
	}

	inline void LuaRemove(char* script)
	{
		if (script == nullptr)
			return;

		while (*script)
		{
			size_t prefixLength = 0;
			if (strncmp(script, "optick.", 7) == 0) prefixLength = 7;
			else if (strncmp(script, "tracy.", 6) == 0) prefixLength = 6;

			if (prefixLength == 0)
			{
				script++;
				continue;
			}

			char* call = script + prefixLength;

			if (strncmp(call, "Zone", 4) == 0)
			{
				char* name = call + 4;
				if (strncmp(name, "End()", 5) == 0)
				{
					size_t length = prefixLength + 9;
					memset(script, ' ', length);
					script += length;
				}
				else if (strncmp(name, "Begin()", 7) == 0)
				{
					size_t length = prefixLength + 11;
					memset(script, ' ', length);
					script += length;
				}
				else if (strncmp(name, "Text(", 5) == 0)
				{
					char* end = LuaDetail::FindEnd(name + 5);
					memset(script, ' ', end - script);
					script = end;
				}
				else if (strncmp(name, "Name(", 5) == 0)
				{
					char* end = LuaDetail::FindEnd(name + 5);
					memset(script, ' ', end - script);
					script = end;
				}
				else if (strncmp(name, "BeginN(", 7) == 0)
				{
					char* end = LuaDetail::FindEnd(name + 7);
					memset(script, ' ', end - script);
					script = end;
				}
				else if (strncmp(name, "BeginS(", 7) == 0)
				{
					char* end = LuaDetail::FindEnd(name + 7);
					memset(script, ' ', end - script);
					script = end;
				}
				else if (strncmp(name, "BeginNS(", 8) == 0)
				{
					char* end = LuaDetail::FindEnd(name + 8);
					memset(script, ' ', end - script);
					script = end;
				}
				else
				{
					script += prefixLength + 4;
				}
			}
			else if (strncmp(call, "Message(", 8) == 0)
			{
				char* end = LuaDetail::FindEnd(call + 8);
				memset(script, ' ', end - script);
				script = end;
			}
			else if (strncmp(call, "SectionEnter(", 13) == 0)
			{
				char* end = LuaDetail::FindEnd(call + 13);
				*script = '0';
				memset(script + 1, ' ', end - script - 1);
				script = end;
			}
			else if (strncmp(call, "SectionLeave(", 13) == 0)
			{
				char* end = LuaDetail::FindEnd(call + 13);
				memset(script, ' ', end - script);
				script = end;
			}
			else
			{
				script += prefixLength;
			}
		}
	}
}

#else

namespace Optick
{
	inline void LuaRegister(lua_State*) {}
	inline void LuaHook(lua_State*, lua_Debug*) {}
	inline void LuaRemove(char*) {}
}

#endif // USE_OPTICK

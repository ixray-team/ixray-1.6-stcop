#include "StdAfx.h"
#include "WristwatchSurgeLuaBridge.h"

#include "pch_script.h"
#include "../ai_space.h"
#include "../Level.h"
#include "../../xrEngine/WristwatchSettings.h"
#include "../../xrScripts/script_engine.h"

#include <cmath>
#include <functional>

namespace WristwatchSurgeLuaBridge
{
namespace
{
bool s_loggedSurgeUnavailable = false;

bool IsLevelScriptsReady()
{
	luabind::functor<bool> probe;
	return ai().script_engine().functor("surge_manager.is_started", probe);
}

bool LuaPcall(lua_State* L, int argc, int nresults)
{
	if (lua_pcall(L, argc, nresults, 0) != 0)
	{
		if (const char* err = lua_tostring(L, -1))
		{
			Msg("! [wristwatch] Lua error: %s", err);
		}

		lua_pop(L, 1);
		return false;
	}

	return true;
}

bool PushSurgeManager(lua_State* L, int& managerIndex)
{
	const luabind::object globals = luabind::get_globals(L);
	const luabind::object surgeModule = globals["surge_manager"];
	if (luabind::get_type(surgeModule) != LUA_TTABLE)
	{
		return false;
	}

	const luabind::object getManager = surgeModule["get_surge_manager"];
	if (luabind::get_type(getManager) != LUA_TFUNCTION)
	{
		return false;
	}

	getManager.pushvalue();
	if (!LuaPcall(L, 0, 1))
	{
		return false;
	}

	if (lua_isnil(L, -1))
	{
		lua_pop(L, 1);
		return false;
	}

	managerIndex = lua_gettop(L);
	return true;
}

bool WithSurgeManager(const std::function<bool(lua_State*, int)>& visitor)
{
	if (!g_pGameLevel || !Level().game || !IsLevelScriptsReady())
	{
		return false;
	}

	lua_State* L = ai().script_engine().lua();
	if (L == nullptr)
	{
		return false;
	}

	int managerIndex = 0;
	if (!PushSurgeManager(L, managerIndex))
	{
		return false;
	}

	const bool ok = visitor(L, managerIndex);
	lua_pop(L, 1);
	return ok;
}

bool LuaReadBool(lua_State* L, int tableIndex, const char* key, bool& out)
{
	lua_getfield(L, tableIndex, key);
	const bool ok = lua_isboolean(L, -1);
	if (ok)
	{
		out = lua_toboolean(L, -1) != 0;
	}

	lua_pop(L, 1);
	return ok;
}

bool LuaReadNumber(lua_State* L, int tableIndex, const char* key, float& out)
{
	lua_getfield(L, tableIndex, key);
	const bool ok = lua_isnumber(L, -1);
	if (ok)
	{
		out = static_cast<float>(lua_tonumber(L, -1));
	}

	lua_pop(L, 1);
	return ok;
}

bool LuaFieldIsNil(lua_State* L, int tableIndex, const char* key)
{
	lua_getfield(L, tableIndex, key);
	const bool isNil = lua_isnil(L, -1);
	lua_pop(L, 1);
	return isNil;
}

bool PushGameTime(lua_State* L, int& gameTimeIndex)
{
	const luabind::object globals = luabind::get_globals(L);
	const luabind::object gameModule = globals["game"];
	if (luabind::get_type(gameModule) != LUA_TTABLE)
	{
		return false;
	}

	const luabind::object getGameTime = gameModule["get_game_time"];
	if (luabind::get_type(getGameTime) != LUA_TFUNCTION)
	{
		return false;
	}

	getGameTime.pushvalue();
	if (!LuaPcall(L, 0, 1))
	{
		return false;
	}

	if (lua_isnil(L, -1))
	{
		lua_pop(L, 1);
		return false;
	}

	gameTimeIndex = lua_gettop(L);
	return true;
}

float DiffGameTimeSeconds(lua_State* L, int newerIndex, int olderIndex)
{
	lua_getfield(L, newerIndex, "diffSec");
	if (!lua_isfunction(L, -1))
	{
		lua_pop(L, 1);
		return 0.f;
	}

	lua_pushvalue(L, newerIndex);
	lua_pushvalue(L, olderIndex);
	if (!LuaPcall(L, 2, 1))
	{
		return 0.f;
	}

	const float diff = static_cast<float>(lua_tonumber(L, -1));
	lua_pop(L, 1);
	return diff;
}

void ApplySurgeState(const u8 mode, const u32 countdownSeconds, const u32 untilSurgeSeconds)
{
	SetWristwatchSurgeState(mode, countdownSeconds, untilSurgeSeconds);
}

void LuaSetStringField(lua_State* L, int tableIndex, const char* key, const char* value)
{
	lua_pushstring(L, value);
	lua_setfield(L, tableIndex, key);
}

void LuaSetBoolField(lua_State* L, int tableIndex, const char* key, const bool value)
{
	lua_pushboolean(L, value ? 1 : 0);
	lua_setfield(L, tableIndex, key);
}

bool PushGlobalField(lua_State* L, const char* tableName, const char* fieldName)
{
	const luabind::object globals = luabind::get_globals(L);
	if (luabind::get_type(globals) != LUA_TTABLE)
	{
		return false;
	}

	globals.pushvalue();
	lua_getfield(L, -1, tableName);
	lua_remove(L, -2);
	if (!lua_istable(L, -1) && !lua_isfunction(L, -1))
	{
		lua_pop(L, 1);
		return false;
	}

	lua_getfield(L, -1, fieldName);
	lua_remove(L, -2);
	return !lua_isnil(L, -1);
}

void FailHideFromSurgeTask(lua_State* L)
{
	const int stackBase = lua_gettop(L);

	const luabind::object globals = luabind::get_globals(L);
	const luabind::object db = globals["db"];
	if (luabind::get_type(db) != LUA_TTABLE || luabind::get_type(db["actor"]) == LUA_TNIL)
	{
		return;
	}

	if (!PushGlobalField(L, "task_manager", "get_task_manager") || !lua_isfunction(L, -1))
	{
		lua_settop(L, stackBase);
		return;
	}

	if (!LuaPcall(L, 0, 1))
	{
		lua_settop(L, stackBase);
		return;
	}

	const int taskManagerIndex = lua_gettop(L);
	lua_getfield(L, taskManagerIndex, "task_info");
	if (!lua_istable(L, -1))
	{
		lua_settop(L, stackBase);
		return;
	}

	const int taskInfoIndex = lua_gettop(L);
	lua_getfield(L, taskInfoIndex, "hide_from_surge");
	if (!lua_istable(L, -1))
	{
		lua_settop(L, stackBase);
		return;
	}

	const int hideInfoIndex = lua_gettop(L);
	lua_getfield(L, hideInfoIndex, "t");
	if (lua_isnil(L, -1))
	{
		lua_settop(L, stackBase);
		return;
	}

	LuaSetStringField(L, hideInfoIndex, "last_check_task", "fail");

	if (!PushGlobalField(L, "task", "fail"))
	{
		lua_settop(L, stackBase);
		return;
	}

	if (!PushGlobalField(L, "task_manager", "task_callback") || !lua_isfunction(L, -1))
	{
		lua_settop(L, stackBase);
		return;
	}

	lua_pushvalue(L, -3);
	lua_pushvalue(L, -2);
	LuaPcall(L, 2, 0);
	lua_settop(L, stackBase);
}
}

void EnsureSurgeManagerReady()
{
	WithSurgeManager([](lua_State* L, int managerIndex) -> bool
	{
		if (!LuaFieldIsNil(L, managerIndex, "last_surge_time"))
		{
			return true;
		}

		lua_getfield(L, managerIndex, "initialize");
		if (!lua_isfunction(L, -1))
		{
			lua_pop(L, 1);
			return true;
		}

		lua_pushvalue(L, managerIndex);
		LuaPcall(L, 1, 0);
		return true;
	});
}

bool RefreshSurgeState()
{
	if (!g_pGameLevel || !Level().game)
	{
		return false;
	}

	if (!IsLevelScriptsReady())
	{
		ApplySurgeState(0, 0, 0);
		return false;
	}

	EnsureSurgeManagerReady();

	bool refreshed = false;
	WithSurgeManager([&refreshed](lua_State* L, int managerIndex) -> bool
	{
		int gameTimeIndex = 0;
		if (!PushGameTime(L, gameTimeIndex))
		{
			return false;
		}

		bool started = false;
		LuaReadBool(L, managerIndex, "started", started);

		const float timeFactor = std::max(Level().GetGameTimeFactor(), 0.001f);

		if (started)
		{
			float diffSeconds = 0.f;
			lua_getfield(L, managerIndex, "inited_time");
			if (!lua_isnil(L, -1))
			{
				const int initedIndex = lua_gettop(L);
				diffSeconds = DiffGameTimeSeconds(L, gameTimeIndex, initedIndex);
				lua_pop(L, 1);
			}
			else
			{
				lua_pop(L, 1);
			}

			float surgeTime = 190.f;
			LuaReadNumber(L, managerIndex, "surge_time", surgeTime);

			const u32 elapsed = static_cast<u32>(std::max(0.f, std::ceil(diffSeconds / timeFactor)));
			const u32 countdown = static_cast<u32>(std::max(0.f, surgeTime - static_cast<float>(elapsed)));

			ApplySurgeState(static_cast<u8>(EWristwatchSurgeMode::ActiveSurge), countdown, 0);
		}
		else
		{
			u32 untilSeconds = 0;
			float delta = 0.f;

			lua_getfield(L, managerIndex, "last_surge_time");
			const bool hasLast = !lua_isnil(L, -1);
			if (hasLast)
			{
				const int lastIndex = lua_gettop(L);
				if (LuaReadNumber(L, managerIndex, "_delta", delta))
				{
					const float sinceLast = std::max(0.f, DiffGameTimeSeconds(L, gameTimeIndex, lastIndex));
					untilSeconds = static_cast<u32>(std::max(0.f, delta - sinceLast));
				}

				lua_pop(L, 1);
			}
			else
			{
				lua_pop(L, 1);
			}

			ApplySurgeState(static_cast<u8>(EWristwatchSurgeMode::Normal), 0, untilSeconds);
		}

		lua_pop(L, 1);
		refreshed = true;
		return true;
	});

	if (!refreshed)
	{
		if (!s_loggedSurgeUnavailable)
		{
			Msg("! [wristwatch] surge_manager is unavailable");
			s_loggedSurgeUnavailable = true;
		}

		ApplySurgeState(0, 0, 0);
		return false;
	}

	return true;
}

void SuppressVanillaNotifications()
{
	if (!IsWristwatchReplaceSurgeActive())
	{
		return;
	}

	WithSurgeManager([](lua_State* L, int managerIndex) -> bool
	{
		LuaSetStringField(L, managerIndex, "surge_message", "empty");
		LuaSetStringField(L, managerIndex, "surge_task_sect", "empty");

		bool started = false;
		if (!LuaReadBool(L, managerIndex, "started", started) || !started)
		{
			return true;
		}

		LuaSetBoolField(L, managerIndex, "task_given", true);
		LuaSetBoolField(L, managerIndex, "second_message_given", true);
		FailHideFromSurgeTask(L);
		return true;
	});
}

void TryInstallNotificationHooks()
{
	if (!IsWristwatchReplaceSurgeActive())
	{
		return;
	}

	const SWristwatchRuntimeSettings& settings = GetWristwatchRuntimeSettings();
	if (settings.surgeScript.size() == 0 || settings.surgeHooksFn.size() == 0)
	{
		return;
	}

	string_path functorName;
	xr_sprintf(functorName, "%s.%s", settings.surgeScript.c_str(), settings.surgeHooksFn.c_str());

	luabind::functor<void> functor;
	if (ai().script_engine().functor(functorName, functor))
	{
		functor();
	}
}

}

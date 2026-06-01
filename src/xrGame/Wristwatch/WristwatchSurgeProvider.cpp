#include "StdAfx.h"
#include "WristwatchSurgeProvider.h"

#include "../Level.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"

namespace
{
EWristwatchSurgeMode ClampSurgeMode(u32 mode)
{
	if (mode > static_cast<u32>(EWristwatchSurgeMode::ActiveSurge))
	{
		return EWristwatchSurgeMode::Normal;
	}

	return static_cast<EWristwatchSurgeMode>(mode);
}
}

SWristwatchSurgeState CWristwatchSurgeProvider::QueryState() const
{
	SWristwatchSurgeState state;
	if (TryCallLuaState(state))
	{
		return state;
	}

	return state;
}

bool CWristwatchSurgeProvider::TryCallLuaState(SWristwatchSurgeState& outState) const
{
	if (!g_pGameLevel || !Level().game)
	{
		return false;
	}

	luabind::functor<luabind::object> functor;
	if (!ai().script_engine().functor("wristwatch_surge.get_clock_state", functor))
	{
		static bool s_luaMissingLogged = false;
		if (!s_luaMissingLogged)
		{
			Msg("! [wristwatch] Lua module wristwatch_surge.get_clock_state is unavailable, using Normal mode");
			s_luaMissingLogged = true;
		}

		return false;
	}

	const luabind::object result = functor();
	if (result.type() != LUA_TTABLE)
	{
		return false;
	}

	const auto readU32 = [&result](const char* key, u32 fallback) -> u32
	{
		luabind::object value = result[key];
		if (value.type() == LUA_TNUMBER)
		{
			return static_cast<u32>(luabind::object_cast<float>(value));
		}

		return fallback;
	};

	outState.mode = ClampSurgeMode(readU32("mode", 0));
	outState.countdownSeconds = readU32("countdown_sec", 0);
	outState.untilSurgeSeconds = readU32("until_surge_sec", 0);
	return true;
}

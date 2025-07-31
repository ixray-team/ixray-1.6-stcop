////////////////////////////////////////////////////////////////////////////
// script_game_object_trader.сpp :	функции для торговли и торговцев
//////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "ai/trader/ai_trader.h"
#include "ai/trader/trader_animation.h"

void CScriptGameObject::set_trader_global_anim(LPCSTR anim)
{
	if (CAI_Trader* trader = object().cast_trader())
	{
		trader->animation().set_animation(anim);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cannot cast script game object to trader!");
	}

}
void CScriptGameObject::set_trader_head_anim(LPCSTR anim)
{
	if (CAI_Trader* trader = object().cast_trader())
	{
		trader->animation().set_head_animation(anim);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cannot cast script game object to trader!");
	}
}

void CScriptGameObject::set_trader_sound(LPCSTR sound, LPCSTR anim)
{
	if (CAI_Trader* trader = object().cast_trader())
	{
		trader->animation().set_sound(sound, anim);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cannot cast script game object to trader!");
	}
}

void CScriptGameObject::external_sound_start(LPCSTR sound)
{
	if (CAI_Trader* trader = object().cast_trader())
	{
		trader->animation().external_sound_start(sound);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cannot cast script game object to trader!");
	}
}

void CScriptGameObject::external_sound_stop()
{
	if (CAI_Trader* trader = object().cast_trader())
	{
		trader->animation().external_sound_stop();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cannot cast script game object to trader!");
	}
}


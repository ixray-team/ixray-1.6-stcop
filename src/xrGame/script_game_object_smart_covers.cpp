////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object_smart_covers.cpp
//	Created 	: 14.02.2008
//  Modified 	: 14.02.2008
//	Author		: Dmitriy Iassenev
//	Description : script game object class smart covers stuff
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "ai/stalker/ai_stalker.h"
#include "stalker_movement_manager_smart_cover.h"
#include "smart_cover.h"
#include "../xrScripts/script_callback_ex.h"

bool CScriptGameObject::use_smart_covers_only() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->use_smart_covers_only();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member use_smart_covers_only!");
	return false;
}

void CScriptGameObject::use_smart_covers_only(bool value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->use_smart_covers_only(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member use_smart_covers_only!");
	}
}

void CScriptGameObject::set_smart_cover_target_selector()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_selector(CScriptCallbackEx<void>());
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target_selector!");
	}
}

void CScriptGameObject::set_smart_cover_target_selector(luabind::functor<void> functor)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		CScriptCallbackEx<void> callback;
		callback.set(functor);
		stalker->movement().target_selector(callback);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target_selector!");
	}
}

void CScriptGameObject::set_smart_cover_target_selector(luabind::functor<void> functor, luabind::object object)
{
	if (CAI_Stalker* stalker = this->object().cast_stalker())
	{
		CScriptCallbackEx<void> callback;
		callback.set(functor, object);
		stalker->movement().target_selector(callback);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target_selector!");
	}
}

void CScriptGameObject::set_smart_cover_target_idle()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!stalker->g_Alive())
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : do not call smart_cover_setup_idle_target when stalker is dead!");
			return;
		}

		stalker->movement().target_idle();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member smart_cover_setup_idle_target!");
	}
}

void CScriptGameObject::set_smart_cover_target_lookout()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!stalker->g_Alive())
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : do not call smart_cover_setup_lookout_target when stalker is dead!");
			return;
		}

		stalker->movement().target_lookout();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member smart_cover_setup_lookout_target!");
	}
}

void CScriptGameObject::set_smart_cover_target_fire()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!stalker->g_Alive())
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : do not call smart_cover_setup_fire_target when stalker is dead!");
			return;
		}

		stalker->movement().target_fire();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member smart_cover_setup_fire_target!");
	}
}

void CScriptGameObject::set_smart_cover_target_fire_no_lookout()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!stalker->g_Alive())
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : do not call set_smart_cover_target_fire_no_lookout when stalker is dead!");
			return;
		}

		stalker->movement().target_fire_no_lookout();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member smart_cover_setup_fire_no_lookout_target!");
	}
}

void CScriptGameObject::set_smart_cover_target_default(bool value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!stalker->g_Alive())
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : do not call set_smart_cover_target_default when stalker is dead!");
			return;
		}

		stalker->movement().target_default(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target_default!");
	}
}

bool CScriptGameObject::in_smart_cover() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().in_smart_cover();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member in_smart_cover_mode!");
	return false;
}

void CScriptGameObject::set_dest_smart_cover(LPCSTR cover_id)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_id(cover_id);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_dest_smart_cover!");
	}
}

void CScriptGameObject::set_dest_smart_cover()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_id("");
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_dest_smart_cover!");
	}
}

CCoverPoint const* CScriptGameObject::get_dest_smart_cover()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().target_params().cover();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member get_dest_smart_cover!");
	return 0;
}

LPCSTR CScriptGameObject::get_dest_smart_cover_name()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return *stalker->movement().target_params().cover_id();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member get_dest_smart_cover!");
	return 0;
}

void CScriptGameObject::set_dest_loophole(LPCSTR loophole_id)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_loophole_id(loophole_id);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_dest_loophole!");
	}
}

void CScriptGameObject::set_dest_loophole()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_loophole_id("");
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_dest_loophole!");
	}
}

void CScriptGameObject::set_smart_cover_target(Fvector value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_fire_position(&value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target!");
	}
}

void CScriptGameObject::set_smart_cover_target()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_fire_position(0);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target!");
	}
}

void CScriptGameObject::set_smart_cover_target(CScriptGameObject *enemy_object)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().target_params().cover_fire_object(&enemy_object->object());
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_smart_cover_target!");
	}
}

bool CScriptGameObject::in_loophole_fov(LPCSTR cover_id, LPCSTR loophole_id, Fvector object_position) const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().in_fov(cover_id, loophole_id, object_position);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member object_in_loophole_fov!");
	return false;
}

bool CScriptGameObject::in_current_loophole_fov(Fvector object_position) const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().in_current_loophole_fov(object_position);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member object_in_loophole_fov!");
	return false;
}

bool CScriptGameObject::in_loophole_range(LPCSTR cover_id, LPCSTR loophole_id, Fvector object_position) const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().in_range(cover_id, loophole_id, object_position);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member object_in_loophole_range!");
	return false;
}

bool CScriptGameObject::in_current_loophole_range(Fvector object_position) const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().in_current_loophole_range(object_position);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member object_in_loophole_range!");
	return false;
}

float const CScriptGameObject::idle_min_time() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().idle_min_time();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member idle_min_time!");
	return flt_max;
}

void CScriptGameObject::idle_min_time(float value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().idle_min_time(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member idle_min_time!");
	}
}

float const CScriptGameObject::idle_max_time() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().idle_max_time();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member idle_max_time!");
	return flt_max;
}

void CScriptGameObject::idle_max_time(float value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().idle_max_time(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member idle_max_time!");
	}
}

float const CScriptGameObject::lookout_min_time() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().lookout_min_time();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member lookout_min_time!");
	return flt_max;
}

void CScriptGameObject::lookout_min_time(float value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().lookout_min_time(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member lookout_min_time!");
	}
}

float const CScriptGameObject::lookout_max_time() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().lookout_max_time();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member lookout_max_time!");
	return flt_max;
}

void CScriptGameObject::lookout_max_time(float value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().lookout_max_time(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member lookout_max_time!");
	}
}

float CScriptGameObject::apply_loophole_direction_distance() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().apply_loophole_direction_distance();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member smart_cover_enter_distance!");
	return flt_max;
}

void CScriptGameObject::apply_loophole_direction_distance(float value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().apply_loophole_direction_distance(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member smart_cover_enter_distance!");
	}
}

bool CScriptGameObject::movement_target_reached()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().current_params().equal_to_target(stalker->movement().target_params());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement_target_reached!");
	return false;
}

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "ai/monsters/bloodsucker/bloodsucker.h"
#include "ai/monsters/poltergeist/poltergeist.h"
#include "ai/monsters/burer/burer.h"
#include "ai/monsters/zombie/zombie.h"
#include "script_sound_info.h"
#include "script_monster_hit_info.h"
#include "ai/monsters/monster_home.h"
#include "ai/monsters/control_animation_base.h"

//////////////////////////////////////////////////////////////////////////
// Burer

void CScriptGameObject::set_force_anti_aim(bool force)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->set_force_anti_aim(force);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not CBaseMonster to call set_force_anti_aim");
	}
}

bool CScriptGameObject::get_force_anti_aim()
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		return monster->get_force_anti_aim();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not CBaseMonster to call get_force_anti_aim");
	return false;
}

void CScriptGameObject::burer_set_force_gravi_attack(bool force)
{
	if (CBurer* monster = smart_cast<CBurer*>(&object()))
	{
		monster->set_force_gravi_attack(force);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not CBurer to call burer_set_force_gravi_attack");
	}
}

bool CScriptGameObject::burer_get_force_gravi_attack()
{
	if (CBurer* monster = smart_cast<CBurer*>(&object()))
	{
		return monster->get_force_gravi_attack();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not CBurer to call burer_set_force_gravi_attack");
	return false;
}

//////////////////////////////////////////////////////////////////////////
// Poltergeist

void CScriptGameObject::poltergeist_set_actor_ignore(bool ignore)
{
	if (CPoltergeist* monster = smart_cast<CPoltergeist*>(&object()))
	{
		monster->set_actor_ignore(ignore);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not Poltergeist to call poltergeist_set_actor_ignore");
	}
}

bool CScriptGameObject::poltergeist_get_actor_ignore()
{
	if (CPoltergeist* monster = smart_cast<CPoltergeist*>(&object()))
	{
		return monster->get_actor_ignore();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not Poltergeist to call poltergeist_get_actor_ignore");
	return false;
}

//////////////////////////////////////////////////////////////////////////
//CAI_Bloodsucker

void CScriptGameObject::force_visibility_state(int state)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		monster->force_visibility_state(state);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member force_visibility_state!");
	}
}

int CScriptGameObject::get_visibility_state()
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		return monster->get_visibility_state();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member get_visibility_state!");
	return CAI_Bloodsucker::full_visibility;
}

void CScriptGameObject::set_override_animation(pcstr anim_name)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->anim().set_override_animation(anim_name);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not of CBaseMonster class!");
	}
}

void CScriptGameObject::clear_override_animation()
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->anim().clear_override_animation();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "object is not of CBaseMonster class!");
	}
}

void CScriptGameObject::force_stand_sleep_animation(u32 index)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		monster->force_stand_sleep_animation(index);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member force_stand_sleep_animation!");
	}
}

void CScriptGameObject::release_stand_sleep_animation()
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		monster->release_stand_sleep_animation();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member release_stand_sleep_animation!");
	}
}

void CScriptGameObject::set_invisible(bool val)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		if (val)
		{
			monster->manual_activate();
		}
		else
		{
			monster->manual_deactivate();
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member set_invisible!");
	}
}

void CScriptGameObject::set_manual_invisibility(bool val)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		monster->set_manual_control(val);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member set_manual_invisible!");
	}
}

void CScriptGameObject::bloodsucker_drag_jump(CScriptGameObject* e, LPCSTR e_str, const Fvector& position, float factor)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		CGameObject* game_object = &e->object();
		CEntityAlive* entity_alive = smart_cast<CEntityAlive*>(game_object);

		monster->set_drag_jump(entity_alive, e_str, position, factor);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot process drag, anim, jump for CAI_Bloodsucker!");
	}
}

void CScriptGameObject::set_enemy(CScriptGameObject* e)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		CGameObject* game_object = &e->object();
		CEntityAlive* entity_alive = game_object->cast_entity_alive();
		monster->SetEnemy(entity_alive);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member set_enemy!");
	}
}

void CScriptGameObject::set_vis_state(float val)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		if (val == 1)
		{
			monster->set_vis();
		}

		if (val == -1)
		{
			monster->set_invis();
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member set_vis_state!");
	}
}

void CScriptGameObject::off_collision(bool val)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		monster->set_collision_off(val);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member set_vis_state!");
	}
}

void CScriptGameObject::set_alien_control(bool val)
{
	if (CAI_Bloodsucker* monster = smart_cast<CAI_Bloodsucker*>(&object()))
	{
		monster->set_alien_control(val);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Bloodsucker : cannot access class member alien_control_activate!");
	}
}

CScriptSoundInfo CScriptGameObject::GetSoundInfo()
{
	CScriptSoundInfo ret_val;

	if (CBaseMonster* l_tpMonster = object().cast_base_monster())
	{
		if (l_tpMonster->SoundMemory.IsRememberSound())
		{
			SoundElem se;
			bool bDangerous;
			l_tpMonster->SoundMemory.GetSound(se, bDangerous);

			const CGameObject* pO = smart_cast<const CGameObject*>(se.who);
			ret_val.set((pO && !pO->getDestroy()) ? pO->lua_game_object() : 0, bDangerous, se.position, se.power, int(se.time));
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member GetSoundInfo!");
	}

	return ret_val;
}

CScriptMonsterHitInfo CScriptGameObject::GetMonsterHitInfo()
{
	CScriptMonsterHitInfo ret_val;

	if (CBaseMonster* l_tpMonster = object().cast_base_monster())
	{
		if (l_tpMonster->HitMemory.is_hit())
		{
			CObject* get_last_hit_object = l_tpMonster->HitMemory.get_last_hit_object();
			CGameObject* pO = get_last_hit_object != nullptr ? get_last_hit_object->cast_game_object() : 0;
			ret_val.set((pO != 0 && !pO->getDestroy()) ? pO->lua_game_object() : 0, l_tpMonster->HitMemory.get_last_hit_dir(), l_tpMonster->HitMemory.get_last_hit_time());
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member GetMonsterHitInfo!");
	}

	return ret_val;
}

//////////////////////////////////////////////////////////////////////////
// CBaseMonster

void CScriptGameObject::skip_transfer_enemy(bool val)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->skip_transfer_enemy(val);
	}
}

void CScriptGameObject::set_home(LPCSTR name, float r_min, float r_max, bool aggressive, float r_mid)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->Home->setup(name, r_min, r_max, aggressive, r_mid);
	}
}

void CScriptGameObject::set_home(u32 lv_ID, float r_min, float r_max, bool aggressive, float r_mid)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->Home->setup(lv_ID, r_min, r_max, aggressive, r_mid);
	}
}

void CScriptGameObject::remove_home()
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->Home->remove_home();
	}
}

bool CScriptGameObject::fake_death_fall_down()
{
	if (CZombie* monster = smart_cast<CZombie*>(&object()))
	{
		return monster->fake_death_fall_down();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CZombie : cannot access class member fake_death_fall_down!");
	return false;
}

void CScriptGameObject::fake_death_stand_up()
{
	if (CZombie* monster = smart_cast<CZombie*>(&object()))
	{
		monster->fake_death_stand_up();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CZombie : cannot access class member fake_death_fall_down!");
	}
}

void CScriptGameObject::berserk()
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->set_berserk();
	}
}

void CScriptGameObject::set_custom_panic_threshold(float value)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->set_custom_panic_threshold(value);
	}
}

void CScriptGameObject::set_default_panic_threshold()
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->set_default_panic_threshold();
	}
}
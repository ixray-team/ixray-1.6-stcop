#pragma once

#include "../../../level.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateGhostBossShieldAbstract CStateGhostBossShield<_Object>

TEMPLATE_SPECIALIZATION
CStateGhostBossShieldAbstract::CStateGhostBossShield(_Object *obj) : inherited(obj)
{
	m_shield_start_anim_length_sec = 0.0f;
	m_last_shield_started = 0;
	m_next_particle_allowed = 0;
	m_action = ACTION_IDLE;
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossShieldAbstract::initialize()
{
	inherited::initialize		();

	m_last_shield_started =  Device.dwTimeGlobal;
	m_next_particle_allowed = 0;
	m_action = ACTION_IDLE;
//	m_shield_start_anim_length_sec = object->anim().motion_time(eAnimShieldStart, 0, object->Visual());
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossShieldAbstract::execute()
{
	switch (m_action)
	{
	case ACTION_IDLE:
		object->ActivateShield();
		object->com_man().ta_activate(object->anim_triple_shield);
		m_action = ACTION_SHIELD_STARTED;
		break;

	case ACTION_SHIELD_STARTED:
		if (object->m_shield_keep_particle && Device.dwTimeGlobal > m_next_particle_allowed)
		{
			CParticlesPlayer* PP = smart_cast<CParticlesPlayer*>(object);
			if(!PP) return;
			PP->StartParticles(object->m_shield_keep_particle,Fvector().set(0.0f,0.1f,0.0f),object->ID());
			m_next_particle_allowed = Device.dwTimeGlobal + object->m_shield_keep_particle_period;
		}
		break;
//	case ACTION_WAIT_TRIPLE_END:
//		if (!object->com_man().ta_is_active()) 
//		{
//			m_action = ACTION_COMPLETED; 
//		}
//		break;
	case ACTION_COMPLETED:
		break;
	}

	if (object->EnemyMan.get_enemy())
		object->dir().face_target(object->EnemyMan.get_enemy(), 500);
	else {
		Fvector pos;
		pos.mad(object->Position(), Fvector().set(0.1f,0.0f,0.0f));
		object->dir().face_target(pos, 500);
	}
	object->set_action(ACT_STAND_IDLE);
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossShieldAbstract::finalize()
{
	inherited::finalize();

	object->com_man().ta_pointbreak	();
	object->DeactivateShield();
	object->set_script_capture(true);
}

TEMPLATE_SPECIALIZATION
void CStateGhostBossShieldAbstract::critical_finalize()
{
	inherited::critical_finalize();

	object->com_man().ta_pointbreak	();
	object->DeactivateShield();
	object->set_script_capture(false);
}

TEMPLATE_SPECIALIZATION
bool CStateGhostBossShieldAbstract::check_start_conditions()
{
	if (Device.dwTimeGlobal > m_last_shield_started + object->m_shield_time + object->m_shield_cooldown)
	{
		if (object->EnemyMan.get_enemy() && !object->EnemyMan.enemy_see_me_now()) return false; 
	}
	else 
		return false;

	if (object->com_man().ta_is_active()) return false;

	return true;
}

TEMPLATE_SPECIALIZATION
bool CStateGhostBossShieldAbstract::check_completion()
{
	if (Device.dwTimeGlobal <= m_last_shield_started + object->m_shield_time)
	{
		const CEntityAlive* enemy = object->EnemyMan.get_enemy();
		if ((enemy && enemy != Actor()) || !Actor()->IsReloadingWeapon())
			return enemy == 0;
	}
	return true;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateGhostBossShieldAbstract

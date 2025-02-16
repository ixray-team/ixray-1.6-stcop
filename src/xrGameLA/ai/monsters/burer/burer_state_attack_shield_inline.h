#pragma once

#include "../../../level.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateBurerShieldAbstract CStateBurerShield<_Object>

TEMPLATE_SPECIALIZATION
CStateBurerShieldAbstract::CStateBurerShield(_Object *obj) : inherited(obj)
{
	m_shield_start_anim_length_sec = 0.0f;
	m_last_shield_started = 0;
	m_next_particle_allowed = 0;
	m_action = ACTION_IDLE;
}

TEMPLATE_SPECIALIZATION
void CStateBurerShieldAbstract::initialize()
{
	inherited::initialize		();

	m_last_shield_started =  Device.dwTimeGlobal;
	m_next_particle_allowed = 0;
	m_action = ACTION_IDLE;
//	m_shield_start_anim_length_sec = object->anim().motion_time(eAnimShieldStart, 0, object->Visual());
}

TEMPLATE_SPECIALIZATION
void CStateBurerShieldAbstract::execute()
{
	switch (m_action)
	{
	case ACTION_IDLE:
		this->object->ActivateShield();
		this->object->com_man().ta_activate(this->object->anim_triple_shield);
		m_action = ACTION_SHIELD_STARTED;
		break;

	case ACTION_SHIELD_STARTED:
		if (this->object->m_shield_keep_particle && Device.dwTimeGlobal > m_next_particle_allowed)
		{
			CParticlesPlayer* PP = smart_cast<CParticlesPlayer*>(this->object);
			if(!PP) return;
			PP->StartParticles(this->object->m_shield_keep_particle,Fvector().set(0.0f,0.1f,0.0f), this->object->ID());
			m_next_particle_allowed = Device.dwTimeGlobal + this->object->m_shield_keep_particle_period;
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

	if (this->object->EnemyMan.get_enemy())
		this->object->dir().face_target(this->object->EnemyMan.get_enemy(), 500);
	else {
		Fvector pos;
		pos.mad(this->object->Position(), Fvector().set(0.1f,0.0f,0.0f));
		this->object->dir().face_target(pos, 500);
	}
	this->object->set_action(ACT_STAND_IDLE);
}

TEMPLATE_SPECIALIZATION
void CStateBurerShieldAbstract::finalize()
{
	inherited::finalize();

	this->object->com_man().ta_pointbreak	();
	this->object->DeactivateShield();
	this->object->set_script_capture(true);
}

TEMPLATE_SPECIALIZATION
void CStateBurerShieldAbstract::critical_finalize()
{
	inherited::critical_finalize();

	this->object->com_man().ta_pointbreak	();
	this->object->DeactivateShield();
	this->object->set_script_capture(false);
}

TEMPLATE_SPECIALIZATION
bool CStateBurerShieldAbstract::check_start_conditions()
{
	if (Device.dwTimeGlobal > m_last_shield_started + this->object->m_shield_time + this->object->m_shield_cooldown)
	{
		if (this->object->EnemyMan.get_enemy() && !this->object->EnemyMan.enemy_see_me_now()) return false;
	}
	else 
		return false;

	if (this->object->com_man().ta_is_active()) return false;

	return true;
}

TEMPLATE_SPECIALIZATION
bool CStateBurerShieldAbstract::check_completion()
{
	if (Device.dwTimeGlobal <= m_last_shield_started + this->object->m_shield_time)
	{
		const CEntityAlive* enemy = this->object->EnemyMan.get_enemy();
		if ((enemy && enemy != Actor()) || !Actor()->IsReloadingWeapon())
			return enemy == 0;
	}
	return true;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateBurerShieldAbstract

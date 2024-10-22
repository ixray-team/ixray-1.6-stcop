#include "stdafx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "burer.h"
#include "burer_state_attack_shield.h"

#include "../states/monster_state_attack_on_run.h"

CStateBurerShield::CStateBurerShield(CBaseMonster* object) : inherited(object)
{
	pBurerBase = smart_cast<CBurerBase*>(object);

	m_started = {};

	m_last_shield_started = 0;
	m_shield_start_anim_length_sec = 0;
	m_next_particle_allowed = 0;
}

CStateBurerShield::~CStateBurerShield()
{

}

void   CStateBurerShield::initialize()
{
	inherited::initialize();
	object->set_script_capture(false);
	m_last_shield_started = current_time();
	m_next_particle_allowed = 0;
	m_started = false;

	MotionID	motion{};
	object->anim().get_animation_info(eAnimShieldStart, 0, motion, m_shield_start_anim_length_sec);
}

void   CStateBurerShield::execute()
{
	if (!m_started) // && current_time() > m_last_shield_started + TTime(m_shield_start_anim_length_sec*1000) )
	{
		m_started = true;
		pBurerBase->ActivateShield();
	}

	if (m_started &&
		pBurerBase->m_shield_keep_particle != 0 &&
		current_time() > m_next_particle_allowed)
	{
		object->CParticlesPlayer::StartParticles(pBurerBase->m_shield_keep_particle,
			Fvector().set(0, 1, 0),
			object->ID(),
			-1,
			true);

		m_next_particle_allowed = current_time() + pBurerBase->m_shield_keep_particle_period;
	}

	pBurerBase->face_enemy();
	object->set_action(ACT_STAND_IDLE);

	object->anim().set_override_animation(m_started ? eAnimShieldContinue : eAnimShieldStart);
}

void   CStateBurerShield::finalize()
{
	inherited::finalize();
	pBurerBase->DeactivateShield();
	object->set_script_capture(true);
}

void   CStateBurerShield::critical_finalize()
{
	inherited::critical_finalize();
	pBurerBase->DeactivateShield();
	object->set_script_capture(false);
}

bool   CStateBurerShield::check_start_conditions()
{
	if (current_time() < m_last_shield_started + pBurerBase->m_shield_time + pBurerBase->m_shield_cooldown)
		return							false;

	if (!object->EnemyMan.enemy_see_me_now())
		return							false;

	return								true;
}

bool   CStateBurerShield::check_completion()
{
	if (current_time() > m_last_shield_started + pBurerBase->m_shield_time)
		return							true;

	CEntityAlive const* enemy = object->EnemyMan.get_enemy();
	if (!enemy)
		return							true;

	if (enemy == Actor())
	{
		if (actor_is_reloading_weapon())
			return						true;
	}

	if (!object->EnemyMan.get_enemy())
		return							true;

	return								false;
}

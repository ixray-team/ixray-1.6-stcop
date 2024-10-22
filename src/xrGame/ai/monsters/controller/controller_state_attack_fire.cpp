#include "StdAfx.h"
#include "controller.h"
#include "controller_state_attack_fire.h"

#include "controller_animation.h"
#include "controller_direction.h"

CStateControlFire::CStateControlFire(CBaseMonster* object) : inherited(object)
{
	pControllerBase = smart_cast<CControllerBase*>(object);
}

CStateControlFire::~CStateControlFire()
{

}

void CStateControlFire::reinit()
{
	inherited::reinit();

	m_time_state_last_execute = 0;
}

void CStateControlFire::initialize()
{
	inherited::initialize();
	pControllerBase->set_psy_fire_delay_zero();
	m_time_started = time();
}

void CStateControlFire::execute()
{
	object->dir().face_target(object->EnemyMan.get_enemy());
	pControllerBase->custom_dir().head_look_point(get_head_position(const_cast<CEntityAlive*>(object->EnemyMan.get_enemy())));

	pControllerBase->custom_anim().set_body_state(CControllerAnimation::eTorsoIdle, CControllerAnimation::eLegsTypeSteal);
}

void CStateControlFire::finalize()
{
	inherited::finalize();
	pControllerBase->set_psy_fire_delay_default();
	m_time_state_last_execute = time();
}

void CStateControlFire::critical_finalize()
{
	inherited::critical_finalize();
	pControllerBase->set_psy_fire_delay_default();
	m_time_state_last_execute = time();
}

bool CStateControlFire::check_start_conditions()
{
	if (!object->EnemyMan.see_enemy_now()) return false;
	if (object->EnemyMan.get_enemy()->Position().distance_to(object->Position()) < EntityDefinitions::CControllerBase::MIN_ENEMY_DISTANCE) return false;
	if (m_time_state_last_execute + EntityDefinitions::CControllerBase::STATE_EXECUTE_DELAY > time()) return false;

	return true;
}

bool CStateControlFire::check_completion()
{
	if (!object->EnemyMan.see_enemy_now()) return true;
	if (object->HitMemory.is_hit()) return true;
	if (object->EnemyMan.get_enemy()->Position().distance_to(object->Position()) < EntityDefinitions::CControllerBase::MIN_ENEMY_DISTANCE) return true;
	if (m_time_started + EntityDefinitions::CControllerBase::STATE_MAX_TIME < time()) return true;

	return false;
}

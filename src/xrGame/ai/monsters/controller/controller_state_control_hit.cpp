#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "controller.h"

#include "controller_state_control_hit.h"

#include "sound_player.h"

CStateControlAttack::CStateControlAttack(CBaseMonster* object) : inherited(object)
{
	pControllerBase = smart_cast<CControllerBase*>(object);
}

CStateControlAttack::~CStateControlAttack()
{
}

void CStateControlAttack::initialize()
{
	inherited::initialize();

	m_action = eActionPrepare;
	time_control_started = 0;
}

void CStateControlAttack::execute()
{
	switch (m_action) {
	case eActionPrepare:
		execute_hit_prepare();
		m_action = eActionContinue;
		break;

	case eActionContinue:
		execute_hit_continue();
		break;

	case eActionFire:
		execute_hit_fire();
		m_action = eActionWaitTripleEnd;
		break;

	case eActionWaitTripleEnd:
		if (!object->com_man().ta_is_active()) {
			m_action = eActionCompleted;
		}

	case eActionCompleted:
		break;
	}

	object->anim().m_tAction = ACT_STAND_IDLE;
	object->dir().face_target(object->EnemyMan.get_enemy(), 1200);

	object->sound().play(MonsterSound::eMonsterSoundAggressive, 0, 0, object->db().m_dwAttackSndDelay);
}

bool CStateControlAttack::check_start_conditions()
{
	float dist = object->Position().distance_to(object->EnemyMan.get_enemy_position());
	if (dist < EntityDefinitions::CControllerBase::GOOD_DISTANCE_FOR_CONTROL_HIT) return false;

	if (!object->EnemyMan.see_enemy_now()) return false;

	// всё ок, можно начать атаку
	return true;
}

bool CStateControlAttack::check_completion()
{
	return (m_action == eActionCompleted);
}

void CStateControlAttack::finalize()
{
	inherited::finalize();
}

void CStateControlAttack::critical_finalize()
{
	inherited::critical_finalize();
}

void CStateControlAttack::execute_hit_prepare()
{
	pControllerBase->com_man().ta_activate(pControllerBase->anim_triple_control);
	pControllerBase->play_control_sound_start();

	time_control_started = Device.dwTimeGlobal;
}

void CStateControlAttack::execute_hit_continue()
{
	// проверить на грави удар
	if (time_control_started + EntityDefinitions::CControllerBase::CONTROL_PREPARE_TIME < Device.dwTimeGlobal) {
		m_action = eActionFire;
	}
}

void CStateControlAttack::execute_hit_fire()
{
	object->com_man().ta_pointbreak();

	if (object->EnemyMan.see_enemy_now()) pControllerBase->control_hit();
}

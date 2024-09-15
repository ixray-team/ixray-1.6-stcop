#pragma once

#define GOOD_DISTANCE_FOR_CONTROL_HIT	8.f
#define CONTROL_PREPARE_TIME			2900


CStateControlAttack::CStateControlAttack(CBaseMonster *obj) : inherited(obj)
{
}


CStateControlAttack::~CStateControlAttack()
{
}


void CStateControlAttack::initialize()
{
	inherited::initialize();
	
	m_action				= eActionPrepare;
	time_control_started	= 0;
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

	object->sound().play(MonsterSound::eMonsterSoundAggressive, 0,0,object->db().m_dwAttackSndDelay);
}


bool CStateControlAttack::check_start_conditions()
{
	float dist = object->Position().distance_to(object->EnemyMan.get_enemy_position());
	if (dist < GOOD_DISTANCE_FOR_CONTROL_HIT) return false;

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

//////////////////////////////////////////////////////////////////////////
// Processing
//////////////////////////////////////////////////////////////////////////



void CStateControlAttack::execute_hit_prepare()
{
	object->com_man().ta_activate(object->anim_triple_control);
	object->play_control_sound_start();

	time_control_started = Device.dwTimeGlobal;
}


void CStateControlAttack::execute_hit_continue()
{
	// проверить на грави удар
	if (time_control_started + CONTROL_PREPARE_TIME < Device.dwTimeGlobal) {
		m_action = eActionFire;
	}
}


void CStateControlAttack::execute_hit_fire()
{
	object->com_man().ta_pointbreak();
	
	if (object->EnemyMan.see_enemy_now()) object->control_hit();
}

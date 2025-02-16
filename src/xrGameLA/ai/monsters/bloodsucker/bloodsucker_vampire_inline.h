#pragma once

#include "bloodsucker_vampire_execute.h"
#include "../states/state_hide_from_point.h"
#include "bloodsucker_vampire_approach.h"
#include "bloodsucker_vampire_hide.h"
#include "../../../clsid_game.h"
#include "../../../entity_alive.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateBloodsuckerVampireAbstract CStateBloodsuckerVampire<_Object>

#define RUN_AWAY_DISTANCE			5.f
#define MAX_DISTANCE_TO_ENEMY	2.f

TEMPLATE_SPECIALIZATION


CStateBloodsuckerVampireAbstract::CStateBloodsuckerVampire(_Object *obj) : inherited(obj)
{
	this->add_state	(eStateVampire_ApproachEnemy, new CStateBloodsuckerVampireApproach<_Object>(obj));
	this->add_state	(eStateVampire_Execute, new CStateBloodsuckerVampireExecute<_Object>(obj));
	this->add_state	(eStateVampire_RunAway, new CStateMonsterHideFromPoint<_Object>(obj));
	this->add_state	(eStateVampire_Hide, new CStateBloodsuckerVampireHide<_Object>(obj));
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::reinit()
{
	inherited::reinit	();
	
	m_time_last_vampire	= 0;
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::initialize()
{
	inherited::initialize						();
	this->object->start_invisible_predator			();

	m_enemy	= this->object->EnemyMan.get_enemy		();

	this->object->sound().play						(CAI_Bloodsucker::eVampireStartHunt);
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::reselect_state()
{
	u32 state_id = u32(-1);

	// check if we executed 
	if (this->prev_substate == eStateVampire_ApproachEnemy) {
		if (this->get_state(eStateVampire_Execute)->check_start_conditions())
			state_id = eStateVampire_Execute;
	}

	// check if reach time in vampire state is out - then hide
	if (this->prev_substate == eStateVampire_Execute)
		state_id = eStateVampire_Hide;

	// else just 
	if (state_id == u32(-1))
		state_id = eStateVampire_ApproachEnemy;

	this->select_state(state_id);
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::check_force_state()
{
	// check if we can start execute
	if (this->prev_substate == eStateVampire_ApproachEnemy) {
		if (this->get_state(eStateVampire_Execute)->check_start_conditions())
			this->current_substate = u32(-1);
	}
}


TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::finalize()
{
	inherited::finalize();

	this->object->stop_invisible_predator	();
	m_time_last_vampire				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::critical_finalize()
{
	inherited::critical_finalize	();
	
	this->object->stop_invisible_predator	();
	m_time_last_vampire				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
bool CStateBloodsuckerVampireAbstract::check_start_conditions()
{
	if (!this->object->WantVampire()) return false;
	if (this->object->m_bDamaged) return false;

	const CEntityAlive *m_enemy = this->object->EnemyMan.get_enemy();
	if (m_enemy->CLS_ID != CLSID_OBJECT_ACTOR)							return false;
	if (!this->object->EnemyMan.see_enemy_now())								return false;
	if (this->object->CControlledActor::is_installed() && this->object->CControlledActor::is_controlling())	return false;
	if (this->current_substate == eStateAttack_RunAttack)							return false;
	if (this->object->threaten_time() > 0)								return false;

	const CActor *m_actor = smart_cast<const CActor*>(m_enemy);
	VERIFY(m_actor);
	if (m_actor->input_external_handler_installed())						return false;

	float dist_to_enemy = this->object->EnemyMan.get_enemy_position().distance_to(this->object->Position());
	if (dist_to_enemy > MAX_DISTANCE_TO_ENEMY)							return false;

	if (this->controlling_value == 1)									return false;

	return true;
  }

TEMPLATE_SPECIALIZATION
bool CStateBloodsuckerVampireAbstract::check_completion()
{
	// если убежал
	if ((this->current_substate == eStateVampire_Hide) &&
		this->get_state_current()->check_completion())	return true;

	// если враг изменился
	if (m_enemy != this->object->EnemyMan.get_enemy())		return true;
	
	// если актера уже контролит другой кровосос
	if ((this->current_substate != eStateVampire_Execute) &&
		this->object->CControlledActor::is_controlling())	return true;

	return false;
}


TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateVampire_RunAway) {

		SStateHideFromPoint		data;
		data.point				= this->object->EnemyMan.get_enemy_position();
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type			= eAT_Aggressive;
		data.distance			= RUN_AWAY_DISTANCE;
		data.action.action		= ACT_RUN;
		data.action.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = this->object->db().m_dwAttackSndDelay;
		data.action.time_out	= 3000;

		state->fill_data_with(&data, sizeof(SStateHideFromPoint));

		return;
	}
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateBloodsuckerVampireAbstract


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
	add_state	(eStateVampire_ApproachEnemy, new CStateBloodsuckerVampireApproach<_Object>(obj));
	add_state	(eStateVampire_Execute, new CStateBloodsuckerVampireExecute<_Object>(obj));
	add_state	(eStateVampire_RunAway, new CStateMonsterHideFromPoint<_Object>(obj));
	add_state	(eStateVampire_Hide, new CStateBloodsuckerVampireHide<_Object>(obj));
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
	object->start_invisible_predator			();

	m_enemy	= object->EnemyMan.get_enemy		();

	object->sound().play						(CAI_Bloodsucker::eVampireStartHunt);
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::reselect_state()
{
	u32 state_id = u32(-1);

	// check if we executed 
	if (prev_substate == eStateVampire_ApproachEnemy) {
		if (get_state(eStateVampire_Execute)->check_start_conditions())
			state_id = eStateVampire_Execute;
	}

	// check if reach time in vampire state is out - then hide
	if (prev_substate == eStateVampire_Execute)
		state_id = eStateVampire_Hide;

	// else just 
	if (state_id == u32(-1))
		state_id = eStateVampire_ApproachEnemy;

	select_state(state_id);	
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::check_force_state()
{
	// check if we can start execute
	if (prev_substate == eStateVampire_ApproachEnemy) {
		if (get_state(eStateVampire_Execute)->check_start_conditions())
			current_substate = u32(-1);
	}
}


TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::finalize()
{
	inherited::finalize();

	object->stop_invisible_predator	();
	m_time_last_vampire				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::critical_finalize()
{
	inherited::critical_finalize	();
	
	object->stop_invisible_predator	();
	m_time_last_vampire				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
bool CStateBloodsuckerVampireAbstract::check_start_conditions()
{
	if (!object->WantVampire()) return false;
	if (object->m_bDamaged) return false;

	const CEntityAlive *m_enemy = object->EnemyMan.get_enemy();
	if (m_enemy->CLS_ID != CLSID_OBJECT_ACTOR)							return false;
	if (!object->EnemyMan.see_enemy_now())								return false;
	if (object->CControlledActor::is_installed() && object->CControlledActor::is_controlling())	return false;
	if (current_substate == eStateAttack_RunAttack)							return false;
	if (object->threaten_time() > 0)								return false;

	const CActor *m_actor = smart_cast<const CActor*>(m_enemy);
	VERIFY(m_actor);
	if (m_actor->input_external_handler_installed())						return false;

	float dist_to_enemy = object->EnemyMan.get_enemy_position().distance_to(object->Position());
	if (dist_to_enemy > MAX_DISTANCE_TO_ENEMY)							return false;

	if (controlling_value == 1)									return false;

	return true;
  }

TEMPLATE_SPECIALIZATION
bool CStateBloodsuckerVampireAbstract::check_completion()
{
	// если убежал
	if ((current_substate == eStateVampire_Hide) && 
		get_state_current()->check_completion())	return true;

	// если враг изменился
	if (m_enemy != object->EnemyMan.get_enemy())		return true;
	
	// если актера уже контролит другой кровосос
	if ((current_substate != eStateVampire_Execute) && 
		object->CControlledActor::is_controlling())	return true;

	return false;
}


TEMPLATE_SPECIALIZATION
void CStateBloodsuckerVampireAbstract::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStateVampire_RunAway) {

		SStateHideFromPoint		data;
		data.point				= object->EnemyMan.get_enemy_position();
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type			= eAT_Aggressive;
		data.distance			= RUN_AWAY_DISTANCE;
		data.action.action		= ACT_RUN;
		data.action.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = object->db().m_dwAttackSndDelay;
		data.action.time_out	= 3000;

		state->fill_data_with(&data, sizeof(SStateHideFromPoint));

		return;
	}
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateBloodsuckerVampireAbstract


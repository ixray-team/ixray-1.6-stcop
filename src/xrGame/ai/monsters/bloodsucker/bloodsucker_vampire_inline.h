#pragma once

#include "bloodsucker_vampire_execute.h"
#include "../states/state_hide_from_point.h"
#include "bloodsucker_vampire_approach.h"
#include "bloodsucker_vampire_hide.h"

#define RUN_AWAY_DISTANCE			50.f

CStateBloodsuckerVampire::CStateBloodsuckerVampire(CAI_Bloodsucker* obj) : inherited(obj)
{
	this->add_state	(eStateVampire_ApproachEnemy,	xr_new<CStateBloodsuckerVampireApproach>	(obj));
	this->add_state	(eStateVampire_Execute,			xr_new<CStateBloodsuckerVampireExecute>	(obj));
	this->add_state	(eStateVampire_RunAway,			xr_new<CStateMonsterHideFromPoint>		(obj));
	this->add_state	(eStateVampire_Hide,			xr_new<CStateBloodsuckerVampireHide>		(obj));
}

void CStateBloodsuckerVampire::reinit()
{
	inherited::reinit	();
}

void CStateBloodsuckerVampire::initialize()
{
	inherited::initialize						();
	this->object->set_visibility_state				(CAI_Bloodsucker::partial_visibility);

	enemy	= this->object->EnemyMan.get_enemy		();

	this->object->sound().play						(CAI_Bloodsucker::eVampireStartHunt);
}

void CStateBloodsuckerVampire::reselect_state()
{
	u32 state_id = u32(-1);
		
	// check if we can start execute
	if (this->prev_substate == eStateVampire_ApproachEnemy) {
		if (this->get_state(eStateVampire_Execute)->check_start_conditions())	
			state_id = eStateVampire_Execute;
	}

	// check if we executed 
	if (this->prev_substate == eStateVampire_Execute)
		state_id = eStateVampire_Hide;
	
	// check if reach time in vampire state is out - then hide
	if (this->prev_substate == eStateVampire_ApproachEnemy)
		state_id = eStateVampire_Hide;

	// check if we hiding - then hide again
	if (this->prev_substate == eStateVampire_Hide)
		state_id = eStateVampire_Hide;

	// else just 
	if (state_id == u32(-1)) state_id = eStateVampire_ApproachEnemy;

	this->select_state(state_id);
}

void CStateBloodsuckerVampire::check_force_state()
{
	// check if we can start execute
	if (this->prev_substate == eStateVampire_ApproachEnemy) {
		if (this->get_state(eStateVampire_Execute)->check_start_conditions())
			this->current_substate = u32(-1);
	}
}

void CStateBloodsuckerVampire::finalize()
{
	inherited::finalize();

	this->object->set_visibility_state	(CAI_Bloodsucker::full_visibility);
	CAI_Bloodsucker::m_time_last_vampire				= Device.dwTimeGlobal;
}

void CStateBloodsuckerVampire::critical_finalize()
{
	inherited::critical_finalize	();
	
	this->object->set_visibility_state	(CAI_Bloodsucker::full_visibility);
	CAI_Bloodsucker::m_time_last_vampire				= Device.dwTimeGlobal;
}

bool CStateBloodsuckerVampire::check_start_conditions()
{
	if (!this->object->WantVampire()) return false;
	if (this->object->berserk_always) return false;
	
	// является ли враг актером
	const CEntityAlive *enemy = this->object->EnemyMan.get_enemy();
	if (!smart_cast<CActor const*>(enemy))			return false;
	if (!this->object->EnemyMan.see_enemy_now())			return false;
	if (this->object->CControlledActor::is_controlling())	return false;

	const CActor *actor = smart_cast<const CActor *>(enemy);
	VERIFY(actor);
	if (actor->input_external_handler_installed()) return false;

	if (CAI_Bloodsucker::m_time_last_vampire + this->object->m_vampire_min_delay > Device.dwTimeGlobal) return false;

	return true;
}

bool CStateBloodsuckerVampire::check_completion()
{
	// если убежал
	if ((this->current_substate == eStateVampire_Hide) &&
		this->get_state_current()->check_completion())	return true;

	// если враг изменился
	if (enemy != this->object->EnemyMan.get_enemy())		return true;
	
	// если актера уже контролит другой кровосос
	if ((this->current_substate != eStateVampire_Execute) &&
		this->object->CControlledActor::is_controlling())	return true;

	return false;
}

void CStateBloodsuckerVampire::setup_substates()
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
		data.action.time_out	= 15000;

		state->fill_data_with(&data, sizeof(SStateHideFromPoint));

		return;
	}
}

void CStateBloodsuckerVampire::remove_links	(CObject* object)
{
	if (enemy == object)
		enemy					= 0;
}

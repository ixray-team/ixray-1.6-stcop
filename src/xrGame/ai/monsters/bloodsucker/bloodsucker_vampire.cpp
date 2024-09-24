#include "stdafx.h"
#include "actor.h"
#include "sound_player.h"
#include "bloodsucker.h"

#include "bloodsucker_vampire.h"

#include "bloodsucker_vampire_execute.h"
#include "../states/state_hide_from_point.h"
#include "bloodsucker_vampire_approach.h"
#include "bloodsucker_vampire_hide.h"

CustomBloodsuckerStateVampire::CustomBloodsuckerStateVampire(CustomBloodsucker* object) : inherited(object)
{
	enemy = nullptr;

	m_pBloodsucker = smart_cast<CustomBloodsucker*>(object);

	this->add_state	(eStateVampire_ApproachEnemy,	new CustomBloodsuckerVampireApproach(object));
	this->add_state	(eStateVampire_Execute,			new CustomBloodsuckerStateVampireExecute(object));
	this->add_state	(eStateVampire_RunAway,			new CStateMonsterHideFromPoint		(object));
	this->add_state	(eStateVampire_Hide,			new CustomBloodsuckerStateVampireHide(object));
}

CustomBloodsuckerStateVampire::~CustomBloodsuckerStateVampire()
{

}

void CustomBloodsuckerStateVampire::reinit()
{
	inherited::reinit	();
}

void CustomBloodsuckerStateVampire::initialize()
{
	inherited::initialize						();
	this->m_pBloodsucker->set_visibility_state				(CustomBloodsucker::partial_visibility);

	enemy	= this->object->EnemyMan.get_enemy		();

	this->m_pBloodsucker->sound().play						(CustomBloodsucker::eVampireStartHunt);
}

void CustomBloodsuckerStateVampire::reselect_state()
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

void CustomBloodsuckerStateVampire::check_force_state()
{
	// check if we can start execute
	if (this->prev_substate == eStateVampire_ApproachEnemy) {
		if (this->get_state(eStateVampire_Execute)->check_start_conditions())
			this->current_substate = u32(-1);
	}
}

void CustomBloodsuckerStateVampire::finalize()
{
	inherited::finalize();

	this->m_pBloodsucker->set_visibility_state	(CustomBloodsucker::full_visibility);
	CustomBloodsucker::m_time_last_vampire				= Device.dwTimeGlobal;
}

void CustomBloodsuckerStateVampire::critical_finalize()
{
	inherited::critical_finalize	();
	
	this->m_pBloodsucker->set_visibility_state	(CustomBloodsucker::full_visibility);
	CustomBloodsucker::m_time_last_vampire				= Device.dwTimeGlobal;
}

bool CustomBloodsuckerStateVampire::check_start_conditions()
{
	if (!this->m_pBloodsucker->WantVampire()) return false;
	if (this->object->berserk_always) return false;
	
	// является ли враг актером
	const CEntityAlive *enemy = this->object->EnemyMan.get_enemy();
	if (!smart_cast<CActor const*>(enemy))			return false;
	if (!this->object->EnemyMan.see_enemy_now())			return false;
	if (this->m_pBloodsucker->is_controlling())	return false;

	const CActor *actor = smart_cast<const CActor *>(enemy);
	VERIFY(actor);
	if (actor->input_external_handler_installed()) return false;

	if (CustomBloodsucker::m_time_last_vampire + this->m_pBloodsucker->m_vampire_min_delay > Device.dwTimeGlobal) return false;

	return true;
}

bool CustomBloodsuckerStateVampire::check_completion()
{
	// если убежал
	if ((this->current_substate == eStateVampire_Hide) &&
		this->get_state_current()->check_completion())	return true;

	// если враг изменился
	if (enemy != this->object->EnemyMan.get_enemy())		return true;
	
	// если актера уже контролит другой кровосос
	if ((this->current_substate != eStateVampire_Execute) &&
		this->m_pBloodsucker->is_controlling())	return true;

	return false;
}

void CustomBloodsuckerStateVampire::setup_substates()
{
	const float RUN_AWAY_DISTANCE = 50.f;

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

void CustomBloodsuckerStateVampire::remove_links	(CObject* object)
{
	if (enemy == object)
		enemy					= 0;
}
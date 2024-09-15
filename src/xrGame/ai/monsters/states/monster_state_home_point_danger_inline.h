#pragma once

#include "state_move_to_point.h"
#include "state_look_point.h"
#include "state_custom_action.h"
#include "../../../cover_point.h"
#include "../monster_cover_manager.h"
#include "../monster_home.h"
#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

//////////////////////////////////////////////////////////////////////////
// Construct Substates
//////////////////////////////////////////////////////////////////////////


CStateMonsterDangerMoveToHomePoint::CStateMonsterDangerMoveToHomePoint(CBaseMonster *obj) : inherited(obj) 
{
	this->add_state	(eStatePanic_HomePoint_Hide,			xr_new<CStateMonsterMoveToPointEx<CBaseMonster> >	(obj));
	this->add_state	(eStatePanic_HomePoint_LookOpenPlace,	xr_new<CStateMonsterLookToPoint<CBaseMonster> >		(obj));
	this->add_state	(eStatePanic_HomePoint_Camp,			xr_new<CStateMonsterCustomAction<CBaseMonster> >		(obj));
}

//////////////////////////////////////////////////////////////////////////
// Initialize/Finalize
//////////////////////////////////////////////////////////////////////////


void CStateMonsterDangerMoveToHomePoint::initialize()
{
	inherited::initialize	();

	// get the most danger position
	get_most_danger_pos		();

	m_target_node			= this->object->Home->get_place_in_cover();
	m_skip_camp				= false;

	if (m_target_node == u32(-1)) {
		m_target_node	= this->object->Home->get_place();
		m_skip_camp		= true;
	}

	CMonsterSquad *squad = monster_squad().get_squad(this->object);
	squad->lock_cover(m_target_node);
}


void CStateMonsterDangerMoveToHomePoint::finalize()
{
	inherited::finalize();
	CMonsterSquad *squad = monster_squad().get_squad(this->object);
	squad->unlock_cover(m_target_node);
}


void CStateMonsterDangerMoveToHomePoint::critical_finalize()
{
	inherited::critical_finalize();

	CMonsterSquad *squad = monster_squad().get_squad(this->object);
	squad->unlock_cover(m_target_node);
}

//////////////////////////////////////////////////////////////////////////
// Check Start Conditions / Completion
//////////////////////////////////////////////////////////////////////////


bool CStateMonsterDangerMoveToHomePoint::check_start_conditions()
{
	return (!this->object->Home->at_home() && !this->object->Home->at_home(get_most_danger_pos()));
}


bool CStateMonsterDangerMoveToHomePoint::check_completion()
{
	if (this->object->HitMemory.get_last_hit_time() > this->time_state_started) return true;
	if (m_skip_camp && (this->prev_substate != u32(-1)) && (this->prev_substate != eStatePanic_HomePoint_Hide) ) return true;

	return false;
}

//////////////////////////////////////////////////////////////////////////
// Select Substate
//////////////////////////////////////////////////////////////////////////


void CStateMonsterDangerMoveToHomePoint::reselect_state()
{
	if (this->prev_substate == u32(-1)) {
		this->select_state(eStatePanic_HomePoint_Hide);
		return;
	} 

	if (this->prev_substate == eStatePanic_HomePoint_Hide) {
		this->select_state(eStatePanic_HomePoint_LookOpenPlace);
		return;
	}

	this->select_state(eStatePanic_HomePoint_Camp);
}

//////////////////////////////////////////////////////////////////////////
// Setup Substates
//////////////////////////////////////////////////////////////////////////


void CStateMonsterDangerMoveToHomePoint::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStatePanic_HomePoint_Hide) {
		SStateDataMoveToPointEx data;

		data.vertex				= m_target_node;
		data.point				= ai().level_graph().vertex_position(data.vertex);
		data.action.action		= ACT_RUN;
		data.action.time_out	= 0;		// do not use time out
		data.completion_dist	= 1.f;		// get exactly to the point
		data.time_to_rebuild	= 0;		// do not rebuild
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type 		= eAT_Aggressive;
		data.action.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = this->object->db().m_dwAttackSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));
		return;
	}

	if (this->current_substate == eStatePanic_HomePoint_LookOpenPlace) {

		SStateDataLookToPoint	data;

		Fvector dir;
		this->object->CoverMan->less_cover_direction(dir);

		data.point.mad			(this->object->Position(),dir,10.f);
		data.action.action		= ACT_STAND_IDLE;
		data.action.time_out	= 2000;		
		data.action.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
		data.face_delay			= 0;

		state->fill_data_with(&data, sizeof(SStateDataLookToPoint));
		return;
	}

	if (this->current_substate == eStatePanic_HomePoint_Camp) {
		SStateDataAction data;

		data.action		= ACT_LOOK_AROUND;
		data.time_out	= 7000;			// do not use time out
		data.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.sound_delay = this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}

Fvector &CStateMonsterDangerMoveToHomePoint::get_most_danger_pos()
{	
	m_danger_pos.set(0,0,0);

	if (this->object->HitMemory.is_hit()) {
		m_danger_pos = this->object->HitMemory.get_last_hit_position();
	} else if (this->object->hear_dangerous_sound) {
		m_danger_pos = this->object->SoundMemory.GetSound().position;
	} 
	
	return m_danger_pos;
}

#undef 
#undef CStateMonsterDangerMoveToHomePoint

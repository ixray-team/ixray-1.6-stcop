#pragma once

#include "../states/state_move_to_point.h"
#include "../states/state_look_point.h"
#include "../states/state_custom_action.h"
#include "../../../cover_point.h"
#include "../monster_cover_manager.h"
#include "../monster_home.h"

#include "../../../actor.h"
#include "../../../actor_memory.h"
#include "../../../visual_memory_manager.h"

CStateBloodsuckerPredator::CStateBloodsuckerPredator(CAI_Bloodsucker*obj) : inherited(obj)
{
	add_state	(eStatePredator_MoveToCover,	xr_new<CStateMonsterMoveToPointEx >(obj));
	add_state	(eStatePredator_LookOpenPlace,	xr_new<CStateMonsterLookToPoint>	(obj));
	add_state	(eStatePredator_Camp,			xr_new<CStateMonsterCustomAction>	(obj));
}

void CStateBloodsuckerPredator::reinit()
{
	inherited::reinit	();
}

void CStateBloodsuckerPredator::initialize()
{
	inherited::initialize						();

	this->object->predator_start						();

	select_camp_point							();
}

void CStateBloodsuckerPredator::reselect_state()
{
	if (this->prev_substate == u32(-1)) {
		this->select_state(eStatePredator_MoveToCover);
		return;
	}
	
	if (this->prev_substate == eStatePredator_MoveToCover) {
		this->select_state(eStatePredator_LookOpenPlace);
		return;
	}

	if (this->prev_substate == eStatePredator_LookOpenPlace) {
		this->select_state(eStatePredator_Camp);
		return;
	}

	this->select_state(eStatePredator_Camp);
}

void CStateBloodsuckerPredator::finalize()
{
	inherited::finalize							();

	this->object->predator_stop						();
	this->object->predator_unfreeze					();

	CMonsterSquad *squad = monster_squad().get_squad(this->object);
	squad->unlock_cover(m_target_node);

}

void CStateBloodsuckerPredator::critical_finalize()
{
	inherited::critical_finalize				();

	this->object->predator_stop						();
	this->object->predator_unfreeze					();

	CMonsterSquad *squad = monster_squad().get_squad(this->object);
	squad->unlock_cover(m_target_node);
}

bool CStateBloodsuckerPredator::check_start_conditions()
{
	if (Actor()->memory().visual().visible_now(this->object)) return false;
	return true;
}

bool CStateBloodsuckerPredator::check_completion()
{
	if (this->object->HitMemory.get_last_hit_time() > this->time_state_started) return true;
	if (this->object->EnemyMan.get_enemy() && this->object->EnemyMan.see_enemy_now() && (this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position()) < 4.f)) return true;

	return false;
}

void CStateBloodsuckerPredator::setup_substates()
{
	state_ptr state = this->get_state_current();
	
	if (this->current_substate == eStatePredator_Camp) {
		this->object->predator_freeze	();
		m_time_start_camp		= time();

	} else {
		this->object->predator_unfreeze();
	}

	if (this->current_substate == eStatePredator_MoveToCover) {
		SStateDataMoveToPointEx data;

		data.vertex				= m_target_node;
		data.point				= ai().level_graph().vertex_position(data.vertex);
		data.action.action		= ACT_RUN;
		data.action.time_out	= 0;		// do not use time out
		data.completion_dist	= 0.f;		// get exactly to the point
		data.time_to_rebuild	= 0;		// do not rebuild
		data.accelerated		= true;
		data.braking			= true;
		data.accel_type 		= eAT_Aggressive;
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));
		return;
	}

	if (this->current_substate == eStatePredator_LookOpenPlace) {

		SStateDataLookToPoint	data;

		Fvector dir;
		this->object->CoverMan->less_cover_direction(dir);

		data.point.mad			(this->object->Position(),dir,10.f);
		data.action.action		= ACT_STAND_IDLE;
		data.action.time_out	= 2000;		
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
		data.face_delay			= 0;

		state->fill_data_with(&data, sizeof(SStateDataLookToPoint));
		return;
	}

	if (this->current_substate == eStatePredator_Camp) {
		
		SStateDataAction data;

		data.action		= ACT_STAND_IDLE;
		data.time_out	= 0;			// do not use time out
		data.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.sound_delay = this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}

#define TIME_TO_RESELECT_CAMP	15000

void CStateBloodsuckerPredator::check_force_state()
{
	if ((this->current_substate == eStatePredator_Camp) && (m_time_start_camp + TIME_TO_RESELECT_CAMP < time())) {
		if (this->current_substate != u32(-1))
			this->get_state_current()->critical_finalize();

		this->prev_substate		= u32(-1);
		this->current_substate	= u32(-1);

		CMonsterSquad *squad = monster_squad().get_squad(this->object);
		squad->unlock_cover	(m_target_node);

		select_camp_point	();
	}
}

void CStateBloodsuckerPredator::select_camp_point()
{
	m_target_node = u32(-1);
	if (this->object->Home->has_home()) {
		m_target_node							= this->object->Home->get_place_in_cover();
		if (m_target_node == u32(-1)) {
			m_target_node						= this->object->Home->get_place();
		}
	} 

	if (m_target_node == u32(-1)) {
		const CCoverPoint	*point = this->object->CoverMan->find_cover(this->object->Position(),10.f,30.f);
		if (point) {
			m_target_node				= point->level_vertex_id	();
		} 
	}

	if (m_target_node == u32(-1)) 
		m_target_node = this->object->ai_location().level_vertex_id();


	CMonsterSquad *squad = monster_squad().get_squad(this->object);
	squad->lock_cover(m_target_node);
}

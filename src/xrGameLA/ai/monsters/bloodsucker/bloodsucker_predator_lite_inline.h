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


#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateBloodsuckerPredatorLiteAbstract CStateBloodsuckerPredatorLite<_Object>

TEMPLATE_SPECIALIZATION
CStateBloodsuckerPredatorLiteAbstract::CStateBloodsuckerPredatorLite(_Object *obj) : inherited(obj)
{
	add_state	(eStatePredator_Camp, new CStateMonsterCustomAction<_Object>(obj));
	add_state	(eStatePredator_MoveToCover, new CStateMonsterMoveToPointEx<_Object>(obj));
	add_state	(eStatePredator_LookOpenPlace, new CStateMonsterLookToPoint<_Object>(obj));
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::reinit()
{
	inherited::reinit	();
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::initialize()
{
	inherited::initialize						();

	this->object->predator_start						();

	m_target_node								= u32(-1);
	m_freezed									= false;
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::reselect_state()
{
	if (this->prev_substate == u32(-1)) {
		if (enemy_see_me()) this->select_state(eStatePredator_MoveToCover);
		else this->select_state(eStatePredator_LookOpenPlace);
		return;
	}

	if (this->prev_substate == eStatePredator_MoveToCover) {
		if (enemy_see_me()) {
			this->select_state(eStatePredator_MoveToCover);
			this->object->set_berserk();
		}
		else this->select_state(eStatePredator_LookOpenPlace);
		return;
	}
	
	if (this->prev_substate == eStatePredator_LookOpenPlace) {
		this->select_state(eStatePredator_Camp);
		return;
	}

	if (this->prev_substate == eStatePredator_Camp) {
		this->select_state(eStatePredator_MoveToCover);
		return;
	}

	this->select_state(eStatePredator_MoveToCover);
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::finalize()
{
	inherited::finalize							();
	this->object->predator_stop						();
	if (m_freezed)	this->object->predator_unfreeze	();

	if (m_target_node != u32(-1)) 
		monster_squad().get_squad(this->object)->unlock_cover(m_target_node);
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::critical_finalize()
{
	inherited::critical_finalize				();
	this->object->predator_stop						();
	if (m_freezed)	this->object->predator_unfreeze	();

	if (m_target_node != u32(-1)) 
		monster_squad().get_squad(this->object)->unlock_cover(m_target_node);
}

TEMPLATE_SPECIALIZATION
bool CStateBloodsuckerPredatorLiteAbstract::check_completion()
{
	if (this->object->EnemyMan.see_enemy_now() && (this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position()) < 4.f)) {
		this->object->set_berserk();
		return true;
	}
	if (this->object->conditions().health() > 0.9f) return true;

	return false;
}


TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStatePredator_Camp) {
		this->object->predator_freeze		();
		m_freezed					= true;
	} else {
		this->object->predator_unfreeze	();
		m_freezed					= false;
	}

	if (this->current_substate == eStatePredator_MoveToCover) {
		select_camp_point		();
		
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

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::check_force_state()
{
	if (this->prev_substate == eStatePredator_Camp) {
		if (this->object->HitMemory.get_last_hit_time() > this->time_state_started) {
			
			if (this->object->EnemyMan.get_enemy() &&
				(this->object->EnemyMan.get_enemy()->Position().distance_to(this->object->Position()) < 10.f)) {
				this->object->set_berserk	();
			} else 
				this->current_substate	= u32(-1);
			
		}
	}
}

TEMPLATE_SPECIALIZATION
void CStateBloodsuckerPredatorLiteAbstract::select_camp_point()
{
	if (m_target_node != u32(-1)) 
		monster_squad().get_squad(this->object)->unlock_cover(m_target_node);
	
	m_target_node = u32(-1);
	if (this->object->Home->has_home()) {
		m_target_node							= this->object->Home->get_place_in_cover();
		if (m_target_node == u32(-1)) {
			m_target_node						= this->object->Home->get_place();
		}
	} 

	if (m_target_node == u32(-1)) {
		const CCoverPoint	*point = this->object->CoverMan->find_cover(this->object->Position(),20.f,30.f);
		if (point) {
			m_target_node				= point->level_vertex_id	();
		} 
	}

	if (m_target_node == u32(-1)) 
		m_target_node = this->object->ai_location().level_vertex_id();

	monster_squad().get_squad(this->object)->lock_cover(m_target_node);
}

TEMPLATE_SPECIALIZATION
bool CStateBloodsuckerPredatorLiteAbstract::enemy_see_me()
{
	//if (object->EnemyMan.get_enemy() == Actor()) 
	//	return (Actor()->memory().visual().visible_now(object));

	// if I see enemy then probably enemy see me :-)
	return this->object->EnemyMan.enemy_see_me_now();
}


#undef TEMPLATE_SPECIALIZATION
#undef CStateBloodsuckerPredatorLiteAbstract


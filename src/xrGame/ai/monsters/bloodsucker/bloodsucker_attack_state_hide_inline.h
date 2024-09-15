#pragma once

#pragma once

#include "../states/state_move_to_point.h"
#include "bloodsucker_predator_lite.h"

#include "../../../cover_point.h"
#include "../monster_cover_manager.h"
#include "../monster_home.h"

#include "../../../actor.h"
#include "../../../actor_memory.h"
#include "../../../visual_memory_manager.h"

CBloodsuckerStateAttackHide::CBloodsuckerStateAttackHide(CAI_Bloodsucker*obj) : inherited(obj)
{
	add_state	(eStateAttack_HideInCover,	xr_new<CStateMonsterMoveToPointEx>(obj));
	add_state	(eStateAttack_CampInCover,	xr_new<CStateBloodsuckerPredatorLite>	(obj));
}

void CBloodsuckerStateAttackHide::reinit()
{
	inherited::reinit	();
}

void CBloodsuckerStateAttackHide::initialize()
{
	inherited::initialize	();

	m_target_node			= u32(-1);

	object->start_invisible_predator();
}

void CBloodsuckerStateAttackHide::reselect_state()
{
	if (prev_substate == u32(-1)) {
		select_state(eStateAttack_HideInCover);
		return;
	}

	select_state(eStateAttack_CampInCover);
}

void CBloodsuckerStateAttackHide::finalize()
{
	inherited::finalize							();

	if (m_target_node != u32(-1))
		monster_squad().get_squad(object)->unlock_cover(m_target_node);
}

void CBloodsuckerStateAttackHide::critical_finalize()
{
	inherited::critical_finalize				();

	if (m_target_node != u32(-1))
		monster_squad().get_squad(object)->unlock_cover(m_target_node);
}

bool CBloodsuckerStateAttackHide::check_completion()
{
	if (current_substate == eStateAttack_CampInCover)
		return (get_state_current()->check_completion());

	return false;
}

void CBloodsuckerStateAttackHide::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStateAttack_HideInCover) {
		select_camp_point();

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
		data.action.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));
		return;
	}

}

void CBloodsuckerStateAttackHide::check_force_state()
{
}

void CBloodsuckerStateAttackHide::select_camp_point()
{
	if (m_target_node != u32(-1))
		monster_squad().get_squad(object)->unlock_cover(m_target_node);

	m_target_node = u32(-1);
	if (object->Home->has_home()) {
		m_target_node							= object->Home->get_place_in_cover();
		if (m_target_node == u32(-1)) {
			m_target_node						= object->Home->get_place();
		}
	} 

	if (m_target_node == u32(-1)) {
		const CCoverPoint	*point = object->CoverMan->find_cover(object->Position(),10.f,30.f);
		if (point) {
			m_target_node				= point->level_vertex_id	();
		} 
	}

	if (m_target_node == u32(-1)) 
		m_target_node = object->ai_location().level_vertex_id();


	monster_squad().get_squad(object)->lock_cover(m_target_node);
}
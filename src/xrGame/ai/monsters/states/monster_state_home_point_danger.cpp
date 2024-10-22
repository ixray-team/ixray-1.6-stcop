#include "StdAfx.h"
#include "monster_state_home_point_danger.h"


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


CStateMonsterDangerMoveToHomePoint::CStateMonsterDangerMoveToHomePoint(CBaseMonster* obj) : inherited(obj)
{
    add_state(eStatePanic_HomePoint_Hide, new CStateMonsterMoveToPointEx(obj));
    add_state(eStatePanic_HomePoint_LookOpenPlace, new CStateMonsterLookToPoint(obj));
    add_state(eStatePanic_HomePoint_Camp, new CStateMonsterCustomAction(obj));
}


//////////////////////////////////////////////////////////////////////////
// Initialize/Finalize
//////////////////////////////////////////////////////////////////////////


void CStateMonsterDangerMoveToHomePoint::initialize()
{
	inherited::initialize();

	// get the most danger position
	get_most_danger_pos();

	m_target_node = object->Home->get_place_in_cover();
	m_skip_camp = false;

	if (m_target_node == u32(-1)) {
		m_target_node = object->Home->get_place();
		m_skip_camp = true;
	}

	CMonsterSquad* squad = monster_squad().get_squad(object);
	squad->lock_cover(m_target_node);
}


void CStateMonsterDangerMoveToHomePoint::finalize()
{
	inherited::finalize();
	CMonsterSquad* squad = monster_squad().get_squad(object);
	squad->unlock_cover(m_target_node);
}


void CStateMonsterDangerMoveToHomePoint::critical_finalize()
{
	inherited::critical_finalize();

	CMonsterSquad* squad = monster_squad().get_squad(object);
	squad->unlock_cover(m_target_node);
}

//////////////////////////////////////////////////////////////////////////
// Check Start Conditions / Completion
//////////////////////////////////////////////////////////////////////////


bool CStateMonsterDangerMoveToHomePoint::check_start_conditions()
{
	return (!object->Home->at_home() && !object->Home->at_home(get_most_danger_pos()));
}


bool CStateMonsterDangerMoveToHomePoint::check_completion()
{
	if (object->HitMemory.get_last_hit_time() > time_state_started) return true;
	if (m_skip_camp && (prev_substate != u32(-1)) && (prev_substate != eStatePanic_HomePoint_Hide)) return true;

	return false;
}

//////////////////////////////////////////////////////////////////////////
// Select Substate
//////////////////////////////////////////////////////////////////////////


void CStateMonsterDangerMoveToHomePoint::reselect_state()
{
	if (prev_substate == u32(-1)) {
		select_state(eStatePanic_HomePoint_Hide);
		return;
	}

	if (prev_substate == eStatePanic_HomePoint_Hide) {
		select_state(eStatePanic_HomePoint_LookOpenPlace);
		return;
	}

	select_state(eStatePanic_HomePoint_Camp);
}

//////////////////////////////////////////////////////////////////////////
// Setup Substates
//////////////////////////////////////////////////////////////////////////


void CStateMonsterDangerMoveToHomePoint::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStatePanic_HomePoint_Hide) {
		SStateDataMoveToPointEx data;

		data.vertex = m_target_node;
		data.point = ai().level_graph().vertex_position(data.vertex);
		data.action.action = ACT_RUN;
		data.action.time_out = 0;		// do not use time out
		data.completion_dist = 1.f;		// get exactly to the point
		data.time_to_rebuild = 0;		// do not rebuild
		data.accelerated = true;
		data.braking = false;
		data.accel_type = eAT_Aggressive;
		data.action.sound_type = MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = object->db().m_dwAttackSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));
		return;
	}

	if (current_substate == eStatePanic_HomePoint_LookOpenPlace) {

		SStateDataLookToPoint	data;

		Fvector dir;
		object->CoverMan->less_cover_direction(dir);

		data.point.mad(object->Position(), dir, 10.f);
		data.action.action = ACT_STAND_IDLE;
		data.action.time_out = 2000;
		data.action.sound_type = MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;
		data.face_delay = 0;

		state->fill_data_with(&data, sizeof(SStateDataLookToPoint));
		return;
	}

	if (current_substate == eStatePanic_HomePoint_Camp) {
		SStateDataAction data;

		data.action = ACT_LOOK_AROUND;
		data.time_out = 7000;			// do not use time out
		data.sound_type = MonsterSound::eMonsterSoundAggressive;
		data.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}

Fvector& CStateMonsterDangerMoveToHomePoint::get_most_danger_pos()
{
	m_danger_pos.set(0, 0, 0);

	if (object->HitMemory.is_hit()) {
		m_danger_pos = object->HitMemory.get_last_hit_position();
	}
	else if (object->hear_dangerous_sound) {
		m_danger_pos = object->SoundMemory.GetSound().position;
	}

	return m_danger_pos;
}

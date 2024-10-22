#include "StdAfx.h"
#include "monster_state_attack_camp.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "state_move_to_point.h"
#include "state_look_point.h"
#include "monster_state_attack_camp_stealout.h"
#include "../../../cover_point.h"
#include "../monster_cover_manager.h"

CStateMonsterAttackCamp::CStateMonsterAttackCamp(CBaseMonster* obj) : inherited(obj)
{
    add_state(eStateAttackCamp_Hide, new CStateMonsterMoveToPointEx(obj));
    add_state(eStateAttackCamp_Camp, new CStateMonsterLookToPoint(obj));
    add_state(eStateAttackCamp_StealOut, new CStateMonsterAttackCampStealOut(obj));
}

void CStateMonsterAttackCamp::initialize()
{
	inherited::initialize();

	CMonsterSquad* squad = monster_squad().get_squad(object);
	squad->lock_cover(m_target_node);
}

void CStateMonsterAttackCamp::finalize()
{
	inherited::finalize();
	CMonsterSquad* squad = monster_squad().get_squad(object);
	squad->unlock_cover(m_target_node);
}

void CStateMonsterAttackCamp::critical_finalize()
{
	inherited::critical_finalize();

	CMonsterSquad* squad = monster_squad().get_squad(object);
	squad->unlock_cover(m_target_node);
}

bool CStateMonsterAttackCamp::check_completion()
{
	if (current_substate == eStateAttackCamp_StealOut) {
		return get_state_current()->check_completion();
	}

	if (current_substate == eStateAttackCamp_Camp) {
		if (object->EnemyMan.see_enemy_now()) return true;
		if (object->HitMemory.get_last_hit_time() > get_state_current()->time_started()) return true;
	}

	if (object->EnemyMan.get_enemy()->Position().distance_to(object->Position()) < 5.f) return true;

	return false;
}

#define MIN_DISTANCE_TO_ENEMY 20.f

bool CStateMonsterAttackCamp::check_start_conditions()
{
	if (!object->ability_distant_feel()) return false;

	// check enemy
	if (!object->EnemyMan.get_enemy()) return false;

	// check distance to enemy
	if (object->Position().distance_to(object->EnemyMan.get_enemy_position()) < MIN_DISTANCE_TO_ENEMY) return false;

	// check if enemy see me
	//if (EnemyMan.get_flags().is(FLAG_ENEMY_DOESNT_SEE_ME)) return false;

	// try to get cover
	const CCoverPoint* point = object->CoverMan->find_cover(object->EnemyMan.get_enemy_position(), 10.f, 30.f);
	if (!point) return false;

	m_target_node = point->level_vertex_id();

	return true;
}

void CStateMonsterAttackCamp::reselect_state()
{
	if (prev_substate == u32(-1)) {
		select_state(eStateAttackCamp_Hide);
		return;
	}

	if (prev_substate == eStateAttackCamp_Hide) {
		select_state(eStateAttackCamp_Camp);
		return;
	}

	if (prev_substate == eStateAttackCamp_Camp) {
		if (get_state(eStateAttackCamp_StealOut)->check_start_conditions())
			select_state(eStateAttackCamp_StealOut);
		else
			select_state(eStateAttackCamp_Hide);
		return;
	}

	if (prev_substate == eStateAttackCamp_StealOut) {
		select_state(eStateAttackCamp_Camp);
		return;
	}
}

void CStateMonsterAttackCamp::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStateAttackCamp_Hide) {
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
		data.action.sound_type = MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));
		return;
	}

	if (current_substate == eStateAttackCamp_Camp) {

		SStateDataLookToPoint	data;

		Fvector dir;
		object->CoverMan->less_cover_direction(dir);

		data.point.mad(object->Position(), dir, 10.f);
		data.action.action = ACT_STAND_IDLE;
		data.action.time_out = 10000;		// do not use time out
		data.action.sound_type = MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;
		data.face_delay = 0;

		state->fill_data_with(&data, sizeof(SStateDataLookToPoint));
		return;
	}
}

void CStateMonsterAttackCamp::check_force_state()
{

}

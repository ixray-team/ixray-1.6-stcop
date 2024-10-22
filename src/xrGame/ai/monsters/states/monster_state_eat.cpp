#include "StdAfx.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "../monster_home.h"
#include "monster_state_eat.h"

#include "state_data.h"
#include "state_move_to_point.h"
#include "state_hide_from_point.h"
#include "state_custom_action.h"
#include "monster_state_eat_eat.h"
#include "monster_state_eat_drag.h"
#include "../../../../xrPhysics/PhysicsShell.h"
#include "../../../PHMovementControl.h"
#include "../../../CharacterPhysicsSupport.h"


CStateMonsterEat::CStateMonsterEat(CBaseMonster* obj) : inherited(obj)
{
    add_state(eStateEat_CorpseApproachRun, new CStateMonsterMoveToPoint(obj));
    add_state(eStateEat_CorpseApproachWalk, new CStateMonsterMoveToPoint(obj));
    add_state(eStateEat_CheckCorpse, new CStateMonsterCustomAction(obj));
    add_state(eStateEat_Eat, new CStateMonsterEating(obj));
    add_state(eStateEat_WalkAway, new CStateMonsterHideFromPoint(obj));
    add_state(eStateEat_Rest, new CStateMonsterCustomAction(obj));
    add_state(eStateEat_Drag, new CStateMonsterDrag(obj));
}



CStateMonsterEat::~CStateMonsterEat()
{
}


void CStateMonsterEat::reinit()
{
	inherited::reinit();

	m_time_last_eat = 0;
}


void CStateMonsterEat::initialize()
{
	inherited::initialize();
	corpse = object->CorpseMan.get_corpse();

	monster_squad().get_squad(object)->lock_corpse(object->CorpseMan.get_corpse());
}


void CStateMonsterEat::finalize()
{
	inherited::finalize();

	monster_squad().get_squad(object)->unlock_corpse(object->CorpseMan.get_corpse());
}


void CStateMonsterEat::critical_finalize()
{
	inherited::critical_finalize();

	monster_squad().get_squad(object)->unlock_corpse(object->CorpseMan.get_corpse());
}



void CStateMonsterEat::reselect_state()
{
	if (prev_substate == u32(-1)) { select_state(eStateEat_CorpseApproachRun); return; }
	if (prev_substate == eStateEat_CorpseApproachRun) { select_state(eStateEat_CheckCorpse); return; }

	if (prev_substate == eStateEat_CheckCorpse) {
		if (object->ability_can_drag())
		{
			select_state(eStateEat_Drag);
		}
		else
		{
			if (get_state(eStateEat_Eat)->check_start_conditions())
				select_state(eStateEat_Eat);
			else
				select_state(eStateEat_CorpseApproachWalk);
		}
		return;
	}

	if (prev_substate == eStateEat_Drag) {
		if (get_state(eStateEat_Eat)->check_start_conditions())
			select_state(eStateEat_Eat);
		else
			select_state(eStateEat_CorpseApproachWalk);
		return;
	}

	if (prev_substate == eStateEat_Eat) {
		m_time_last_eat = time();

		if (!hungry())
			select_state(eStateEat_WalkAway);
		else
			select_state(eStateEat_CorpseApproachWalk);
		return;
	}

	if (prev_substate == eStateEat_CorpseApproachWalk) {
		if (get_state(eStateEat_Eat)->check_start_conditions())
			select_state(eStateEat_Eat);
		else
			select_state(eStateEat_CorpseApproachWalk);
		return;
	}

	if (prev_substate == eStateEat_WalkAway) { select_state(eStateEat_Rest);		return; }
	if (prev_substate == eStateEat_Rest) { select_state(eStateEat_Rest);		return; }
}


void CStateMonsterEat::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStateEat_CorpseApproachRun) {

		// Определить позицию ближайшей боны у трупа
		Fvector nearest_bone_pos;
		const CEntityAlive* corpse_ = object->CorpseMan.get_corpse();
		if ((corpse_->m_pPhysicsShell == NULL) || (!corpse_->m_pPhysicsShell->isActive())) {
			nearest_bone_pos = corpse_->Position();
		}
		else nearest_bone_pos = object->character_physics_support()->movement()->PHCaptureGetNearestElemPos(corpse_);

#ifdef _DEBUG
		DBG().level_info(this).clear();
		Fvector pos1;
		pos1.set(nearest_bone_pos);
		pos1.y += 20.f;

		DBG().level_info(this).add_item(nearest_bone_pos, pos1, COLOR_GREEN);
#endif
		SStateDataMoveToPoint data;
		data.point = nearest_bone_pos;
		data.vertex = u32(-1);
		data.action.action = ACT_RUN;
		data.accelerated = true;
		data.braking = true;
		data.accel_type = eAT_Calm;
		data.completion_dist = object->db().m_fDistToCorpse;
		data.action.sound_type = MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPoint));
		return;
	}

	if (current_substate == eStateEat_CheckCorpse) {
		SStateDataAction data;
		data.action = ACT_STAND_IDLE;
		data.spec_params = 0;
		data.time_out = 1500;
		data.sound_type = MonsterSound::eMonsterSoundEat;
		data.sound_delay = object->db().m_dwEatSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}

	if (current_substate == eStateEat_WalkAway) {
		SStateHideFromPoint data;

		data.point = object->CorpseMan.get_corpse_position();
		data.action.action = ACT_WALK_FWD;
		data.distance = 15.f;
		data.accelerated = true;
		data.braking = true;
		data.accel_type = eAT_Calm;
		data.cover_min_dist = 20.f;
		data.cover_max_dist = 30.f;
		data.cover_search_radius = 25.f;
		data.action.sound_type = MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateHideFromPoint));

		return;
	}

	if (current_substate == eStateEat_Rest) {
		SStateDataAction data;
		data.action = ACT_REST;
		data.spec_params = 0;
		data.time_out = 8500;
		data.sound_type = MonsterSound::eMonsterSoundIdle;
		data.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));
		return;
	}

	if (current_substate == eStateEat_CorpseApproachWalk) {

		// Определить позицию ближайшей боны у трупа
		Fvector nearest_bone_pos;
		const CEntityAlive* corpse_ = object->CorpseMan.get_corpse();
		if ((corpse_->m_pPhysicsShell == NULL) || (!corpse_->m_pPhysicsShell->isActive())) {
			nearest_bone_pos = corpse_->Position();
		}
		else nearest_bone_pos = object->character_physics_support()->movement()->PHCaptureGetNearestElemPos(corpse_);

		SStateDataMoveToPoint data;
		data.point = nearest_bone_pos;
		data.vertex = u32(-1);
		data.action.action = ACT_WALK_FWD;
		data.accelerated = true;
		data.braking = true;
		data.accel_type = eAT_Calm;
		data.completion_dist = object->db().m_fDistToCorpse;
		data.action.sound_type = MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPoint));
		return;
	}
}


bool CStateMonsterEat::check_completion()
{
	if (corpse != object->CorpseMan.get_corpse()) return true;
	if (!hungry()) return true;

	return false;
}


bool CStateMonsterEat::check_start_conditions()
{
	return (
		object->CorpseMan.get_corpse() &&
		object->Home->at_home(object->CorpseMan.get_corpse()->Position()) &&
		hungry() &&
		!monster_squad().get_squad(object)->is_locked_corpse(object->CorpseMan.get_corpse())
		);

}

#define TIME_NOT_HUNGRY 20000


bool CStateMonsterEat::hungry()
{
	return ((m_time_last_eat == 0) || (m_time_last_eat + TIME_NOT_HUNGRY < time()));
}


void CStateMonsterEat::remove_links(CObject* object_)
{
	if (corpse == object_)
		corpse = 0;
}

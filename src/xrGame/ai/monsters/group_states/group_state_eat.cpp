#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "../monster_home.h"

#include "../dog/dog.h"

#include "../states/state_data.h"
#include "../states/state_move_to_point.h"
#include "../states/state_hide_from_point.h"
#include "../states/state_custom_action.h"
#include "../../../../xrPhysics/PhysicsShell.h"
#include "../../../PHMovementControl.h"
#include "../../../CharacterPhysicsSupport.h"
#include "group_state_eat_drag.h"
#include "group_state_custom.h"
#include "group_state_eat.h"
#include "group_state_eat_eat.h "

#define TIME_NOT_HUNGRY 20000

CStateGroupEat::CStateGroupEat(CBaseMonster* object) : inherited(object)
{
	m_pDog = smart_cast<CDogBase*>(object);

	corpse = nullptr;

	add_state(eStateEat_CorpseApproachRun, new CStateMonsterMoveToPoint (object));
	add_state(eStateEat_CorpseApproachWalk, new CStateMonsterMoveToPoint(object));
	add_state(eStateEat_CheckCorpse, new CStateMonsterCustomAction(object));
	add_state(eStateEat_Eat, new CStateGroupEating(object));
	add_state(eStateEat_WalkAway, new CStateMonsterHideFromPoint (object));
	add_state(eStateEat_Rest, new CStateMonsterCustomAction (object));
	add_state(eStateEat_Drag, new CStateGroupDrag (object));
	add_state(eStateCustom, new CStateCustomGroup (object));
}

CStateGroupEat::~CStateGroupEat()
{
}


void CStateGroupEat::reinit()
{
	inherited::reinit();

	m_time_last_eat = 0;
}


void CStateGroupEat::initialize()
{
	inherited::initialize();
	corpse = object->EatedCorpse;
}


void CStateGroupEat::finalize()
{
	inherited::finalize();

	if ((corpse == object->EatedCorpse) && object->EatedCorpse)
	{
		const_cast<CEntityAlive*>(object->EatedCorpse)->m_use_timeout = m_pDog->m_corpse_use_timeout;
		const_cast<CEntityAlive*>(object->EatedCorpse)->set_lock_corpse(false);
	}
	if (object->character_physics_support()->movement()->PHCapture())
		object->character_physics_support()->movement()->PHReleaseObject();
	object->EatedCorpse = NULL;
	m_pDog->b_end_state_eat = true;
}


void CStateGroupEat::critical_finalize()
{
	inherited::critical_finalize();
	if ((corpse == object->EatedCorpse) && object->EatedCorpse && check_completion())
	{
		if (object->character_physics_support()->movement()->PHCapture())
			object->character_physics_support()->movement()->PHReleaseObject();
		const_cast<CEntityAlive*>(object->EatedCorpse)->m_use_timeout = m_pDog->m_corpse_use_timeout;
		const_cast<CEntityAlive*>(object->EatedCorpse)->set_lock_corpse(false);
		object->EatedCorpse = NULL;
		m_pDog->b_end_state_eat = true;
	}
	if (object->EnemyMan.get_enemy())
		if (object->character_physics_support()->movement()->PHCapture())
			object->character_physics_support()->movement()->PHReleaseObject();
	object->EatedCorpse = NULL;
	m_pDog->b_end_state_eat = true;
}



void CStateGroupEat::reselect_state()
{
	if (m_pDog->b_state_check)
	{
		select_state(eStateCustom);
		m_pDog->b_state_check = false;
		m_time_last_eat = time() + TIME_NOT_HUNGRY;
		return;
	}

	if (m_pDog->saved_state == eStateEat_Eat)
	{
		m_pDog->saved_state = u32(-1);
		if (object->character_physics_support()->movement()->PHCapture())
			object->character_physics_support()->movement()->PHReleaseObject();
		select_state(eStateEat_Eat);
		return;
	}

	if (prev_substate == u32(-1))
	{
		select_state(eStateEat_CorpseApproachWalk);
		return;
	}

	if (prev_substate == eStateEat_CorpseApproachWalk)
	{
		if (!get_state(eStateEat_CorpseApproachWalk)->check_completion())
		{
			select_state(eStateEat_CorpseApproachWalk);
			return;
		}
		// Lain: added
		if (object->ability_can_drag() && object->check_eated_corpse_draggable())
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

	if (prev_substate == eStateEat_Drag)
	{
		if (!get_state(eStateEat_Drag)->check_completion())
		{
			select_state(eStateEat_Drag);
			return;
		}

		if (get_state(eStateEat_Eat)->check_start_conditions())
		{
			m_pDog->set_current_animation(15);
			m_pDog->saved_state = eStateEat_Eat;
			select_state(eStateCustom);
			m_pDog->b_state_check = false;
		}
		else
		{
			select_state(eStateEat_CorpseApproachWalk);
		}
		return;
	}

	if (prev_substate == eStateEat_Eat)
	{
		m_time_last_eat = time();

		if (!hungry())
			select_state(eStateEat_WalkAway);
		else
			select_state(eStateEat_CorpseApproachWalk);
		return;
	}

	if (prev_substate == eStateEat_WalkAway)
	{
		select_state(eStateEat_Rest);
		return;
	}

	if (prev_substate == eStateEat_Rest)
	{
		select_state(eStateEat_Rest);
		return;
	}

	select_state(eStateEat_Rest);
}


void CStateGroupEat::setup_substates()
{
	state_ptr state = get_state_current();

	if (current_substate == eStateEat_CorpseApproachRun) {

		// Определить позицию ближайшей боны у трупа
		Fvector nearest_bone_pos;
		const CEntityAlive* corpse_ = object->EatedCorpse;
		if ((corpse_->m_pPhysicsShell == NULL) || (!corpse_->m_pPhysicsShell->isActive())) {
			nearest_bone_pos = corpse_->Position();
		}
		else nearest_bone_pos = object->character_physics_support()->movement()->PHCaptureGetNearestElemPos(corpse_);

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
		data.time_out = 500;
		data.sound_type = MonsterSound::eMonsterSoundEat;
		data.sound_delay = object->db().m_dwEatSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}

	if (current_substate == eStateEat_WalkAway) {
		SStateHideFromPoint data;

		data.point = object->EatedCorpse->Position();
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
		data.action = ACT_STAND_IDLE;
		data.spec_params = 0;
		data.time_out = 500;
		data.sound_type = MonsterSound::eMonsterSoundIdle;
		data.sound_delay = object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}

	if (current_substate == eStateEat_CorpseApproachWalk) {

		// Определить позицию ближайшей боны у трупа
		Fvector nearest_bone_pos;
		const CEntityAlive* corpse_ = object->EatedCorpse;

#ifdef DEBUG
		if (!corpse_)
		{
			debug::text_tree tree;
			object->add_debug_info(tree);
			debug::log_text_tree(tree);
			FATAL("Debug info has been added, plz save log");
		}
#endif //#ifdef DEBUG

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


bool CStateGroupEat::check_completion()
{
	if (corpse != object->EatedCorpse) return true;
	if (!hungry()) return true;

	return false;
}


bool CStateGroupEat::check_start_conditions()
{
	if (object->EatedCorpse) return true;
	return (
		object->CorpseMan.get_corpse() &&
		m_pDog->Home->at_home(object->CorpseMan.get_corpse()->Position()) &&
		hungry() &&
		!const_cast<CEntityAlive*>(object->CorpseMan.get_corpse())->is_locked_corpse()
		);

}


bool CStateGroupEat::hungry()
{
	return ((m_time_last_eat == 0) || (m_time_last_eat + TIME_NOT_HUNGRY < time()));
}


void CStateGroupEat::remove_links(CObject* object_)
{
	if (corpse == object_)
		corpse = 0;
}

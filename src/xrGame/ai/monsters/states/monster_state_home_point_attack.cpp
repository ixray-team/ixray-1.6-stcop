#include "StdAfx.h"

#include "ai_object_location.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "monster_state_home_point_attack.h"


#include "state_move_to_point.h"
#include "state_look_point.h"
#include "state_custom_action.h"
#include "../../../cover_point.h"
#include "../monster_cover_manager.h"
#include "../monster_home.h"
#include "monster_state_attack_on_run.h"

//////////////////////////////////////////////////////////////////////////
// Construct Substates
//////////////////////////////////////////////////////////////////////////


CStateMonsterAttackMoveToHomePoint::CStateMonsterAttackMoveToHomePoint(CBaseMonster* obj)
	: inherited(obj)
{
}

//////////////////////////////////////////////////////////////////////////
// Initialize/Finalize
//////////////////////////////////////////////////////////////////////////


void CStateMonsterAttackMoveToHomePoint::select_target()
{
	CMonsterSquad* const squad = monster_squad().get_squad(object);
	u32 const self_node = object->ai_location().level_vertex_id();

	if (m_target_node != u32(-1))
		squad->unlock_cover(m_target_node);

	for (u32 i = 0; i < 5; ++i)
	{
		m_target_node = object->Home->get_place_in_cover();
		if (m_target_node != self_node)
			break;
		m_target_node = u32(-1);
	}

	if (m_target_node == u32(-1))
	{
		for (u32 i = 0; i < 5; ++i)
		{
			m_target_node = object->Home->get_place();
			if (m_target_node != self_node)
				break;
			m_target_node = u32(-1);
		}
	}

	m_selected_target_time = current_time();

	if (m_target_node == u32(-1))
		object->control().path_builder().get_node_in_radius(self_node, 5, 25, 10, m_target_node);

	if (m_target_node != u32(-1))
	{
		m_target_pos = ai().level_graph().vertex_position(m_target_node);
		squad->lock_cover(m_target_node);
	}
}


void CStateMonsterAttackMoveToHomePoint::initialize()
{
	inherited::initialize();

	m_selected_target_time = 0;
	m_target_node = u32(-1);
	m_target_pos = object->Position();
	select_target();
}


void CStateMonsterAttackMoveToHomePoint::execute()
{
	if (m_target_node == u32(-1))
	{
		if (current_time() > m_selected_target_time + 500)
			select_target();
	}
	else
	{
		float const dist_to_target = object->Position().distance_to_xz(m_target_pos);
		if (dist_to_target < 2)
			select_target();
	}

	if (m_target_node == u32(-1))
	{
		object->set_action(ACT_STAND_IDLE);
		object->path().set_target_point(object->EnemyMan.get_enemy()->Position(),
			object->EnemyMan.get_enemy()->ai_location().level_vertex_id());
	}
	else
	{
		object->set_action(ACT_RUN);
		object->path().set_target_point(m_target_pos, m_target_node);
	}

	object->path().set_rebuild_time(250);
	object->path().set_distance_to_end(1);
	object->path().set_use_covers();
	object->path().set_cover_params(5.f, 30.f, 1.f, 30.f);
	object->path().set_use_dest_orient(false);

	object->anim().accel_activate(eAT_Aggressive);
	object->anim().accel_set_braking(false);

	object->set_state_sound(MonsterSound::eMonsterSoundAggressive,
		object->db().m_dwAttackSndDelay == u32(-1));
}


void CStateMonsterAttackMoveToHomePoint::finalize()
{
	inherited::finalize();
	clean();
}


void CStateMonsterAttackMoveToHomePoint::clean()
{
	inherited::finalize();

	if (m_target_node != u32(-1))
	{
		CMonsterSquad* squad = monster_squad().get_squad(object);
		squad->unlock_cover(m_target_node);
	}
}


void CStateMonsterAttackMoveToHomePoint::critical_finalize()
{
	inherited::critical_finalize();
	clean();
}

//////////////////////////////////////////////////////////////////////////
// Check Start Conditions / Completion
//////////////////////////////////////////////////////////////////////////



bool CStateMonsterAttackMoveToHomePoint::check_start_conditions()
{
	if (!object->at_home())
		return									true;

	if (!object->run_home_point_when_enemy_inaccessible())
		return									false;

	if (!object->enemy_accessible())
		return									true;

	return										false;
}


bool CStateMonsterAttackMoveToHomePoint::check_completion()
{
	if (!object->at_home())
		return									false;

	if (object->run_home_point_when_enemy_inaccessible())
	{
		if (!object->enemy_accessible())
			return								false;
	}

	return										true;
}


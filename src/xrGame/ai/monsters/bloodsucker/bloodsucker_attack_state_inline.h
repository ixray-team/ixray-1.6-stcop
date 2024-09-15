#pragma once

//#include "bloodsucker_attack_state_hide.h"
#include "../states/state_move_to_point.h"

CBloodsuckerStateAttack::CBloodsuckerStateAttack(CAI_Bloodsucker*obj) : inherited_attack(obj)
{
	add_state	(eStateAttack_Hide,	xr_new<CStateMonsterBackstubEnemy>(obj));
	add_state	(eStateVampire_Execute,	xr_new<CStateBloodsuckerVampireExecute>(obj));
}

CBloodsuckerStateAttack::~CBloodsuckerStateAttack()
{
}

void CBloodsuckerStateAttack::initialize()
{
	__super::initialize	();
	m_time_stop_invis		= 0;
	m_last_health           = this->object->conditions().GetHealth();
}

void CBloodsuckerStateAttack::finalize()
{
	__super::finalize();
	this->object->start_invisible_predator();
}

void CBloodsuckerStateAttack::critical_finalize()
{
	__super::critical_finalize();
	this->object->start_invisible_predator();
}

namespace detail
{

namespace bloodsucker
{
	const u32   encircle_time               = 3000;
	const float loose_health_diff           = 0.15f;
	const u32   change_behaviour_time       = 1000;

} // namespace bloodsucker

} // namespace detail

void CBloodsuckerStateAttack::execute()
{
	if (this->check_home_point() )				this->select_state(eStateAttack_MoveToHomePoint);
	else if ( check_vampire() )				this->select_state(eStateVampire_Execute);
	else if ( this->check_steal_state() )			this->select_state(eStateAttack_Steal);
	else if ( this->check_camp_state() )			this->select_state(eStateAttackCamp);
	else if ( this->check_find_enemy_state() )	this->select_state(eStateAttack_FindEnemy);
	else if ( check_hiding() )				this->select_state(eStateAttack_Hide);
	else if (this->check_run_attack_state() )	this->select_state(eStateAttack_RunAttack);
	else
	{
		// определить тип атаки
		bool b_melee = false; 

		if (this->prev_substate == eStateAttack_Melee )
		{
			if ( !this->get_state_current()->check_completion() )
			{
				b_melee = true;
			}
		} 
		else if (this->get_state(eStateAttack_Melee)->check_start_conditions() )
		{
			b_melee = true;
		}
		
 		if ( !b_melee && (this->prev_substate == eStateAttack_Melee) )
 		{
			this->select_state(eStateAttack_Hide);
 		}
 		else
		// установить целевое состояние
		if ( b_melee ) 
		{  
			// check if enemy is behind me for a long time
			// [TODO] make specific state and replace run_away state (to avoid ratation jumps)
			//if (check_behinder()) 
			//	select_state(eStateAttack_RunAway);
			//else 
			this->select_state(eStateAttack_Melee);
		}
		else 
		{
			this->select_state(eStateAttack_Run);
		}
	}

	// clear behinder var if not melee state selected
	if (this->current_substate != eStateAttack_Melee )
	{
		this->m_time_start_check_behinder = 0;
	}
	else
	{
		this->object->clear_runaway_invisible();
	}
	
	this->get_state_current()->execute();
	this->prev_substate = this->current_substate;

	// Notify squad	
	CMonsterSquad* squad = monster_squad().get_squad(this->object);
	if ( squad )
	{
		SMemberGoal goal;

		goal.type	= MG_AttackEnemy;
		goal.entity	= const_cast<CEntityAlive*>(this->object->EnemyMan.get_enemy());

		squad->UpdateGoal(this->object, goal);
	}
}

bool CBloodsuckerStateAttack::check_vampire()
{
	if (this->prev_substate != eStateVampire_Execute )
	{
		if (this->get_state(eStateVampire_Execute)->check_start_conditions())	return true;
	} 
	else
	{
		if (!this->get_state(eStateVampire_Execute)->check_completion())		return true;
	}
	return false;
}

bool CBloodsuckerStateAttack::check_hiding()
{
	const bool health_step_lost = this->object->conditions().GetHealth() < 
		                          m_last_health-::detail::bloodsucker::loose_health_diff;

	if ( health_step_lost )
	{
		this->object->start_runaway_invisible();
		m_last_health = this->object->conditions().GetHealth();
		m_start_with_encircle = true;
		return true;
	}

	// if we get here before 1 sec after last critical hit: 
	u32 last_critical_hit_tick = this->object->get_last_critical_hit_tick();
	if ( last_critical_hit_tick && time() < last_critical_hit_tick + 1000 )
	{
		this->object->clear_last_critical_hit_tick();
		m_start_with_encircle = true;
		return true;
	}

	if (this->current_substate == eStateAttack_Hide )
	{
		return !this->get_state_current()->check_completion();
	}

	m_start_with_encircle = false;
	return this->get_state(eStateAttack_Hide)->check_start_conditions();
}

void CBloodsuckerStateAttack::setup_substates()
{
	auto state = this->get_state_current();

	if (this->current_substate == eStateAttack_Hide )
	{
		typename CStateMonsterBackstubEnemy::StateParams data;
		
		data.action.action		= ACT_RUN;
		data.action.time_out	= 0;
		data.completion_dist	= 1.f;		// get exactly to the point
		data.time_to_rebuild	= 200;		
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type 		= eAT_Aggressive;
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;
		data.start_with_encircle = m_start_with_encircle;

		state->fill_data_with(&data, sizeof(CStateMonsterBackstubEnemy::StateParams));
		return;
	}
}

//////////////////////////////////////////////////////////////////////////
// CStateMonsterMoveToPointEx with path rebuild options
//////////////////////////////////////////////////////////////////////////

void   CStateMonsterBackstubEnemy::initialize ()
{
	inherited::initialize();
	this->object->path().prepare_builder();
	m_last_health = this->object->conditions().GetHealth();
	m_encircle = data.start_with_encircle;
	m_encircle_end_tick = Device.dwTimeGlobal + ::detail::bloodsucker::encircle_time;
	m_next_change_behaviour_tick = 0;
}

void   CStateMonsterBackstubEnemy::execute ()
{
	// on hit, change behaviour
	if ( this->object->conditions().GetHealth() < m_last_health- ::detail::bloodsucker::loose_health_diff &&
		 Device.dwTimeGlobal > m_next_change_behaviour_tick )
	{
		m_next_change_behaviour_tick = Device.dwTimeGlobal + ::detail::bloodsucker::change_behaviour_time;
		m_last_health = this->object->conditions().GetHealth();
		m_encircle = !m_encircle;
		if ( m_encircle )
		{
			m_encircle_end_tick = Device.dwTimeGlobal + ::detail::bloodsucker::encircle_time;
		}
	}
	
	if ( Device.dwTimeGlobal > m_encircle_end_tick ) 
	{
		if ( this->object->EnemyMan.enemy_see_me_now() )
		{
			m_encircle = false;
		}
	} 

	this->object->set_action(data.action.action);
	this->object->anim().SetSpecParams(data.action.spec_params);

	data.point  = this->object->EnemyMan.get_enemy_position();
	data.vertex = 0;

	data.target_direction = Fvector().set(0.f, 0.f, 0.f);
	const CEntityAlive* enemy = this->object->EnemyMan.get_enemy();
	VERIFY(enemy);

	const SRotation rot = enemy->Orientation();
	data.target_direction.setHP(rot.yaw, rot.pitch);

	this->object->path().set_target_point		(data.point, data.vertex);
	this->object->path().set_rebuild_time		(data.time_to_rebuild);
	this->object->path().set_distance_to_end	(data.completion_dist);
	this->object->path().set_use_covers		();
	this->object->path().set_cover_params		(5.f, 30.f, 1.f, 30.f);

	if ( m_encircle )
	{
		this->object->path().set_use_dest_orient (true);
		this->object->path().set_dest_direction  (data.target_direction);
		this->object->path().set_try_min_time    (false);
	}
	else
	{
		this->object->path().set_try_min_time    (true);
		this->object->path().set_use_dest_orient (false);
	}

	if ( data.accelerated ) 
	{
		this->object->anim().accel_activate	 (EAccelType(data.accel_type));
		this->object->anim().accel_set_braking (data.braking);
	}

	if ( data.action.sound_type != u32(-1) )
	{
		this->object->set_state_sound(data.action.sound_type, data.action.sound_delay == u32(-1));
	}
}

bool   CStateMonsterBackstubEnemy::check_start_conditions ()
{
	if ( !this->object->Home->at_home(this->object->EnemyMan.get_enemy_position()) )
	{
		return false;
	}

	float dist = this->object->MeleeChecker.distance_to_enemy	(this->object->EnemyMan.get_enemy());

	return dist > this->object->MeleeChecker.get_min_distance();
}

bool   CStateMonsterBackstubEnemy::check_completion ()
{	
	if ( !this->object->Home->at_home(this->object->EnemyMan.get_enemy_position()) )
	{
		return true;
	}

	const bool real_path_end = fis_zero(data.completion_dist) ? 
		   (data.point.distance_to_xz(this->object->Position()) < ai().level_graph().header().cell_size()) : true;

	if ( this->object->control().path_builder().is_path_end(data.completion_dist) && real_path_end ) 
	{
		// in straight-mode we're done
		if ( !m_encircle )
		{
			return true;
		}

		if ( this->object->EnemyMan.see_enemy_now() && !this->object->EnemyMan.enemy_see_me_now() )
		{
			// this->object->sound().play(MonsterSound::eMonsterSoundSteal);
			return true;
		}
	}

	return false;
}


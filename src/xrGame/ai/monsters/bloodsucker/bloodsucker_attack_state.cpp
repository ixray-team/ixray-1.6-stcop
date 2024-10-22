#include "stdafx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "bloodsucker.h"
#include "bloodsucker_attack_state.h"
#include "bloodsucker_vampire_execute.h"

#include "../states/state_move_to_point.h"

#include "bloodsucker_backstub_enemy.h"

CustomBloodsuckerStateAttack::CustomBloodsuckerStateAttack(CBloodsuckerBase* object) : inherited_attack(object)
{
	m_dir_point = {};
	m_last_health = 0;
	m_start_with_encircle = false;
	m_time_stop_invis = 0;

	m_pBloodsucker = smart_cast<CBloodsuckerBase*>(object);

	add_state(eStateAttack_Hide, new CustomBloodsuckerBackstubEnemy(object));
	add_state(eStateVampire_Execute, new CustomBloodsuckerStateVampireExecute(object));
}

CustomBloodsuckerStateAttack::~CustomBloodsuckerStateAttack()
{

}

void CustomBloodsuckerStateAttack::initialize()
{
	inherited::initialize();
	m_time_stop_invis = 0;
	m_last_health = object->conditions().GetHealth();
}

void CustomBloodsuckerStateAttack::finalize()
{
	inherited::finalize();
	m_pBloodsucker->start_invisible_predator();
}

void CustomBloodsuckerStateAttack::critical_finalize()
{
	inherited::critical_finalize();
	m_pBloodsucker->start_invisible_predator();
}

void CustomBloodsuckerStateAttack::execute()
{
	if (check_home_point())				select_state(eStateAttack_MoveToHomePoint);
	else if (check_vampire())				select_state(eStateVampire_Execute);
	else if (check_steal_state())			select_state(eStateAttack_Steal);
	else if (check_camp_state())			select_state(eStateAttackCamp);
	else if (check_find_enemy_state())	select_state(eStateAttack_FindEnemy);
	else if (check_hiding())				select_state(eStateAttack_Hide);
	else if (check_run_attack_state())	select_state(eStateAttack_RunAttack);
	else
	{
		// определить тип атаки
		bool b_melee = false;

		if (prev_substate == eStateAttack_Melee)
		{
			if (!get_state_current()->check_completion())
			{
				b_melee = true;
			}
		}
		else if (get_state(eStateAttack_Melee)->check_start_conditions())
		{
			b_melee = true;
		}

		if (!b_melee && (prev_substate == eStateAttack_Melee))
		{
			select_state(eStateAttack_Hide);
		}
		else
			// установить целевое состояние
			if (b_melee)
			{
				// check if enemy is behind me for a long time
				// [TODO] make specific state and replace run_away state (to avoid ratation jumps)
				//if (check_behinder()) 
				//	select_state(eStateAttack_RunAway);
				//else 
				select_state(eStateAttack_Melee);
			}
			else
			{
				select_state(eStateAttack_Run);
			}
	}

	// clear behinder var if not melee state selected
	if (current_substate != eStateAttack_Melee)
	{
		m_time_start_check_behinder = 0;
	}
	else
	{
		m_pBloodsucker->clear_runaway_invisible();
	}

	get_state_current()->execute();
	prev_substate = current_substate;

	// Notify squad	
	CMonsterSquad* squad = monster_squad().get_squad(object);
	if (squad)
	{
		SMemberGoal goal{};

		goal.type = MG_AttackEnemy;
		goal.entity = const_cast<CEntityAlive*>(object->EnemyMan.get_enemy());

		squad->UpdateGoal(object, goal);
	}
}

bool CustomBloodsuckerStateAttack::check_vampire()
{
	if (prev_substate != eStateVampire_Execute)
	{
		if (get_state(eStateVampire_Execute)->check_start_conditions())	return true;
	}
	else
	{
		if (!get_state(eStateVampire_Execute)->check_completion())		return true;
	}
	return false;
}

bool CustomBloodsuckerStateAttack::check_hiding()
{
	const bool health_step_lost = object->conditions().GetHealth() <
		m_last_health - EntityDefinitions::CBloodsuckerBase::loose_health_diff;

	if (health_step_lost)
	{
		m_pBloodsucker->start_runaway_invisible();
		m_last_health = object->conditions().GetHealth();
		m_start_with_encircle = true;
		return true;
	}

	// if we get here before 1 sec after last critical hit: 
	u32 last_critical_hit_tick = m_pBloodsucker->get_last_critical_hit_tick();
	if (last_critical_hit_tick && time() < last_critical_hit_tick + 1000)
	{
		m_pBloodsucker->clear_last_critical_hit_tick();
		m_start_with_encircle = true;
		return true;
	}

	if (current_substate == eStateAttack_Hide)
	{
		return !get_state_current()->check_completion();
	}

	m_start_with_encircle = false;
	return get_state(eStateAttack_Hide)->check_start_conditions();
}

void CustomBloodsuckerStateAttack::setup_substates()
{
	auto state = get_state_current();

	if (current_substate == eStateAttack_Hide)
	{
		typename CustomBloodsuckerBackstubEnemy::StateParams data{};

		data.action.action = ACT_RUN;
		data.action.time_out = 0;
		data.completion_dist = 1.f;		// get exactly to the point
		data.time_to_rebuild = 200;
		data.accelerated = true;
		data.braking = false;
		data.accel_type = eAT_Aggressive;
		data.action.sound_type = MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = object->db().m_dwIdleSndDelay;
		data.start_with_encircle = m_start_with_encircle;

		state->fill_data_with(&data, sizeof(CustomBloodsuckerBackstubEnemy::StateParams));
		return;
	}
}

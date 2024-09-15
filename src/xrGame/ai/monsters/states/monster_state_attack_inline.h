#pragma once

#include "monster_state_attack_melee.h"
#include "monster_state_attack_run.h"
#include "monster_state_attack_run_attack.h"
#include "monster_state_attack_on_run.h"
#include "state_hide_from_point.h"
#include "monster_state_find_enemy.h"
#include "monster_state_steal.h"
#include "monster_state_attack_camp.h"
#include "monster_state_home_point_attack.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "../../../actor.h"

CStateMonsterAttack::CStateMonsterAttack(CBaseMonster *obj) : inherited(obj)
{
	this->add_state	(eStateAttack_Run,				xr_new<CStateMonsterAttackRun<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_Melee,			xr_new<CStateMonsterAttackMelee<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_RunAttack,		xr_new<CStateMonsterAttackRunAttack<CBaseMonster> >	(obj));
	this->add_state	(eStateAttack_Attack_On_Run,	xr_new<CStateMonsterAttackOnRun<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_RunAway,			xr_new<CStateMonsterHideFromPoint<CBaseMonster> >	(obj));
	this->add_state	(eStateAttack_FindEnemy,		xr_new<CStateMonsterFindEnemy<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_Steal,			xr_new<CStateMonsterSteal<CBaseMonster> >			(obj));
	this->add_state	(eStateAttackCamp,				xr_new<CStateMonsterAttackCamp<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_MoveToHomePoint,	xr_new<CStateMonsterAttackMoveToHomePoint<CBaseMonster> >(obj));
}

// Lain: added 
CStateMonsterAttack::CStateMonsterAttack(CBaseMonster*obj, state_ptr state_move2home) : inherited(obj)
{
	this->add_state	(eStateAttack_Run,				xr_new<CStateMonsterAttackRun<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_Melee,			xr_new<CStateMonsterAttackMelee<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_RunAttack,		xr_new<CStateMonsterAttackRunAttack<CBaseMonster> >	(obj));
	this->add_state	(eStateAttack_Attack_On_Run,	xr_new<CStateMonsterAttackOnRun<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_RunAway,			xr_new<CStateMonsterHideFromPoint<CBaseMonster> >	(obj));
	this->add_state	(eStateAttack_FindEnemy,		xr_new<CStateMonsterFindEnemy<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_Steal,			xr_new<CStateMonsterSteal<CBaseMonster> >			(obj));
	this->add_state	(eStateAttackCamp,				xr_new<CStateMonsterAttackCamp<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_MoveToHomePoint,	state_move2home);	
}

CStateMonsterAttack::CStateMonsterAttack(CBaseMonster*obj, state_ptr state_run, state_ptr state_melee) : inherited(obj)
{
	this->add_state	(eStateAttack_Run,				state_run);
	this->add_state	(eStateAttack_Melee,			state_melee);
	this->add_state	(eStateAttack_RunAttack,		xr_new<CStateMonsterAttackRunAttack<CBaseMonster> >	(obj));
	this->add_state	(eStateAttack_Attack_On_Run,	xr_new<CStateMonsterAttackOnRun<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_RunAway,			xr_new<CStateMonsterHideFromPoint<CBaseMonster> >	(obj));
	this->add_state	(eStateAttack_FindEnemy,		xr_new<CStateMonsterFindEnemy<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_Steal,			xr_new<CStateMonsterSteal<CBaseMonster> >			(obj));
	this->add_state	(eStateAttackCamp,				xr_new<CStateMonsterAttackCamp<CBaseMonster> >		(obj));
	this->add_state	(eStateAttack_MoveToHomePoint,	xr_new<CStateMonsterAttackMoveToHomePoint<CBaseMonster> >(obj));
}

CStateMonsterAttack::~CStateMonsterAttack()
{
}

void CStateMonsterAttack::initialize()
{
	inherited::initialize				();
	this->object->MeleeChecker.init_attack	();
	
	m_time_next_run_away				= 0;
	m_time_start_check_behinder			= 0;
	m_time_start_behinder				= 0;
}

#define FIND_ENEMY_DELAY	12000

void CStateMonsterAttack::execute()
{
	bool	can_attack_on_move		=	this->object->can_attack_on_move();

	if		(check_home_point())		this->select_state(eStateAttack_MoveToHomePoint);
	else if (check_steal_state())		this->select_state(eStateAttack_Steal);
	else if (check_camp_state()) 		this->select_state(eStateAttackCamp);
	else if (check_find_enemy_state()) 	this->select_state(eStateAttack_FindEnemy);
	else if (check_run_away_state()) 	this->select_state(eStateAttack_RunAway);
	else if (!can_attack_on_move && 
			 check_run_attack_state())	this->select_state(eStateAttack_RunAttack);
	else if ( can_attack_on_move )			this->select_state(eStateAttack_Attack_On_Run);
	else
	{
		// определить тип атаки
		bool b_melee		=	false; 
		if (this->prev_substate == eStateAttack_Melee )
		{
			if ( !this->get_state_current()->check_completion() )
			{
				b_melee		=	true;
			}
		} 
		else if (this->get_state(eStateAttack_Melee)->check_start_conditions() )
		{
			b_melee			=	true;
		}

		// установить целевое состояние
		this->select_state			(b_melee ? eStateAttack_Melee : eStateAttack_Run);
	}

	this->get_state_current()->execute();
	
	this->prev_substate			= this->current_substate;

	// Notify squad	
	CMonsterSquad *squad	=	monster_squad().get_squad(this->object);
	if (squad) {
		SMemberGoal				goal;
		goal.type			=	MG_AttackEnemy;
		goal.entity			=	const_cast<CEntityAlive*>(this->object->EnemyMan.get_enemy());

		squad->UpdateGoal		(this->object, goal);
	}
	//////////////////////////////////////////////////////////////////////////
}

bool CStateMonsterAttack::check_steal_state()
{
	if (this->prev_substate == u32(-1)) {
		if (this->get_state(eStateAttack_Steal)->check_start_conditions())
			return true;
	} else if (this->prev_substate == eStateAttack_Steal) {
		if (!this->get_state(eStateAttack_Steal)->check_completion())
			return true;
	}
	return false;
}

bool CStateMonsterAttack::check_camp_state()
{
	if (this->prev_substate == u32(-1)) {
		if (this->get_state(eStateAttackCamp)->check_start_conditions())
			return true;
	} else if (this->prev_substate == eStateAttackCamp) {
		if (!this->get_state(eStateAttackCamp)->check_completion())
			return true;
	}
	return false;
}

bool CStateMonsterAttack::check_find_enemy_state()
{
	// check state find enemy
	if (this->object->EnemyMan.get_enemy_time_last_seen() + FIND_ENEMY_DELAY < Device.dwTimeGlobal) 
		return true;
	return false;
}

bool CStateMonsterAttack::check_run_away_state()
{
	if (m_time_start_behinder != 0) 
		return false;

	if (this->prev_substate == eStateAttack_RunAway) {
		if (!this->get_state(eStateAttack_RunAway)->check_completion())
			return true;
		else m_time_next_run_away = Device.dwTimeGlobal + 10000;
	} else if ((this->object->EnemyMan.get_enemy() != Actor()) && this->object->Morale.is_despondent() && (m_time_next_run_away < Device.dwTimeGlobal)) {
		return true;
	}

	return false;
}

bool CStateMonsterAttack::check_home_point()
{
	if (this->prev_substate != eStateAttack_MoveToHomePoint) {
		if (this->get_state(eStateAttack_MoveToHomePoint)->check_start_conditions())
			return true;
	} else {
		if (!this->get_state(eStateAttack_MoveToHomePoint)->check_completion())
			return true;
	}

	return false;
}

bool CStateMonsterAttack::check_run_attack_state()
{
	if (!this->object->ability_run_attack()) return false;

	if (this->prev_substate == eStateAttack_Run) {
		if (this->get_state(eStateAttack_RunAttack)->check_start_conditions())
			return true;			
	} else if (this->prev_substate == eStateAttack_RunAttack) {
		if (!this->get_state(eStateAttack_RunAttack)->check_completion())
			return true;
	}

	return false;
}

void CStateMonsterAttack::setup_substates()
{
	state_ptr state = this->get_state_current();

	if (this->current_substate == eStateAttack_RunAway) {
		
		SStateHideFromPoint		data;
		data.point				= this->object->EnemyMan.get_enemy_position();
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type			= eAT_Aggressive;
		data.distance			= 20.f;
		data.action.action		= ACT_RUN;
		data.action.sound_type	= MonsterSound::eMonsterSoundAggressive;
		data.action.sound_delay = this->object->db().m_dwAttackSndDelay;
		data.action.time_out	= 5000;

		state->fill_data_with(&data, sizeof(SStateHideFromPoint));

		return;
	}
}

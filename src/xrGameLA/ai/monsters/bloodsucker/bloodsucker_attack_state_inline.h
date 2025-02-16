#pragma once

//#include "bloodsucker_attack_state_hide.h"
#include "../states/state_move_to_point.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CBloodsuckerStateAttackAbstract CBloodsuckerStateAttack<_Object>

TEMPLATE_SPECIALIZATION
CBloodsuckerStateAttackAbstract::CBloodsuckerStateAttack(_Object *obj) : inherited_attack(obj)
{
	//add_state(eStateAttack_Hide,	new CBloodsuckerStateAttackHide<_Object>(obj));
	this->add_state	(eStateAttack_Hide, new CStateMonsterMoveToPointEx<_Object>(obj));
}

TEMPLATE_SPECIALIZATION
CBloodsuckerStateAttackAbstract::~CBloodsuckerStateAttack()
{
}

TEMPLATE_SPECIALIZATION
void CBloodsuckerStateAttackAbstract::initialize()
{
	this->inherited::initialize	();
	m_time_stop_invis		= 0;
}

TEMPLATE_SPECIALIZATION
void CBloodsuckerStateAttackAbstract::finalize()
{
	this->inherited::finalize();
	this->object->stop_invisible_predator();
}

TEMPLATE_SPECIALIZATION
void CBloodsuckerStateAttackAbstract::critical_finalize()
{
	this->inherited::critical_finalize();
	this->object->stop_invisible_predator();
}

#define	INVIS_ACTIVATE_DELAY	3000
#define INVIS_DIST_TO_ENEMY		5.f

TEMPLATE_SPECIALIZATION
void CBloodsuckerStateAttackAbstract::execute()
{
	bool selected = false;

	if (this->check_home_point()) {
		this->select_state	(eStateAttack_MoveToHomePoint);
		selected		= true;
	} else if (this->check_hiding()) {
		this->select_state	(eStateAttack_Hide);
		selected		= true;
	} else if (this->check_steal_state()) {
		this->select_state	(eStateAttack_Steal);
		selected		= true;
	} else if (this->check_camp_state()) {
		this->select_state	(eStateAttackCamp);
		selected		= true;
	} else if (this->check_find_enemy_state()) {
		this->select_state	(eStateAttack_FindEnemy);
		selected		= true;
	} else if (this->check_run_attack_state()) {
		this->select_state	(eStateAttack_RunAttack);
		selected		= true;
	}

	if (!selected) {
		// определить тип атаки
		bool b_melee = false; 

		if (this->prev_substate == eStateAttack_Melee) {
			if (!this->get_state_current()->check_completion()) {
				b_melee = true;
			}
		} else if (this->get_state(eStateAttack_Melee)->check_start_conditions()) {
			b_melee = true;
		}
		
		if (!b_melee && (this->prev_substate == eStateAttack_Melee)) {
			this->select_state	(eStateAttack_Hide);
		} else
		// установить целевое состояние
		if (b_melee) {  
			// check if enemy is behind me for a long time
			// [TODO] make specific state and replace run_away state (to avoid ratation jumps)
			//if (check_behinder()) 
			//	select_state(eStateAttack_RunAway);
			//else 
			this->select_state(eStateAttack_Melee);
		}
		else this->select_state(eStateAttack_Run);
	}

	// clear behinder var if not melee state selected
	if (this->current_substate != eStateAttack_Melee) this->m_time_start_check_behinder = 0;
	update_invisibility				();
	
	this->get_state_current()->execute	();
	this->prev_substate = this->current_substate;

	// Notify squad	
	CMonsterSquad *squad	= monster_squad().get_squad(this->object);
	if (squad) {
		SMemberGoal			goal;

		goal.type			= MG_AttackEnemy;
		goal.entity			= const_cast<CEntityAlive*>(this->object->EnemyMan.get_enemy());

		squad->UpdateGoal	(this->object, goal);
	}
	//////////////////////////////////////////////////////////////////////////
}



TEMPLATE_SPECIALIZATION
void CBloodsuckerStateAttackAbstract::update_invisibility()
{
	if (this->object->threaten_time() > 0)
	{
		this->object->stop_invisible_predator	();
		return;
	}

	if (this->object->state_invisible) {
		// check conditions to stop invis
		if (this->current_substate == eStateAttack_Melee) {
			this->object->stop_invisible_predator	();
			m_time_stop_invis				= time();		
		}
	} else {
		// check conditions to start invis
		if (this->current_substate == eStateAttack_Hide) {
			this->object->start_invisible_predator();
		} else 
		if ((this->current_substate == eStateAttack_Run) && (this->object->EnemyMan.get_enemy()->Position().distance_to(this->object->Position()) > INVIS_DIST_TO_ENEMY)) {
			if (m_time_stop_invis + INVIS_ACTIVATE_DELAY < time()) 
				this->object->start_invisible_predator();
		}
	}
}

TEMPLATE_SPECIALIZATION
bool CBloodsuckerStateAttackAbstract::check_hiding()
{
	if (this->current_substate == eStateAttack_Hide)
		if (!this->get_state(eStateAttack_Melee)->check_start_conditions())
			if (!this->get_state_current()->check_completion()) {
				//object->path().set_use_dest_orient	(true);
				//object->path().set_dest_direction	(Fvector().sub(object->EnemyMan.get_enemy()->Position(),m_dir_point));
				return true;
			}

	return false;


	//if (current_substate == eStateAttack_Melee) {
	//	if (prev_substate != eStateAttack_Melee) {
	//		object->stop_invisible_predator	();
	//		m_time_stop_invis				= time();
	//	}
	//	
	//	if (get_state_current()->check_completion()) ret_value = true;
	//} else 
	//if (current_substate == eStateAttack_Run) {
	//	if (object->EnemyMan.get_enemy()->Position().distance_to(object->Position()) > INVIS_DIST_TO_ENEMY) {
	//		if (!object->state_invisible && (m_time_stop_invis + INVIS_ACTIVATE_DELAY < time())) 
	//			object->start_invisible_predator();
	//	}
	//} 
	//		
	//return ret_value;
}

TEMPLATE_SPECIALIZATION
void CBloodsuckerStateAttackAbstract::setup_substates()
{
	auto state = this->get_state_current();

	if (this->current_substate == eStateAttack_Hide) {

		SStateDataMoveToPointEx data;

		Fvector target_dir		= Random.randI(2) ? this->object->XFORM().i : Fvector().set(this->object->XFORM().i).invert();
		m_dir_point				= Fvector().mad(this->object->Position(), target_dir, 2.5f);

		data.vertex				= 0;
		data.point				= m_dir_point;
		data.action.action		= ACT_RUN;
		data.action.time_out	= 1500;		// do not use time out
		data.completion_dist	= 1.f;		// get exactly to the point
		data.time_to_rebuild	= this->object->get_attack_rebuild_time();
		data.accelerated		= true;
		data.braking			= false;
		data.accel_type 		= eAT_Aggressive;
		data.action.sound_type	= MonsterSound::eMonsterSoundIdle;
		data.action.sound_delay = this->object->db().m_dwIdleSndDelay;

		state->fill_data_with(&data, sizeof(SStateDataMoveToPointEx));

		return;
	}
}

#undef TEMPLATE_SPECIALIZATION
#undef CBloodsuckerStateAttackAbstract


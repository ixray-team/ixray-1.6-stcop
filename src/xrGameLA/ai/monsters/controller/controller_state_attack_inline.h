#pragma once

#include "controller_state_attack_hide.h"
#include "controller_state_attack_hide_lite.h"
#include "controller_state_attack_moveout.h"
#include "controller_state_attack_camp.h"
#include "controller_state_attack_fire.h"
#include "controller_tube.h"

#define CONTROL_FIRE_PERC 80
#define CONTROL_TUBE_PERC 20


#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateControllerAttackAbstract CStateControllerAttack<_Object>

TEMPLATE_SPECIALIZATION
CStateControllerAttackAbstract::CStateControllerAttack(_Object *obj) : inherited(obj)
{
/*
	add_state	(eStateAttack_HideInCover,		xr_new<CStateControlHide<_Object> >		(obj));
	add_state	(eStateAttack_HideInCoverLite,	xr_new<CStateControlHideLite<_Object> >	(obj));
	add_state	(eStateAttack_MoveOut,			xr_new<CStateControlMoveOut<_Object> >	(obj));
	add_state	(eStateAttack_CampInCover,		xr_new<CStateControlCamp<_Object> >		(obj));
	add_state	(eStateAttack_ControlFire,		xr_new<CStateControlFire<_Object> >		(obj));
	add_state	(eStateAttack_ControlTube,		xr_new<CStateControllerTube<_Object> >	(obj));
*/
				
//	add_state	(eStateAttack_Run,				xr_new<CStateMonsterAttackRun<CController>>		(obj));
//	add_state	(eStateAttack_Melee,			xr_new<CStateMonsterAttackMelee<CController>>	(obj));	
//	add_state	(eStateAttack_ControlTube,		xr_new<CStateControllerTube<CController>>		(obj));

//	add_state	(eStateAttack_Hide,				xr_new<CStateControlHide<CController>>			(obj));
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::execute()
{
	/*
	if (get_state(eStateAttack_ControlTube)->check_start_conditions())
	{
		select_state								(eStateAttack_ControlTube);
		get_state_current()->execute				();
		prev_substate								= current_substate;
		return;
	}
	*/
	inherited::execute								();
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::setup_substates()
{
	inherited::setup_substates();
	/*
	state_ptr state = get_state_current();
	
	if (current_substate == eStateFaceEnemy) {
		SStateDataLookToPoint data;

		data.point				= object->EnemyMan.get_enemy_position();
		data.action.action		= ACT_STAND_IDLE;

		state->fill_data_with(&data, sizeof(SStateDataLookToPoint));
	
		object->sound().play(MonsterSound::eMonsterSoundAggressive, 0,0,object->db().m_dwAttackSndDelay);
		return;
	}
	*/	
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::check_force_state() 
{
	float dist_to_enemy = -1.f;
	if (current_substate == eStateAttack_Run) 
	{
		dist_to_enemy = object->Position().distance_to(object->EnemyMan.get_enemy_position());
		if (dist_to_enemy > 10.f) 
		{
			get_state_current()->critical_finalize();
			current_substate = u32(-1);
		}
	}
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::initialize()
{
	inherited::initialize				();
	object->set_mental_state			(CController::eStateDanger);
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::finalize()
{
	inherited::finalize();
	object->set_mental_state(CController::eStateIdle);
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::critical_finalize()
{
	inherited::critical_finalize();
	object->set_mental_state(CController::eStateIdle);
}

TEMPLATE_SPECIALIZATION
bool CStateControllerAttackAbstract::check_state(u32 state_id) 
{
	if (prev_substate == state_id) {
		if (!get_state_current()->check_completion())		return true;
	} else {
		if (get_state(state_id)->check_start_conditions())	return true;
	}

	return false;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateControllerAttackAbstract
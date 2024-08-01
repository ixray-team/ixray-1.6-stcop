#pragma once

#include "controller_state_attack_hide.h"
#include "controller_state_attack_hide_lite.h"
#include "controller_state_attack_moveout.h"
#include "controller_state_attack_camp.h"
#include "controller_state_attack_fire.h"
#include "controller_tube.h"

#include "../states/monster_state_home_point_attack.h"
#include "../states/monster_state_attack_run.h"
#include "../states/monster_state_attack_melee.h"

#define CONTROL_FIRE_PERC 80
#define CONTROL_TUBE_PERC 20


#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateControllerAttackAbstract CStateControllerAttack<_Object>

TEMPLATE_SPECIALIZATION
CStateControllerAttackAbstract::CStateControllerAttack(_Object *obj) : inherited(obj)
{
	this->add_state(eStateAttack_MoveToHomePoint,	xr_new<CStateMonsterAttackMoveToHomePoint<CController> >(obj));	
 	this->add_state(eStateAttack_Run,				xr_new<CStateMonsterAttackRun<CController> >			(obj));
 	this->add_state(eStateAttack_Melee,			xr_new<CStateMonsterAttackMelee<CController> >			(obj));
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::initialize()
{	
	inherited::initialize				();
}

TEMPLATE_SPECIALIZATION
bool CStateControllerAttackAbstract::check_home_point()
{
	if (this->prev_substate != eStateAttack_MoveToHomePoint) {
		if (this->get_state(eStateAttack_MoveToHomePoint)->check_start_conditions())	return true;
	} else {
		if (!this->get_state(eStateAttack_MoveToHomePoint)->check_completion())		return true;
	}

	return false;
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::execute()
{
    EMonsterState state_id = eStateUnknown;

    if (this->prev_substate == u32(-1))
        state_id = eStateAttack_HideInCoverLite;

    if (state_id == eStateUnknown)
    {
        if (this->prev_substate == eStateAttack_HideInCoverLite)
        {
            if (!this->get_state_current()->check_completion())
                state_id = eStateAttack_HideInCoverLite;
            else if (this->get_state(eStateAttack_MoveOut)->check_start_conditions())
                state_id = eStateAttack_MoveOut;
        }
    }

    if (state_id == eStateUnknown)
    {
        if (this->prev_substate == eStateAttack_MoveOut)
        {
            if (this->get_state(eStateAttack_ControlFire)->check_start_conditions())
                state_id = eStateAttack_ControlFire;
            else
            {
                if (!this->get_state_current()->check_completion())
                    state_id = eStateAttack_MoveOut;
                else
                    state_id = eStateAttack_HideInCover;
            }
        }
    }

    if (state_id == eStateUnknown)
    {
        if (this->prev_substate == eStateAttack_ControlFire)
        {
            if (!this->get_state_current()->check_completion())
                state_id = eStateAttack_ControlFire;
            else
                state_id = eStateAttack_HideInCover;
        }
    }

    if (state_id == eStateUnknown)
    {
        if (this->prev_substate == eStateAttack_HideInCover)
        {
            if (!this->get_state_current()->check_completion())
                state_id = eStateAttack_HideInCover;
            else
                state_id = eStateAttack_CampInCover;
        }
    }

    if (state_id == eStateUnknown)
    {
        if (this->prev_substate == eStateAttack_CampInCover)
        {
            if (!this->get_state_current()->check_completion())
                state_id = eStateAttack_CampInCover;
            else
            {
                if (this->get_state(eStateAttack_MoveOut)->check_start_conditions())
                    state_id = eStateAttack_MoveOut;
                else
                    state_id = eStateAttack_HideInCoverLite;
            }
        }
    }

    if (state_id == eStateUnknown)
    {
        state_id = eStateAttack_HideInCover;
    }

    this->select_state(state_id);
    this->get_state_current()->execute();
    this->prev_substate = this->current_substate;
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::setup_substates()
{
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::check_force_state() 
{
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::finalize()
{
	inherited::finalize();
}

TEMPLATE_SPECIALIZATION
void CStateControllerAttackAbstract::critical_finalize()
{
	inherited::critical_finalize();
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateControllerAttackAbstract
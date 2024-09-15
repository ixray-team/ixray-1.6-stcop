#pragma once

#include "monster_state_hitted_hide.h"
#include "monster_state_hitted_moveout.h"
#include "monster_state_home_point_danger.h"


CStateMonsterHitted::CStateMonsterHitted(CBaseMonster*obj) : inherited(obj)
{
	this->add_state	(eStateHitted_Hide,		xr_new<CStateMonsterHittedHide >(obj));
	this->add_state	(eStateHitted_MoveOut,	xr_new<CStateMonsterHittedMoveOut >(obj));
	this->add_state	(eStateHitted_Home,		xr_new<CStateMonsterDangerMoveToHomePoint>(obj));
}


CStateMonsterHitted::~CStateMonsterHitted()
{
}


void CStateMonsterHitted::reselect_state()
{
	if (this->get_state(eStateHitted_Home)->check_start_conditions())	{
		this->select_state(eStateHitted_Home);
		return;
	}

	if (this->prev_substate == u32(-1)) {
		this->select_state(eStateHitted_Hide);
		return;
	}
	
	if (this->prev_substate == eStateHitted_Hide) {
		this->select_state(eStateHitted_MoveOut);
		return;
	}

	this->select_state(eStateHitted_Hide);
}

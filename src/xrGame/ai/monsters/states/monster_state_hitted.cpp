#include "StdAfx.h"
#include "monster_state_hitted.h"

#include "monster_state_hitted_hide.h"
#include "monster_state_hitted_moveout.h"
#include "monster_state_home_point_danger.h"


CStateMonsterHitted::CStateMonsterHitted(CBaseMonster* obj) : inherited(obj)
{
    add_state(eStateHitted_Hide, new CStateMonsterHittedHide(obj));
    add_state(eStateHitted_MoveOut, new CStateMonsterHittedMoveOut(obj));
    add_state(eStateHitted_Home, new CStateMonsterDangerMoveToHomePoint(obj));
}



CStateMonsterHitted::~CStateMonsterHitted()
{
}


void CStateMonsterHitted::reselect_state()
{
	if (get_state(eStateHitted_Home)->check_start_conditions()) {
		select_state(eStateHitted_Home);
		return;
	}

	if (prev_substate == u32(-1)) {
		select_state(eStateHitted_Hide);
		return;
	}

	if (prev_substate == eStateHitted_Hide) {
		select_state(eStateHitted_MoveOut);
		return;
	}

	select_state(eStateHitted_Hide);
}

#pragma once

#include "chimera_state_hunting_move_to_cover.h"
#include "chimera_state_hunting_come_out.h"


CStateChimeraHunting::CStateChimeraHunting(CBaseMonster *obj) : inherited(obj)
{
	add_state(eStateMoveToCover,	xr_new<CStateChimeraHuntingMoveToCover>	(obj));
	add_state(eStateComeOut,		xr_new<CStateChimeraHuntingComeOut>		(obj));
}



bool CStateChimeraHunting::check_start_conditions()
{
	return true;
}


bool CStateChimeraHunting::check_completion()
{
	return false;
}


void CStateChimeraHunting::reselect_state()
{
	if (prev_substate == u32(-1))					select_state(eStateMoveToCover);
	else if (prev_substate == eStateMoveToCover)	select_state(eStateComeOut);
	else											select_state(eStateMoveToCover);
}


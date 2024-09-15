#pragma once



CStateChimeraHuntingMoveToCover::CStateChimeraHuntingMoveToCover(CBaseMonster *obj) : inherited(obj)
{
}



bool CStateChimeraHuntingMoveToCover::check_start_conditions()
{
	return true;
}


bool CStateChimeraHuntingMoveToCover::check_completion()
{
	return false;
}


void CStateChimeraHuntingMoveToCover::reselect_state()
{
	if (prev_substate == u32(-1))					select_state(eStateMoveToCover);
	else if (prev_substate == eStateMoveToCover)	select_state(eStateComeOut);
	else											select_state(eStateMoveToCover);
}

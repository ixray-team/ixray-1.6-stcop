#pragma once


CStateChimeraHuntingMoveToCover::CStateChimeraHuntingMoveToCover(CBaseMonster *obj) : inherited(obj)
{
}


void CStateChimeraHuntingMoveToCover::initialize()
{
	inherited::initialize();
	
	
}


bool CStateChimeraHuntingMoveToCover::check_completion()
{
	return false;
}


void CStateChimeraHuntingMoveToCover::execute()
{
	
}


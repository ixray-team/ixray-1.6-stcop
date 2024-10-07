#include "StdAfx.h"
#include "controller.h"
#include "controller_state_attack_fast_move.h"

CStateControllerFastMove::CStateControllerFastMove(CBaseMonster* obj) : inherited(obj) 
{
	m_pController = smart_cast<CControllerBase*>(obj);
}

void CStateControllerFastMove::initialize()
{
	inherited::initialize();

	m_pController->set_mental_state(CControllerBase::eStateIdle);
}


void CStateControllerFastMove::finalize()
{
	inherited::finalize();
	m_pController->set_mental_state(CControllerBase::eStateDanger);
}


void CStateControllerFastMove::critical_finalize()
{
	inherited::critical_finalize();
	m_pController->set_mental_state(CControllerBase::eStateDanger);
}



void CStateControllerFastMove::execute()
{
	object->set_action(ACT_RUN);

	//select another cover


}
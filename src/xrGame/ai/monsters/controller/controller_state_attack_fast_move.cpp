#include "StdAfx.h"
#include "controller.h"
#include "controller_state_attack_fast_move.h"

CStateControllerFastMove::CStateControllerFastMove(CBaseMonster* object) : inherited(object)
{
	pControllerBase = smart_cast<CControllerBase*>(object);
}

CStateControllerFastMove::~CStateControllerFastMove()
{

}

void CStateControllerFastMove::initialize()
{
	inherited::initialize();

	pControllerBase->set_mental_state(CControllerBase::eStateIdle);
}

void CStateControllerFastMove::finalize()
{
	inherited::finalize();
	pControllerBase->set_mental_state(CControllerBase::eStateDanger);
}

void CStateControllerFastMove::critical_finalize()
{
	inherited::critical_finalize();
	pControllerBase->set_mental_state(CControllerBase::eStateDanger);
}

void CStateControllerFastMove::execute()
{
	object->set_action(ACT_RUN);
}
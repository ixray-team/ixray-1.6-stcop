#pragma once


void CStateControllerFastMove::initialize()
{
	inherited::initialize();

	object->set_mental_state(CController::eStateIdle);
}


void CStateControllerFastMove::finalize()
{
	inherited::finalize();
	object->set_mental_state	(CController::eStateDanger);	
}


void CStateControllerFastMove::critical_finalize()
{
	inherited::critical_finalize();
	object->set_mental_state	(CController::eStateDanger);
}



void CStateControllerFastMove::execute()
{
	object->set_action	(ACT_RUN);

	//select another cover


}

#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateControllerFastMoveAbstract CStateControllerFastMove<_Object>

TEMPLATE_SPECIALIZATION
void CStateControllerFastMoveAbstract::initialize()
{
	inherited::initialize();

	this->object->set_mental_state(CController::eStateIdle);
}

TEMPLATE_SPECIALIZATION
void CStateControllerFastMoveAbstract::finalize()
{
	inherited::finalize();
	this->object->set_mental_state	(CController::eStateDanger);
}

TEMPLATE_SPECIALIZATION
void CStateControllerFastMoveAbstract::critical_finalize()
{
	inherited::critical_finalize();
	this->object->set_mental_state	(CController::eStateDanger);
}


TEMPLATE_SPECIALIZATION
void CStateControllerFastMoveAbstract::execute()
{
	this->object->set_action	(ACT_RUN);

	//select another cover


}

#undef TEMPLATE_SPECIALIZATION
#undef CStateControllerFastMoveAbstract

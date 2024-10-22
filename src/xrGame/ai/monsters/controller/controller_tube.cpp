#include "StdAfx.h"
#include "controller.h"
#include "controller_tube.h"

#include "controller_psy_hit.h"

CStateControllerTube::CStateControllerTube(CBaseMonster* object) : inherited(object)
{
	pControllerBase = smart_cast<CControllerBase*>(object);
}

CStateControllerTube::~CStateControllerTube()
{

}

void CStateControllerTube::execute()
{
	object->control().activate(ControlCom::eComCustom1);
	object->set_action(ACT_STAND_IDLE);
}

bool CStateControllerTube::check_start_conditions()
{
	if (object->EnemyMan.see_enemy_duration() < EntityDefinitions::CControllerBase::SEE_ENEMY_DURATION) return false;
	if (!pControllerBase->m_psy_hit->check_start_conditions()) return false;

	return true;
}


bool CStateControllerTube::check_completion()
{
	return (!pControllerBase->m_psy_hit->is_active());
}
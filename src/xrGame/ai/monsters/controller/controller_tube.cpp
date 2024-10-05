#include "StdAfx.h"
#include "controller.h"
#include "controller_tube.h"

#include "controller_psy_hit.h"

CStateControllerTube::CStateControllerTube(CBaseMonster* obj) : inherited(obj) 
{
	m_pController = smart_cast<CController*>(obj);
}

void CStateControllerTube::execute()
{
	this->object->control().activate(ControlCom::eComCustom1);
	this->object->set_action(ACT_STAND_IDLE);
}

#define SEE_ENEMY_DURATION 1000


bool CStateControllerTube::check_start_conditions()
{
	if (this->object->EnemyMan.see_enemy_duration() < SEE_ENEMY_DURATION) return false;
	if (!this->m_pController->m_psy_hit->check_start_conditions()) return false;

	return true;
}


bool CStateControllerTube::check_completion()
{
	return (!this->m_pController->m_psy_hit->is_active());
}
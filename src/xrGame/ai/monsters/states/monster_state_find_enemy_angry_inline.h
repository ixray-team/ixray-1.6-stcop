#pragma once

CStateMonsterFindEnemyAngry::CStateMonsterFindEnemyAngry(CBaseMonster*obj) : inherited(obj)
{
}


CStateMonsterFindEnemyAngry::~CStateMonsterFindEnemyAngry()
{
}


void CStateMonsterFindEnemyAngry::execute()
{
	this->object->set_action				(ACT_STAND_IDLE);
	this->object->anim().SetSpecParams		(ASP_THREATEN);
	this->object->set_state_sound			(MonsterSound::eMonsterSoundAggressive);
}


bool CStateMonsterFindEnemyAngry::check_completion()
{	
	if (this->time_state_started + 4000 > Device.dwTimeGlobal) return false;
	return true;
}


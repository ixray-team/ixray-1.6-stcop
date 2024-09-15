#pragma once

void CStateMonsterFindEnemyWalk::execute()
{
	this->object->set_action			(ACT_STAND_IDLE);
	this->object->set_state_sound		(MonsterSound::eMonsterSoundAggressive);
}


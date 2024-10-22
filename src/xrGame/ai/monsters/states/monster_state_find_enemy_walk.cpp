#include "StdAfx.h"
#include "monster_state_find_enemy_walk.h"

void CStateMonsterFindEnemyWalkAround::execute()
{
	object->set_action(ACT_STAND_IDLE);
	object->set_state_sound(MonsterSound::eMonsterSoundAggressive);
}


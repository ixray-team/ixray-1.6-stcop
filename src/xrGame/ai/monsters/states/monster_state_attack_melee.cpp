#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "monster_state_attack_melee.h"

CStateMonsterAttackMelee::CStateMonsterAttackMelee(CBaseMonster* obj) : inherited(obj)
{
}

CStateMonsterAttackMelee::~CStateMonsterAttackMelee()
{
}

void CStateMonsterAttackMelee::execute()
{
	object->set_action(ACT_ATTACK);
	if (object->control().direction().is_face_target(object->EnemyMan.get_enemy(), PI_DIV_3))
		object->dir().face_target(object->EnemyMan.get_enemy(), 800);
	else
		object->dir().face_target(object->EnemyMan.get_enemy(), 0, deg(15));

	object->set_state_sound(MonsterSound::eMonsterSoundAggressive);
}

bool CStateMonsterAttackMelee::check_start_conditions()
{
	return (
		object->MeleeChecker.can_start_melee(object->EnemyMan.get_enemy()) &&
		object->EnemyMan.see_enemy_now()
		);
}

bool CStateMonsterAttackMelee::check_completion()
{
	return (object->MeleeChecker.should_stop_melee(object->EnemyMan.get_enemy()));
}
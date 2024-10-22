#include "stdafx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "burer.h"
#include "burer_state_attack_melee.h"

#include "../control_animation_base.h"

CStateBurerAttackMelee::CStateBurerAttackMelee(CBaseMonster* object) : inherited(object)
{

}

CStateBurerAttackMelee::~CStateBurerAttackMelee()
{

}

bool CStateBurerAttackMelee::check_start_conditions()
{
	float dist = this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position());
	if (dist > EntityDefinitions::CBurerBase::MIN_DIST_MELEE_ATTACK) return false;

	return true;
}

bool CStateBurerAttackMelee::check_completion()
{
	float dist = this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position());
	if (dist < EntityDefinitions::CBurerBase::MAX_DIST_MELEE_ATTACK) return false;

	return true;
}

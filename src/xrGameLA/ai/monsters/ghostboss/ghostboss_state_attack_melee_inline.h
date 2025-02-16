#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateGhostBossAttackMeleeAbstract CStateGhostBossAttackMelee<_Object>

#define MIN_DIST_MELEE_ATTACK	5.f
#define MAX_DIST_MELEE_ATTACK	9.f

TEMPLATE_SPECIALIZATION
CStateGhostBossAttackMeleeAbstract::CStateGhostBossAttackMelee(_Object *obj) : inherited(obj)
{
}

TEMPLATE_SPECIALIZATION
bool CStateGhostBossAttackMeleeAbstract::check_start_conditions()
{
	float dist = this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position());
	if (dist > MIN_DIST_MELEE_ATTACK) return false;

	return true;
}

TEMPLATE_SPECIALIZATION
bool CStateGhostBossAttackMeleeAbstract::check_completion()
{
	float dist = this->object->Position().distance_to(this->object->EnemyMan.get_enemy()->Position());
	if (dist < MAX_DIST_MELEE_ATTACK) return false;

	return true;

}

#undef TEMPLATE_SPECIALIZATION
#undef CStateGhostBossAttackMeleeAbstract

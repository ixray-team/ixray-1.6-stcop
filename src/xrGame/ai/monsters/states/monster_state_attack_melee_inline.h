#pragma once

CStateMonsterAttackMelee::CStateMonsterAttackMelee(CBaseMonster *obj) : inherited(obj)
{
}

CStateMonsterAttackMelee::~CStateMonsterAttackMelee()
{
}

void CStateMonsterAttackMelee::execute()
{
	this->object->set_action			(ACT_ATTACK);
	if (this->object->control().direction().is_face_target(this->object->EnemyMan.get_enemy(), PI_DIV_3))
		this->object->dir().face_target	(this->object->EnemyMan.get_enemy(), 800);
	else 
		this->object->dir().face_target	(this->object->EnemyMan.get_enemy(), 0, deg(15));

	this->object->set_state_sound		(MonsterSound::eMonsterSoundAggressive);
}

bool CStateMonsterAttackMelee::check_start_conditions()
{
	return (
		this->object->MeleeChecker.can_start_melee(this->object->EnemyMan.get_enemy()) &&
		this->object->EnemyMan.see_enemy_now()
	);
}

bool CStateMonsterAttackMelee::check_completion()
{
	return (this->object->MeleeChecker.should_stop_melee(this->object->EnemyMan.get_enemy()));
}
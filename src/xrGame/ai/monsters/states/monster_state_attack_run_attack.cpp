#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "monster_state_attack_run_attack.h"

#include "../monster_velocity_space.h"


void CStateMonsterAttackRunAttack::initialize()
{
	inherited::initialize();

	object->m_time_last_attack_success = 0;
}

void CStateMonsterAttackRunAttack::execute()
{
	object->set_action(ACT_RUN);
	object->set_state_sound(MonsterSound::eMonsterSoundAggressive);
	object->anim().SetSpecParams(ASP_ATTACK_RUN);
}

void CStateMonsterAttackRunAttack::finalize()
{
	inherited::finalize();
}

void CStateMonsterAttackRunAttack::critical_finalize()
{
	inherited::critical_finalize();
}

bool CStateMonsterAttackRunAttack::check_start_conditions()
{
	float dist = object->MeleeChecker.distance_to_enemy(object->EnemyMan.get_enemy());

	if (dist > object->db().m_run_attack_start_dist)	return false;
	if (dist < object->MeleeChecker.get_min_distance())		return false;

	// check angle
	if (!object->control().direction().is_face_target(object->EnemyMan.get_enemy(), deg(30))) return false;

	// try to build path
	Fvector target_position;
	target_position.mad(object->Position(), object->Direction(), object->db().m_run_attack_path_dist);

	//if (!object->control().path_builder().build_special(target_position, u32(-1), MonsterMovement::eVelocityParamsRunAttack)) return false;
	//else object->path().enable_path();

	return true;
}

bool CStateMonsterAttackRunAttack::check_completion()
{
	if (!object->control().path_builder().is_moving_on_path() || (object->m_time_last_attack_success != 0)) return true;
	return false;
}
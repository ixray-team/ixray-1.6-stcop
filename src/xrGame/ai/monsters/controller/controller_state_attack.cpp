#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "controller.h"

#include "controller_state_attack.h"

#include "controller_state_attack_hide.h"
#include "controller_state_attack_hide_lite.h"
#include "controller_state_attack_moveout.h"
#include "controller_state_attack_camp.h"
#include "controller_state_attack_fire.h"
#include "controller_tube.h"

#include "../states/monster_state_home_point_attack.h"
#include "../states/monster_state_attack_run.h"
#include "../states/monster_state_attack_melee.h"


CStateControllerAttack::CStateControllerAttack(CBaseMonster* object) : inherited(object)
{
	add_state(eStateAttack_MoveToHomePoint, new CStateMonsterAttackMoveToHomePoint(object));
	add_state(eStateAttack_Run, new CStateMonsterAttackRun(object));
	add_state(eStateAttack_Melee, new CStateMonsterAttackMelee(object));
}

CStateControllerAttack::~CStateControllerAttack()
{

}

void CStateControllerAttack::initialize()
{
	inherited::initialize();
}

bool CStateControllerAttack::check_home_point()
{
	if (prev_substate != eStateAttack_MoveToHomePoint) {
		if (get_state(eStateAttack_MoveToHomePoint)->check_start_conditions())	return true;
	}
	else {
		if (!get_state(eStateAttack_MoveToHomePoint)->check_completion())		return true;
	}

	return false;
}

void CStateControllerAttack::execute()
{
	object->anim().clear_override_animation();

	if (check_home_point())
	{
		select_state(eStateAttack_MoveToHomePoint);
		get_state_current()->execute();
		prev_substate = current_substate;
		return;
	}

	EMonsterState		state_id = eStateUnknown;
	const CEntityAlive* enemy = object->EnemyMan.get_enemy();

	if (current_substate == eStateAttack_Melee)
	{
		if (get_state(eStateAttack_Melee)->check_completion())
			state_id = eStateAttack_Run;
		else
			state_id = eStateAttack_Melee;
	}
	else
	{
		if (get_state(eStateAttack_Melee)->check_start_conditions())
			state_id = eStateAttack_Melee;
		else
			state_id = eStateAttack_Run;
	}

	if (!object->enemy_accessible() && state_id == eStateAttack_Run)
	{
		current_substate = (u32)eStateUnknown;
		prev_substate = current_substate;

		Fvector dir_xz = object->Direction();
		dir_xz.y = 0;
		Fvector self_to_enemy_xz = enemy->Position() - object->Position();
		self_to_enemy_xz.y = 0;

		float const angle = angle_between_vectors(dir_xz, self_to_enemy_xz);

		if (_abs(angle) > deg2rad(30.f))
		{
			bool const rotate_right = object->control().direction().is_from_right(enemy->Position());
			object->anim().set_override_animation(rotate_right ?
				eAnimStandTurnRight : eAnimStandTurnLeft, 0);
			object->dir().face_target(enemy);
		}

		object->set_action(ACT_STAND_IDLE);
		return;
	}

	select_state(state_id);
	get_state_current()->execute();
	prev_substate = current_substate;
}

void CStateControllerAttack::setup_substates()
{
	inherited::setup_substates();
}

void CStateControllerAttack::check_force_state()
{
	inherited::check_force_state();
}

void CStateControllerAttack::finalize()
{
	inherited::finalize();
}

void CStateControllerAttack::critical_finalize()
{
	inherited::critical_finalize();
}

#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "sound_player.h"
#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "zombie.h"
#include "zombie_state_attack_run.h"

CStateZombieAttackRun::CStateZombieAttackRun(CBaseMonster* object) : inherited(object) 
{
	action = {};
	m_time_action_change = {};
}

CStateZombieAttackRun::~CStateZombieAttackRun() 
{

}

void CStateZombieAttackRun::initialize()
{
	inherited::initialize();
	m_time_action_change = 0;
	action = ACT_WALK_FWD;

	object->path().prepare_builder();
}

void CStateZombieAttackRun::execute()
{
	float dist = object->EnemyMan.get_enemy()->Position().distance_to(object->Position());

	object->path().set_try_min_time(false);

	object->path().set_target_point(object->EnemyMan.get_enemy_position(), object->EnemyMan.get_enemy_vertex());
	object->path().set_rebuild_time(100 + u32(50.f * dist));
	object->path().set_distance_to_end(2.5f);
	object->path().set_use_covers(false);

	CMonsterSquad* squad = monster_squad().get_squad(object);
	bool squad_active = squad && squad->SquadActive();

	SSquadCommand command{};
	squad->GetCommand(object, command);

	if (!squad_active || (command.type != SC_ATTACK)) 
		squad_active = false;

	if (squad_active) 
	{
		object->path().set_use_dest_orient(true);
		object->path().set_dest_direction(command.direction);
	}
	else
		object->path().set_use_dest_orient(false);

	choose_action();
	object->anim().m_tAction = action;

	if (action == ACT_RUN)
		object->path().set_try_min_time(true);

	object->sound().play(MonsterSound::eMonsterSoundAggressive, 0, 0, object->db().m_dwAttackSndDelay);
	object->anim().accel_activate(eAT_Aggressive);
	object->anim().accel_set_braking(false);
}

bool CStateZombieAttackRun::check_completion()
{
	float m_fDistMin = object->MeleeChecker.get_min_distance();
	float dist = object->MeleeChecker.distance_to_enemy(object->EnemyMan.get_enemy());

	if (dist < m_fDistMin)	
		return true;

	return false;
}

bool CStateZombieAttackRun::check_start_conditions()
{
	float m_fDistMax = object->MeleeChecker.get_max_distance();
	float dist = object->MeleeChecker.distance_to_enemy(object->EnemyMan.get_enemy());

	if (dist > m_fDistMax)	
		return true;

	return false;
}

void CStateZombieAttackRun::choose_action()
{
	action = object->HitMemory.is_hit() ? ACT_RUN : ACT_WALK_FWD;
}
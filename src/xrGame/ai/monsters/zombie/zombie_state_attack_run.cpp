#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "sound_player.h"
#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "zombie.h"
#include "zombie_state_attack_run.h"

CStateZombieAttackRun::CStateZombieAttackRun(CBaseMonster* obj) : inherited(obj)
{
	
}


CStateZombieAttackRun::~CStateZombieAttackRun()
{
}


void CStateZombieAttackRun::initialize()
{
	inherited::initialize();
	m_time_action_change = 0;
	action = ACT_WALK_FWD;

	this->object->path().prepare_builder();
}


void CStateZombieAttackRun::execute()
{
	float dist = this->object->EnemyMan.get_enemy()->Position().distance_to(this->object->Position());

	this->object->path().set_try_min_time(false);

	// установка параметров функциональных блоков
	this->object->path().set_target_point(this->object->EnemyMan.get_enemy_position(), this->object->EnemyMan.get_enemy_vertex());
	this->object->path().set_rebuild_time(100 + u32(50.f * dist));
	this->object->path().set_distance_to_end(2.5f);
	this->object->path().set_use_covers(false);

	//////////////////////////////////////////////////////////////////////////
	// обработать squad-данные
	//////////////////////////////////////////////////////////////////////////
	CMonsterSquad* squad = monster_squad().get_squad(this->object);
	bool squad_active = squad && squad->SquadActive();

	// Получить команду
	SSquadCommand command;
	squad->GetCommand(this->object, command);
	if (!squad_active || (command.type != SC_ATTACK)) squad_active = false;
	//////////////////////////////////////////////////////////////////////////

	if (squad_active) {
		this->object->path().set_use_dest_orient(true);
		this->object->path().set_dest_direction(command.direction);
	}
	else
		this->object->path().set_use_dest_orient(false);

	choose_action();
	this->object->anim().m_tAction = action;

	if (action == ACT_RUN)
		this->object->path().set_try_min_time(true);

	this->object->sound().play(MonsterSound::eMonsterSoundAggressive, 0, 0, this->object->db().m_dwAttackSndDelay);
	this->object->anim().accel_activate(eAT_Aggressive);
	this->object->anim().accel_set_braking(false);
}


bool CStateZombieAttackRun::check_completion()
{
	float m_fDistMin = this->object->MeleeChecker.get_min_distance();
	float dist = this->object->MeleeChecker.distance_to_enemy(this->object->EnemyMan.get_enemy());

	if (dist < m_fDistMin)	return true;

	return false;
}


bool CStateZombieAttackRun::check_start_conditions()
{
	float m_fDistMax = this->object->MeleeChecker.get_max_distance();
	float dist = this->object->MeleeChecker.distance_to_enemy(this->object->EnemyMan.get_enemy());

	if (dist > m_fDistMax)	return true;

	return false;
}

#define CHANGE_ACTION_FROM_RUN	10000


void CStateZombieAttackRun::choose_action()
{
	action = this->object->HitMemory.is_hit() ? ACT_RUN : ACT_WALK_FWD;
}
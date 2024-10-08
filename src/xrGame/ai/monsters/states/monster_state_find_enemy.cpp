#include "StdAfx.h"
#include "monster_state_find_enemy.h"

#include "monster_state_find_enemy_run.h"
#include "monster_state_find_enemy_angry.h"
#include "monster_state_find_enemy_walk.h"
#include "monster_state_find_enemy_look.h"


CStateMonsterFindEnemy::CStateMonsterFindEnemy(CBaseMonster* obj) : inherited(obj)
{
    this->add_state(eStateFindEnemy_Run, new CStateMonsterFindEnemyRun(obj));
    this->add_state(eStateFindEnemy_LookAround, new CStateMonsterFindEnemyLook(obj));
    this->add_state(eStateFindEnemy_Angry, new CStateMonsterFindEnemyAngry(obj));
    this->add_state(eStateFindEnemy_WalkAround, new CStateMonsterFindEnemyWalkAround(obj));
}



CStateMonsterFindEnemy::~CStateMonsterFindEnemy()
{
}


void CStateMonsterFindEnemy::reselect_state()
{
	if (this->prev_substate == u32(-1)) {
		this->select_state(eStateFindEnemy_Run);
		return;
	}

	switch (this->prev_substate) {
	case eStateFindEnemy_Run:			this->select_state(eStateFindEnemy_LookAround);	break;
	case eStateFindEnemy_LookAround:	this->select_state(eStateFindEnemy_Angry);		break;
	case eStateFindEnemy_Angry:			this->select_state(eStateFindEnemy_WalkAround);	break;
	case eStateFindEnemy_WalkAround:	this->select_state(eStateFindEnemy_WalkAround);	break;
	}
}


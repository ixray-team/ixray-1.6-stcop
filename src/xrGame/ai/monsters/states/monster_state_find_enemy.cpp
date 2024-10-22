#include "StdAfx.h"
#include "monster_state_find_enemy.h"

#include "monster_state_find_enemy_run.h"
#include "monster_state_find_enemy_angry.h"
#include "monster_state_find_enemy_walk.h"
#include "monster_state_find_enemy_look.h"


CStateMonsterFindEnemy::CStateMonsterFindEnemy(CBaseMonster* obj) : inherited(obj)
{
    add_state(eStateFindEnemy_Run, new CStateMonsterFindEnemyRun(obj));
    add_state(eStateFindEnemy_LookAround, new CStateMonsterFindEnemyLook(obj));
    add_state(eStateFindEnemy_Angry, new CStateMonsterFindEnemyAngry(obj));
    add_state(eStateFindEnemy_WalkAround, new CStateMonsterFindEnemyWalkAround(obj));
}



CStateMonsterFindEnemy::~CStateMonsterFindEnemy()
{
}


void CStateMonsterFindEnemy::reselect_state()
{
	if (prev_substate == u32(-1)) {
		select_state(eStateFindEnemy_Run);
		return;
	}

	switch (prev_substate) {
	case eStateFindEnemy_Run:			select_state(eStateFindEnemy_LookAround);	break;
	case eStateFindEnemy_LookAround:	select_state(eStateFindEnemy_Angry);		break;
	case eStateFindEnemy_Angry:			select_state(eStateFindEnemy_WalkAround);	break;
	case eStateFindEnemy_WalkAround:	select_state(eStateFindEnemy_WalkAround);	break;
	}
}


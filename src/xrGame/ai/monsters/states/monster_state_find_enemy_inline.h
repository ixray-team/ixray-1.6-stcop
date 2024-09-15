#pragma once

#include "monster_state_find_enemy_run.h"
#include "monster_state_find_enemy_angry.h"
#include "monster_state_find_enemy_walk.h"
#include "monster_state_find_enemy_look.h"


CStateMonsterFindEnemy::CStateMonsterFindEnemy(CBaseMonster *obj) : inherited(obj)
{
	this->add_state	(eStateFindEnemy_Run,			xr_new<CStateMonsterFindEnemyRun<CBaseMonster> >			(obj));
	this->add_state	(eStateFindEnemy_LookAround,	xr_new<CStateMonsterFindEnemyLook<CBaseMonster> >		(obj));
	this->add_state	(eStateFindEnemy_Angry,			xr_new<CStateMonsterFindEnemyAngry<CBaseMonster> >		(obj));
	this->add_state	(eStateFindEnemy_WalkAround,	xr_new<CStateMonsterFindEnemyWalkAround<CBaseMonster> >	(obj));
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
	
	switch (this->prev_substate)	{
		case eStateFindEnemy_Run:			this->select_state(eStateFindEnemy_LookAround);	break;
		case eStateFindEnemy_LookAround:	this->select_state(eStateFindEnemy_Angry);		break;
		case eStateFindEnemy_Angry:			this->select_state(eStateFindEnemy_WalkAround);	break;
		case eStateFindEnemy_WalkAround:	this->select_state(eStateFindEnemy_WalkAround);	break;
	}
}


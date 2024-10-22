#include "StdAfx.h"
#include "monster_state_rest_sleep.h"



CStateMonsterRestSleep::CStateMonsterRestSleep(CBaseMonster* obj) : inherited(obj)
{
}


CStateMonsterRestSleep::~CStateMonsterRestSleep()
{
}


void CStateMonsterRestSleep::initialize()
{
	inherited::initialize();
	object->fall_asleep();
}


void CStateMonsterRestSleep::execute()
{
	object->set_action(ACT_SLEEP);
	object->set_state_sound(MonsterSound::eMonsterSoundIdle);
}


void CStateMonsterRestSleep::finalize()
{
	inherited::finalize();
	object->wake_up();
}


void CStateMonsterRestSleep::critical_finalize()
{
	inherited::critical_finalize();
	object->wake_up();
}


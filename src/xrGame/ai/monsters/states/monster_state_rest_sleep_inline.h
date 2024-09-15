#pragma once


CStateMonsterRestSleep::CStateMonsterRestSleep(CBaseMonster *obj) : inherited(obj)
{
}


CStateMonsterRestSleep::~CStateMonsterRestSleep	()
{
}


void CStateMonsterRestSleep::initialize()
{
	inherited::initialize	();
	this->object->fall_asleep		();
}


void CStateMonsterRestSleep::execute()
{
	this->object->set_action				(ACT_SLEEP);
	this->object->set_state_sound			(MonsterSound::eMonsterSoundIdle);	
}


void CStateMonsterRestSleep::finalize()
{
	inherited::finalize	();
	this->object->wake_up		();
}


void CStateMonsterRestSleep::critical_finalize()
{
	inherited::critical_finalize	();
	this->object->wake_up					();
}


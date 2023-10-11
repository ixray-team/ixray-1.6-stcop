#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>
#define CStateChimeraThreatenRoarAbstract CStateChimeraThreatenRoar<_Object>

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenRoarAbstract::initialize()
{
	inherited::initialize	();

}

TEMPLATE_SPECIALIZATION
void CStateChimeraThreatenRoarAbstract::execute()
{

	this->object->set_action			(ACT_STAND_IDLE);
	this->object->anim().SetSpecParams	(ASP_THREATEN);
	this->object->set_state_sound		(MonsterSound::eMonsterSoundThreaten);
	this->object->dir().face_target		(this->object->EnemyMan.get_enemy(), 1200);
}

#define STATE_TIME_OUT	4000

TEMPLATE_SPECIALIZATION
bool CStateChimeraThreatenRoarAbstract::check_completion()
{	
	if (this->time_state_started + STATE_TIME_OUT < Device.dwTimeGlobal) return true;
	return false;
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateChimeraThreatenRoarAbstract


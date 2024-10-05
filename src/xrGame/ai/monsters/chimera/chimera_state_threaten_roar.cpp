#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "chimera_state_threaten_roar.h"

void CStateChimeraThreatenRoar::initialize()
{
	inherited::initialize();

}


void CStateChimeraThreatenRoar::execute()
{

	this->object->set_action(ACT_STAND_IDLE);
	this->object->anim().SetSpecParams(ASP_THREATEN);
	this->object->set_state_sound(MonsterSound::eMonsterSoundThreaten);
	this->object->dir().face_target(this->object->EnemyMan.get_enemy(), 1200);
}

#define STATE_TIME_OUT	4000


bool CStateChimeraThreatenRoar::check_completion()
{
	if (this->time_state_started + STATE_TIME_OUT < Device.dwTimeGlobal) return true;
	return false;
}
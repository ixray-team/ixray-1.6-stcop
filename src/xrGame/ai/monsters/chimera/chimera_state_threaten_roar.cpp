#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "chimera_state_threaten_roar.h"

CStateChimeraThreatenRoar::CStateChimeraThreatenRoar(CBaseMonster* object) : inherited(object) 
{

}

CStateChimeraThreatenRoar::~CStateChimeraThreatenRoar()
{

}

void CStateChimeraThreatenRoar::initialize()
{
	inherited::initialize();
}

void CStateChimeraThreatenRoar::execute()
{
	object->set_action(ACT_STAND_IDLE);
	object->anim().SetSpecParams(ASP_THREATEN);
	object->set_state_sound(MonsterSound::eMonsterSoundThreaten);
	object->dir().face_target(this->object->EnemyMan.get_enemy(), 1200);
}

bool CStateChimeraThreatenRoar::check_completion()
{
	if (this->time_state_started + EntityDefinitions::CChimeraBase::STATE_TIME_OUT < Device.dwTimeGlobal) return true;
	return false;
}
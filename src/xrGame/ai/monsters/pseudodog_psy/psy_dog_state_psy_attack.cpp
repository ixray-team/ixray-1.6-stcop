#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "sound_player.h"

#include "psy_dog_state_psy_attack.h"
#include "psy_dog_state_psy_attack_hide.h"

CStatePsyDogPsyAttack::CStatePsyDogPsyAttack(CBaseMonster* obj) : inherited(obj)
{
	this->add_state(eStateAttack_HideInCover, xr_new<CStatePsyDogHide>(obj));
}

void CStatePsyDogPsyAttack::reselect_state()
{
	this->select_state(eStateAttack_HideInCover);
}
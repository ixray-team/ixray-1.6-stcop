#pragma once

#include "psy_dog_state_psy_attack_hide.h"


CStatePsyDogPsyAttack::CStatePsyDogPsyAttack(CBaseMonster*obj) : inherited(obj)
{
	this->add_state	(eStateAttack_HideInCover,	xr_new<CStatePsyDogHide>	(obj));
}

void CStatePsyDogPsyAttack::reselect_state()
{
	this->select_state(eStateAttack_HideInCover);
}

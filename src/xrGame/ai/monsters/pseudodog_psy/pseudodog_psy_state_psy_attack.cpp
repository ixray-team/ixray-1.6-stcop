#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "sound_player.h"

#include "pseudodog_psy_state_psy_attack.h"
#include "pseudodog_psy_state_psy_attack_hide.h"

CStatePsyDogPsyAttack::CStatePsyDogPsyAttack(CBaseMonster* obj) : inherited(obj)
{
	add_state(eStateAttack_HideInCover, new CStatePsyDogHide(obj));
}

void CStatePsyDogPsyAttack::reselect_state()
{
	select_state(eStateAttack_HideInCover);
}
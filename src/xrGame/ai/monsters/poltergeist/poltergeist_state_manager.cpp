#include "stdafx.h"
#include "poltergeist.h"
#include "poltergeist_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "poltergeist_state_rest.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_panic.h"
#include "poltergeist_state_attack_hidden.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "../../../entitycondition.h"

CPoltergeistBaseStateManager::CPoltergeistBaseStateManager(CPoltergeistBase* object) : inherited(object)
{
	pPoltergeistBase = smart_cast<CPoltergeistBase*>(object);

	time_next_flame_attack = {};
	time_next_tele_attack = {};
	time_next_scare_attack = {};

    add_state(eStateRest, new CPoltergeistStateRest(object));
    add_state(eStateEat, new CStateMonsterEat(object));
    add_state(eStateAttack_AttackHidden, new CStatePoltergeistAttackHidden(object));
    add_state(eStatePanic, new CStateMonsterPanic(object));
    add_state(eStateHitted, new CStateMonsterHitted(object));
    add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
    add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound(object));
}

CPoltergeistBaseStateManager::~CPoltergeistBaseStateManager()
{

}

void CPoltergeistBaseStateManager::reinit()
{
	inherited::reinit();
	
	time_next_flame_attack	= 0;
	time_next_tele_attack	= 0;
	time_next_scare_attack	= 0;

}

void CPoltergeistBaseStateManager::execute()
{
	u32 state_id = u32(-1);

	if ( object->EnemyMan.get_enemy() && pPoltergeistBase->detected_enemy() )
	{
		state_id = eStateAttack_AttackHidden;
	}
	else
	{
		state_id = eStateRest;
	}

	select_state(state_id); 

	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

#include "stdafx.h"
#include "controller.h"
#include "controller_state_manager.h"

#include "controller_animation.h"
#include "controller_direction.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../controlled_entity.h"

#include "../states/monster_state_rest.h"
#include "controller_state_attack.h"
#include "../states/monster_state_attack_melee.h"
#include "../states/monster_state_attack_run.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_panic.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "../states/monster_state_attack.h"

#include "../../../entitycondition.h"

#include "controller_state_attack_hide.h"


CControllerBaseStateManager::CControllerBaseStateManager(CControllerBase* object) : inherited(object)
{
    pControllerBase = smart_cast<CControllerBase*>(object);

    add_state(eStateRest, new CStateMonsterRest(object));
    add_state(eStatePanic, new CStateMonsterPanic(object));
    add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
    add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound(object));
    add_state(eStateHitted, new CStateMonsterHitted(object));
    add_state(eStateAttack, new CStateControllerAttack(object));

    add_state(eStateEat, new CStateMonsterEat(object));
    add_state(eStateCustom, new CStateControlHide(object));
}

bool   CControllerBaseStateManager::check_control_start_conditions	(ControlCom::EControlType type)
{
	if ( type == ControlCom::eAntiAim )
	{		
		return current_substate == eStateAttack_Run;
	}

	return false;
}

CControllerBaseStateManager::~CControllerBaseStateManager()
{
}

void CControllerBaseStateManager::reinit()
{
	inherited::reinit();
	pControllerBase->set_mental_state(CControllerBase::eStateIdle);
}

void CControllerBaseStateManager::execute()
{
	u32 state_id = u32(-1);
		
	const CEntityAlive* enemy	= object->EnemyMan.get_enemy();

	if (enemy) {
		state_id = eStateAttack;
	} else if (object->HitMemory.is_hit()) {
		state_id = eStateHitted;
	} else if (object->hear_interesting_sound) {
		state_id = eStateHearInterestingSound;
	} else if (object->hear_dangerous_sound) {
		state_id = eStateHearDangerousSound;	
	} else {
		if (can_eat())	state_id = eStateEat;
		else			state_id = eStateRest;
	}

	select_state(state_id); 

	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

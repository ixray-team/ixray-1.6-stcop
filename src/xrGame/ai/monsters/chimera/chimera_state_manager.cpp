#include "stdafx.h"
#include "chimera.h"
#include "chimera_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../states/monster_state_rest.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_panic.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "chimera_state_threaten.h"
//#include "../states/state_test_state.h"

#include "chimera_attack_state.h"

CChimeraBaseStateManager::CChimeraBaseStateManager(CChimeraBase* object) : inherited(object)
{
    add_state(eStateRest, new CStateMonsterRest(object));
    add_state(eStatePanic, new CStateMonsterPanic(object));
    add_state(eStateAttack, new ChimeraAttackState(object));
    add_state(eStateEat, new CStateMonsterEat(object));
    add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
    add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound(object));
}

CChimeraBaseStateManager::~CChimeraBaseStateManager()
{

}

void CChimeraBaseStateManager::execute()
{
	u32 state_id = u32(-1);

	const CEntityAlive* enemy	= object->EnemyMan.get_enemy	();

	if (enemy) {
		//if (check_state(eStateThreaten)) state_id = eStateThreaten;
		switch (object->EnemyMan.get_danger_type()) {
			case eStrong:	state_id = eStatePanic; break;
			case eWeak:		state_id = eStateAttack; break;
		}
// 	else if (object->HitMemory.is_hit()) {
// 		state_id = eStateHitted;
 	} else if (object->hear_dangerous_sound) {
 		state_id = eStateHearDangerousSound;
 	} else if (object->hear_interesting_sound) {
 		state_id = eStateHearInterestingSound;
	} else {
		if (can_eat())	state_id = eStateEat;
		else			state_id = eStateRest;
	}
	
	//state_id = eStateCustom;

	select_state(state_id); 

	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}


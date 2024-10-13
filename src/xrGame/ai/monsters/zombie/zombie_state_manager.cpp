#include "stdafx.h"
#include "zombie.h"
#include "zombie_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../states/monster_state_rest.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_hear_int_sound.h"
#include "zombie_state_attack_run.h"
#include "../../../entitycondition.h"
#include "../../../detail_path_manager.h"
#include "../states/monster_state_controlled.h"
#include "../states/monster_state_help_sound.h"

#include "../states/monster_state_attack_melee.h"

CZombieBaseStateManager::CZombieBaseStateManager(CBaseMonster* object) : inherited(object)
{
	pZombieBase = smart_cast<CZombieBase*>(object);

    add_state(eStateRest, new CStateMonsterRest(object));
    add_state(eStateAttack, new CStateMonsterAttack(object, 
		new CStateZombieAttackRun(object), new CStateMonsterAttackMelee(object)));

    add_state(eStateEat, new CStateMonsterEat(object));
    add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
    add_state(eStateControlled, new CStateMonsterControlled(object));
    add_state(eStateHearHelpSound, new CStateMonsterHearHelpSound(object));
}

CZombieBaseStateManager::~CZombieBaseStateManager() 
{

}

void CZombieBaseStateManager::execute()
{
	if (object->com_man().ta_is_active()) return;
	
	u32 state_id = u32(-1);
	
	if (!pZombieBase->is_under_control())
	{
		const CEntityAlive* enemy	= object->EnemyMan.get_enemy();

		if (enemy) 
		{
			state_id = eStateAttack;
		} 
		else if (check_state(eStateHearHelpSound)) 
		{
			state_id = eStateHearHelpSound;
		} 
		else if (object->hear_interesting_sound || object->hear_dangerous_sound) 
		{
			state_id = eStateHearInterestingSound;
		} 
		else 
		{
			if (can_eat())	
				state_id = eStateEat;
			else			
				state_id = eStateRest;
		}
	} else state_id = eStateControlled;

	select_state(state_id); 

	get_state_current()->execute();

	prev_substate = current_substate;
}


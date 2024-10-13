#include "stdafx.h"
#include "snork.h"
#include "snork_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../../../level.h"
#include "../../../level_debug.h"
#include "../states/monster_state_rest.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_panic.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "../states/state_look_point.h"
#include "../states/monster_state_help_sound.h"

#include "../../../entitycondition.h"

CSnorkBaseStateManager::CSnorkBaseStateManager(CBaseMonster* object) : inherited(object)
{
	pSnorkBase = smart_cast<CSnorkBase*>(object);

    add_state(eStateRest, new CStateMonsterRest(object));
    add_state(eStatePanic, new CStateMonsterPanic(object));
    add_state(eStateAttack, new CStateMonsterAttack(object));
    add_state(eStateEat, new CStateMonsterEat(object));
    add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
    add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound(object));
    add_state(eStateHitted, new CStateMonsterHitted(object));

    add_state(eStateHearHelpSound, new CStateMonsterHearHelpSound(object));
}

CSnorkBaseStateManager::~CSnorkBaseStateManager()
{

}

void CSnorkBaseStateManager::execute()
{
	u32 state_id = u32(-1);

	const CEntityAlive* enemy	= object->EnemyMan.get_enemy();

	if (enemy) 
	{
		switch (object->EnemyMan.get_danger_type()) 
		{
			case eStrong:	
				state_id = eStatePanic; 
				break;
			case eWeak:		
				state_id = eStateAttack; 
				break;
		}
	} 
	else if (object->HitMemory.is_hit()) 
	{
		state_id = eStateHitted;
	} 
	else if (check_state(eStateHearHelpSound)) 
	{
		state_id = eStateHearHelpSound;
	}
	else if (object->hear_dangerous_sound) 
	{
		state_id = eStateHearDangerousSound;

	} 
	else if (object->hear_interesting_sound)
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

	select_state(state_id); 

	if ((current_substate == eStateAttack) && (current_substate != prev_substate)) 
	{
		pSnorkBase->start_threaten = true;
	}

	get_state_current()->execute();

	prev_substate = current_substate;
}


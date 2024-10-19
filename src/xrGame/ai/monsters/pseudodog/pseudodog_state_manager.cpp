#include "stdafx.h"
#include "pseudodog.h"
#include "pseudodog_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../../../actor.h"
#include "../../stalker/ai_stalker.h"
#include "../states/monster_state_rest.h"
#include "../states/monster_state_attack.h"
#include "../states/monster_state_panic.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"

#include "../states/monster_state_home_point_attack.h"

CStateManagerPseudodog::CStateManagerPseudodog(CBaseMonster* object) : inherited(object)
{
    add_state(eStateRest, new CStateMonsterRest(object));
    add_state(eStatePanic, new CStateMonsterPanic(object));

    CStateMonsterAttackMoveToHomePoint* move2home =
        new CStateMonsterAttackMoveToHomePoint(object);

    add_state(eStateAttack, new CStateMonsterAttack(object, move2home));

    add_state(eStateEat, new CStateMonsterEat(object));
    add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
    add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound(object));
    add_state(eStateHitted, new CStateMonsterHitted(object));
}

CStateManagerPseudodog::~CStateManagerPseudodog()
{

}

void CStateManagerPseudodog::execute()
{
	u32 state_id = u32(-1);

	const CEntityAlive* enemy	= object->EnemyMan.get_enemy();

	if (enemy) 
	{
		switch (object->EnemyMan.get_danger_type()) 
		{
				case eStrong:	state_id = eStatePanic; break;
				case eWeak:		state_id = eStateAttack; break;
		}
	} 
	else if (object->HitMemory.is_hit()) 
	{
		state_id = eStateHitted;
	} 
	else if (object->hear_interesting_sound) 
	{
		state_id = eStateHearInterestingSound;
	} 
	else if (object->hear_dangerous_sound) 
	{
		state_id = eStateHearDangerousSound;	
	} 
	else 
	{
		if (can_eat())	
			state_id = eStateEat;
		else			
			state_id = eStateRest;
	}

	select_state(state_id); 

	get_state_current()->execute();

	prev_substate = current_substate;
}


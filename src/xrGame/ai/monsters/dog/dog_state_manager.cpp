#include "stdafx.h"

#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "../monster_home.h"

#include "dog.h"
#include "dog_state_manager.h"
#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hitted.h"
#include "../states/monster_state_controlled.h"
#include "../states/monster_state_help_sound.h"
#include "../group_states/group_state_attack.h"
#include "../group_states/group_state_rest.h"
#include "../group_states/group_state_eat.h"
#include "../group_states/group_state_panic.h"
#include "../group_states/group_state_hear_danger_sound.h"

CDogBaseStateManager::CDogBaseStateManager(CDogBase* object) : inherited(object)
{
	pDogBase = smart_cast<CDogBase*>(object);

	add_state(eStateRest, new CStateGroupRest(object));
	add_state(eStatePanic, new CStateGroupPanic(object));
	add_state(eStateAttack, new CStateGroupAttack(object));
	add_state(eStateEat, new CStateGroupEat(object));
	add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound(object));
	add_state(eStateHearDangerousSound, new CStateGroupHearDangerousSound(object));
	add_state(eStateHitted, new CStateMonsterHitted(object));
	add_state(eStateControlled, new CStateMonsterControlled(object));
	add_state(eStateHearHelpSound, new CStateMonsterHearHelpSound(object));

	object->EatedCorpse = nullptr;
}

CDogBaseStateManager::~CDogBaseStateManager()
{

}

void CDogBaseStateManager::execute()
{
	u32   state_id = u32(-1);

	const float atack_decision_maxdist = 6.f;

	CMonsterSquad* squad = monster_squad().get_squad(object);

	const CEntityAlive* enemy = object->EnemyMan.get_enemy();

	bool atack = false;
	if ( enemy )
	{
		const Fvector3& enemy_pos = enemy->Position();

		if ( squad )
		{
			if ( object->Home->at_min_home(enemy_pos) )
			{
				squad->set_home_in_danger();
			}

			if ( object->Position().distance_to(enemy_pos) < atack_decision_maxdist )
			{
				squad->set_home_in_danger();
			}

			if ( squad->home_in_danger() )
			{
				atack = true;
			}
		}

		if ( object->Home->at_mid_home(enemy_pos) )
		{
			atack = true;
		}
	}

	if ( !pDogBase->is_under_control() )
	{
		if ( atack )
		{
			CMonsterSquad* squad_ = monster_squad().get_squad(object);
			switch ( object->EnemyMan.get_danger_type() ) 
			{
				case eStrong: state_id = eStatePanic;  break;
				case eWeak:   state_id = eStateAttack; break;
			}
			if ( state_id == eStatePanic && squad_->squad_alife_count() > 2 )
			{
				state_id = eStateAttack;
			}
		} 
		else if ( object->HitMemory.is_hit() )
		{
			// only inform squad of new hit (made not later then after 1 sec)
			if ( current_substate != eStateHitted && 
				 time() < object->HitMemory.get_last_hit_time()+1000 )
			{
				if ( squad )
				{
					squad->set_home_in_danger();
				}				
			}

			state_id = eStateHitted;			
		} 
		else if ( check_state(eStateHearHelpSound) )
		{
			state_id = eStateHearHelpSound;
		} 
		else if ( object->hear_interesting_sound )
		{
			state_id = eStateHearInterestingSound;
		}
		else if ( object->hear_dangerous_sound )
		{
			//comment by Lain: || monster_squad().get_squad(object)->GetCommand(object).type == SC_REST) {
			state_id = eStateHearDangerousSound;	
		} 
		else
		{
			if (pDogBase->get_custom_anim_state() )
			{
				return; 
			}

			if ( check_eat() )	
			{
				state_id = eStateEat;
				if (!object->EatedCorpse)
				{
					object->EatedCorpse = object->CorpseMan.get_corpse();
					const_cast<CEntityAlive *>(object->EatedCorpse)->set_lock_corpse(true);
				}
			}
			else 
			{
				state_id = eStateRest;
			}
		}
	}
	else 
	{
		state_id = eStateControlled;
	}

	select_state(state_id); 

	if ( prev_substate != current_substate && pDogBase->get_custom_anim_state() )
	{
		pDogBase->anim_end_reinit();
	}

	if ( prev_substate == eStateEat && current_substate != eStateEat )
	{
		if ( object->character_physics_support()->movement()->PHCapture() )
		{
			object->character_physics_support()->movement()->PHReleaseObject();
		}
	}

	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

bool CDogBaseStateManager::check_eat ()
{
	if ( !object->CorpseMan.get_corpse() )
	{
		if ( !object->EatedCorpse )
		{
			return false;
		}
	}

	return inherited::check_state(eStateEat);
}

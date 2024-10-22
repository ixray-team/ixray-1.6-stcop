#include "stdafx.h"

#include "bloodsucker.h"
#include "bloodsucker_state_manager.h"

#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"

#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "../xrPhysics/IPHCapture.h"

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

#include "bloodsucker_vampire.h"
#include "bloodsucker_predator.h"
#include "bloodsucker_state_capture_jump.h"
#include "bloodsucker_attack_state.h"
#include "bloodsucker_vampire_execute.h"

CBloodsuckerBaseStateManager::CBloodsuckerBaseStateManager(CBloodsuckerBase *object) : inherited(object)
{
	pBloodsuckerBase = smart_cast<CBloodsuckerBase*>(object);

	add_state(eStateRest,					new CStateMonsterRest					(object));
	add_state(eStatePanic,					new CStateMonsterPanic				(object));
											    
	add_state(eStateAttack,					new CStateMonsterAttack						(object));
											    
	add_state(eStateEat,					new CStateMonsterEat					(object));
	add_state(eStateHearInterestingSound,	new CStateMonsterHearInterestingSound	(object));
	add_state(eStateHearDangerousSound,		new CStateMonsterHearDangerousSound	(object));
	add_state(eStateHitted,					new CStateMonsterHitted				(object));
	add_state(eStateVampire_Execute,		new CustomBloodsuckerStateVampireExecute	(object));
}

CBloodsuckerBaseStateManager::~CBloodsuckerBaseStateManager()
{

}

void CBloodsuckerBaseStateManager::drag_object()
{
	CEntityAlive* const ph_obj = pBloodsuckerBase->m_cob;
	if ( !ph_obj )
	{
		return;
	}

	IKinematics* const kinematics = smart_cast<IKinematics*>(ph_obj->Visual());
	if ( !kinematics )
	{
		return;
	}

	CMonsterSquad* const squad = monster_squad().get_squad(object);
	if ( squad )
	{
		squad->lock_corpse(ph_obj);
	}

	{
		const u16 drag_bone = kinematics->LL_BoneID(pBloodsuckerBase->m_str_cel);
		object->character_physics_support()->movement()->PHCaptureObject(ph_obj, drag_bone);
	}

	IPHCapture* const capture = object->character_physics_support()->movement()->PHCapture();

	if ( capture && !capture->Failed() && pBloodsuckerBase->is_animated() )
	{
		pBloodsuckerBase->start_drag();
	}
}

void CBloodsuckerBaseStateManager::update ()
{
	inherited::update();
}

bool CBloodsuckerBaseStateManager::check_vampire()
{
	if ( prev_substate != eStateVampire_Execute )
	{
		if (get_state(eStateVampire_Execute)->check_start_conditions())	return true;
	} 
	else
	{
		if (!get_state(eStateVampire_Execute)->check_completion())		return true;
	}
	return false;
}

void CBloodsuckerBaseStateManager::execute ()
{
	u32 state_id = u32(-1);

	const CEntityAlive* enemy = object->EnemyMan.get_enemy();
	
	if ( enemy ) 
	{
		 if ( check_vampire() ) 
		 {
			state_id = eStateVampire_Execute;
		 } 
		 else 
		 {
			switch ( object->EnemyMan.get_danger_type() )
			{
				case eStrong: state_id = eStatePanic; break;
				case eWeak:	  state_id = eStateAttack; break;
			}
		}
	} 
	else if ( object->HitMemory.is_hit() ) 
	{
		state_id = eStateHitted;
	} 
	else if ( object->hear_interesting_sound )
	{
		state_id = eStateHearInterestingSound;
	} 
	else if ( object->hear_dangerous_sound )
	{
		state_id = eStateHearDangerousSound;
	} 
	else 
	{
		if ( can_eat() ) state_id = eStateEat;
		else			 state_id = eStateRest;
	}

	select_state(state_id); 

	get_state_current()->execute();

	prev_substate = current_substate;
}


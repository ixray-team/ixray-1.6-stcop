#include "stdafx.h"
#include "anomal_pseudo_gigant.h"
#include "anomal_pseudo_gigant_state_manager.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"

#include "../states/monster_state_rest.h"
#include "../states/monster_state_panic.h"
#include "../states/monster_state_eat.h"
#include "../states/monster_state_hear_int_sound.h"
#include "../states/monster_state_hear_danger_sound.h"
#include "../states/monster_state_hitted.h"
#include "../states/state_custom_action.h"
#include "../states/monster_state_controlled.h"
#include "../states/monster_state_help_sound.h"

#include "anomal_pseudo_gigant_state_attack.h"


CStateManagerAnomalPseudoGigant::CStateManagerAnomalPseudoGigant(CAnomalPseudoGigant*monster) : inherited(monster)
{
	add_state(eStateRest, new CStateMonsterRest<CAnomalPseudoGigant>(monster));
	add_state(eStatePanic, new CStateMonsterPanic<CAnomalPseudoGigant>(monster));
	add_state(eStateAttack, new CStateAnomalPseudoGigantAttack<CAnomalPseudoGigant>(monster));
	add_state(eStateEat, new CStateMonsterEat<CAnomalPseudoGigant>(monster));
	add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound<CAnomalPseudoGigant>(monster));
	add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound<CAnomalPseudoGigant>(monster));
	add_state(eStateHitted, new CStateMonsterHitted<CAnomalPseudoGigant>(monster));
	add_state(eStateControlled, new CStateMonsterControlled<CAnomalPseudoGigant>(monster));
	add_state(eStateHearHelpSound, new CStateMonsterHearHelpSound<CAnomalPseudoGigant>(monster));

	/*add_state(eStateRest, xr_new<CStateMonsterRest<CAnomalPseudoGigant> >(monster));
	add_state(eStatePanic,					xr_new<CStateMonsterPanic<CAnomalPseudoGigant> >					(monster));
	add_state(eStateAttack,					xr_new<CStateAnomalPseudoGigantAttack<CAnomalPseudoGigant> >					(monster));
	add_state(eStateEat,					xr_new<CStateMonsterEat<CAnomalPseudoGigant> >					(monster));
	add_state(eStateHearInterestingSound,	xr_new<CStateMonsterHearInterestingSound<CAnomalPseudoGigant> >	(monster));
	add_state(eStateHearDangerousSound,		xr_new<CStateMonsterHearDangerousSound<CAnomalPseudoGigant> >	(monster));
	add_state(eStateHitted,					xr_new<CStateMonsterHitted<CAnomalPseudoGigant> >				(monster));
	add_state(eStateBurerScanning,			xr_new<CStateMonsterCustomAction<CAnomalPseudoGigant> >				(monster));*/
}

#define SCAN_STATE_TIME 4000

void CStateManagerAnomalPseudoGigant::execute()
{
	u32 state_id = static_cast<u32>(-1);

	if (!object->is_under_control()) {
		const CEntityAlive* enemy = object->EnemyMan.get_enemy();

		if (enemy) {
			switch (object->EnemyMan.get_danger_type()) {
			case eStrong:	state_id = eStatePanic; break;
			case eWeak:		state_id = eStateAttack; break;
			}

		}
		else if (object->HitMemory.is_hit()) {
			state_id = eStateHitted;
		}
		else if (check_state(eStateHearHelpSound)) {
			state_id = eStateHearHelpSound;
		}
		else if (object->hear_interesting_sound) {
			state_id = eStateHearInterestingSound;
		}
		else if (object->hear_dangerous_sound) {
			state_id = eStateHearDangerousSound;
		}
		else {
			if (can_eat())	state_id = eStateEat;
			else			state_id = eStateRest;
		}
	}
	else state_id = eStateControlled;

	select_state(state_id);

	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

void CStateManagerAnomalPseudoGigant::setup_substates()
{
	/*if (current_substate == eStateBurerScanning) {
		SStateDataAction	data;
		
		data.action			= ACT_LOOK_AROUND;
		data.sound_type		= MonsterSound::eMonsterSoundIdle;
		data.sound_delay	= object->db().m_dwIdleSndDelay;
		
		get_state_current()->fill_data_with(&data, sizeof(SStateDataAction));
		return;
	}*/
}

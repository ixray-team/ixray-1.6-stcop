#include "stdafx.h"
#include "burer.h"
#include "burer_state_manager.h"

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

#include "burer_state_attack.h"


CStateManagerBurer::CStateManagerBurer(CBurerBase *monster) : inherited(monster)
{
	m_pBurer = smart_cast<CBurerBase*>(monster);

	add_state(eStateRest,					new CStateMonsterRest					(monster));
	add_state(eStatePanic,					new CStateMonsterPanic					(monster));
	add_state(eStateAttack,					new CStateBurerAttack 					(monster));
	add_state(eStateEat,					new CStateMonsterEat 					(monster));
	add_state(eStateHearInterestingSound,	new CStateMonsterHearInterestingSound 	(monster));
	add_state(eStateHearDangerousSound,		new CStateMonsterHearDangerousSound 	(monster));
	add_state(eStateHitted,					new CStateMonsterHitted				(monster));
	add_state(eStateBurerScanning,			new CStateMonsterCustomAction 				(monster));
}

#define SCAN_STATE_TIME 4000

void CStateManagerBurer::execute()
{
	u32 state = u32(-1);

	if (object->EnemyMan.get_enemy()) {
		switch (object->EnemyMan.get_danger_type()) {
				case eStrong:	state = eStatePanic; break;
				case eWeak:		state = eStateAttack; break;
		}
	} else if (object->HitMemory.is_hit() && (object->HitMemory.get_last_hit_time() + 10000 > Device.dwTimeGlobal)) 
		state = eStateHitted;
	 else if (object->hear_interesting_sound)
		state = eStateHearInterestingSound;
	 else if (object->hear_dangerous_sound )
		state = eStateHearDangerousSound;
	 else if (m_pBurer->time_last_scan + SCAN_STATE_TIME > Device.dwTimeGlobal)
		state = eStateBurerScanning;
	 else if (can_eat())
			state = eStateEat;
	 else	state = eStateRest;

	select_state(state); 
	
	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

void CStateManagerBurer::setup_substates()
{
	if (current_substate == eStateBurerScanning) {
		SStateDataAction	data;
		
		data.action			= ACT_LOOK_AROUND;
		data.sound_type		= MonsterSound::eMonsterSoundIdle;
		data.sound_delay	= object->db().m_dwIdleSndDelay;
		
		get_state_current()->fill_data_with(&data, sizeof(SStateDataAction));
		return;
	}
}

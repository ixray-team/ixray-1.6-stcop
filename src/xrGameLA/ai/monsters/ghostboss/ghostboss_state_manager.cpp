#include "stdafx.h"
#include "ghostboss.h"
#include "ghostboss_state_manager.h"

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

#include "GhostBoss_state_attack.h"


CStateManagerGhostBoss::CStateManagerGhostBoss(CGhostBoss *monster) : inherited(monster)
{
	add_state(eStateRest, new CStateMonsterRest<CGhostBoss>(monster));
	add_state(eStatePanic, new CStateMonsterPanic<CGhostBoss>(monster));
	add_state(eStateAttack, new CStateGhostBossAttack<CGhostBoss>(monster));
	add_state(eStateEat, new CStateMonsterEat<CGhostBoss>(monster));
	add_state(eStateHearInterestingSound, new CStateMonsterHearInterestingSound<CGhostBoss>(monster));
	add_state(eStateHearDangerousSound, new CStateMonsterHearDangerousSound<CGhostBoss>(monster));
	add_state(eStateHitted, new CStateMonsterHitted<CGhostBoss>(monster));
	add_state(eStateGhostBossScanning, new CStateMonsterCustomAction<CGhostBoss>(monster));
	add_state(eStateGhostBossAttack_Shield, new CStateGhostBossShield<CGhostBoss>(monster));
	add_state(eStateGhostBossAttack_Teleport, new CStateGhostBossTeleport<CGhostBoss>(monster));
	m_last_health			= 1.f;
}

#define SCAN_STATE_TIME 4000

void CStateManagerGhostBoss::execute()
{
	u32 state = u32(-1);

	bool lost_health = false;
	if (m_last_health - 0.01 > object->GetfHealth())
	{
		m_last_health = object->GetfHealth();
		lost_health = true;
	}

	if (current_substate == eStateGhostBossAttack_Teleport && check_state(eStateGhostBossAttack_Teleport)) {
		state = eStateGhostBossAttack_Teleport;
	} else if (current_substate == eStateGhostBossAttack_Shield && check_state(eStateGhostBossAttack_Shield))
	{
		state = eStateGhostBossAttack_Shield;
	}
	else if	(lost_health) {
		if (object->CanTeleport())
			state = eStateGhostBossAttack_Teleport;
		if (state==u32(-1) && check_state(eStateGhostBossAttack_Shield))
			state = eStateGhostBossAttack_Shield;
	}
	else if (object->EnemyMan.get_enemy()) {
		switch (object->EnemyMan.get_danger_type()) {
				case eStrong:	state = eStatePanic; break;
				case eWeak:		state = eStateAttack; break;
		}
	} else if (object->HitMemory.is_hit() && (object->HitMemory.get_last_hit_time() + 10000 > Device.dwTimeGlobal)) {
		state = eStateHitted;
	} else if (object->hear_dangerous_sound || object->hear_interesting_sound) {
		state = eStateHearInterestingSound;
	} else if (object->time_last_scan + SCAN_STATE_TIME > Device.dwTimeGlobal){
		state = eStateGhostBossScanning;
	} else if (can_eat()) {
			state = eStateEat;
	} else	state = eStateRest;

	select_state(state); 
	
	// выполнить текущее состояние
	get_state_current()->execute();

	prev_substate = current_substate;
}

void CStateManagerGhostBoss::setup_substates()
{
	if (current_substate == eStateGhostBossScanning) {
		SStateDataAction	data;
		
		data.action			= ACT_LOOK_AROUND;
		data.sound_type		= MonsterSound::eMonsterSoundIdle;
		data.sound_delay	= object->db().m_dwIdleSndDelay;
		
		get_state_current()->fill_data_with(&data, sizeof(SStateDataAction));
		return;
	}
}

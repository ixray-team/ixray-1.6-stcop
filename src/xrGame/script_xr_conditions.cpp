#include "Stdafx.h"
#include "script_xr_conditions.h"
#include "script_xr_conditions_functions.cpp"

// set breakpoint on these functions for 'understanding' the sense of usage and the purpose of CAnyCallable
//bool test_adiahfuirhgarughargha(int a, int b) { return false; }
//bool test2_Afjreauhgaeughauegaoegheog(int a, int b, const xr_vector<int>& c) { return false; }

CScriptXRConditionsStorage::CScriptXRConditionsStorage() : m_pLevel{}
{
	/* for test
	REGISTER_FUNCTION_TO_XR_CONDITIONS(test_adiahfuirhgarughargha);
	REGISTER_FUNCTION_TO_XR_CONDITIONS(test2_Afjreauhgaeughauegaoegheog);

	this->getRegisteredFunctionByName("test_adiahfuirhgarughargha")(1,2);
	this->getRegisteredFunctionByName("test2_Afjreauhgaeughauegaoegheog")(1, 2, xr_vector<int>{});
	*/
}

CScriptXRConditionsStorage::~CScriptXRConditionsStorage()
{
}

void CScriptXRConditionsStorage::initialize(CLevel* pLevelManager)
{
	R_ASSERT2(pLevelManager, "you have to have a valid pointer of LevelManager! early calling?");

	m_pLevel = pLevelManager;

	REGISTER_FUNCTION_TO_SCRIPT(fighting_dist_ge);
	REGISTER_FUNCTION_TO_SCRIPT(surge_started);
	REGISTER_FUNCTION_TO_SCRIPT(surge_complete);
	REGISTER_FUNCTION_TO_SCRIPT(surge_kill_all);
	REGISTER_FUNCTION_TO_SCRIPT(signal_rocket_flying);
	REGISTER_FUNCTION_TO_SCRIPT(quest_npc_enemy_actor);
	REGISTER_FUNCTION_TO_SCRIPT(animpoint_reached);
	REGISTER_FUNCTION_TO_SCRIPT(distance_to_obj_ge);
	REGISTER_FUNCTION_TO_SCRIPT(distance_to_obj_le);
	REGISTER_FUNCTION_TO_SCRIPT(in_dest_smart_cover);
	REGISTER_FUNCTION_TO_SCRIPT(active_item);
	REGISTER_FUNCTION_TO_SCRIPT(actor_nomove_nowpn);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b16_is_zone_active);
	REGISTER_FUNCTION_TO_SCRIPT(check_bloodsucker_state);
	REGISTER_FUNCTION_TO_SCRIPT(dist_to_story_obj_ge);
	REGISTER_FUNCTION_TO_SCRIPT(actor_has_nimble_weapon);
	REGISTER_FUNCTION_TO_SCRIPT(actor_has_active_nimble_weapon);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b202_inventory_box_empty);
	REGISTER_FUNCTION_TO_SCRIPT(is_in_danger);
	REGISTER_FUNCTION_TO_SCRIPT(object_exist);
	REGISTER_FUNCTION_TO_SCRIPT(squad_curr_action);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_snork);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_dog);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_psy_dog);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_polter);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_tushkano);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_burer);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_controller);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_flesh);
	REGISTER_FUNCTION_TO_SCRIPT(is_monster_boar);
	REGISTER_FUNCTION_TO_SCRIPT(dead_body_searching);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b47_npc_online);
	REGISTER_FUNCTION_TO_SCRIPT(anomaly_has_artefact);
	REGISTER_FUNCTION_TO_SCRIPT(zat_b29_anomaly_has_af);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b221_who_will_start);
	REGISTER_FUNCTION_TO_SCRIPT(pas_b400_actor_far_forward);
	REGISTER_FUNCTION_TO_SCRIPT(pas_b400_actor_far_backward);
	REGISTER_FUNCTION_TO_SCRIPT(pri_a28_actor_is_far);
	REGISTER_FUNCTION_TO_SCRIPT(check_enemy_smart);
	REGISTER_FUNCTION_TO_SCRIPT(zat_b103_actor_has_needed_food);
	REGISTER_FUNCTION_TO_SCRIPT(zat_b29_rivals_dialog_precond);
	REGISTER_FUNCTION_TO_SCRIPT(poltergeist_get_actor_ignore);
	REGISTER_FUNCTION_TO_SCRIPT(burer_gravi_attack);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b202_actor_treasure_not_in_steal);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b25_senya_spawn_condition);
	REGISTER_FUNCTION_TO_SCRIPT(jup_b25_flint_gone_condition);
	REGISTER_FUNCTION_TO_SCRIPT(check_deimos_phase);
	REGISTER_FUNCTION_TO_SCRIPT(actor_in_surge_cover);
	REGISTER_FUNCTION_TO_SCRIPT(is_door_blocked_by_npc);
	REGISTER_FUNCTION_TO_SCRIPT(has_active_tutorial);
	REGISTER_FUNCTION_TO_SCRIPT(upgrade_hint_kardan);
	REGISTER_FUNCTION_TO_SCRIPT(fighting_dist_le);
	REGISTER_FUNCTION_TO_SCRIPT(enemy_in_zone);
	REGISTER_FUNCTION_TO_SCRIPT(black_screen);
}

void CScriptXRConditionsStorage::destroy()
{
}

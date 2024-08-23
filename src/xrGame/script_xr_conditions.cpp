#include "Stdafx.h"
#include "script_xr_conditions.h"
#include "script_xr_conditions_functions.cpp"

// set breakpoint on these functions for 'understanding' the sense of usage and
// the purpose of CAnyCallable
// bool test_adiahfuirhgarughargha(int a, int b) { return false; }
// bool test2_Afjreauhgaeughauegaoegheog(int a, int b, const xr_vector<int>& c)
// { return false; }

CScriptXRConditionsStorage::CScriptXRConditionsStorage() : m_pLevel{}
{
	/* for test
	REGISTER_FUNCTION_TO_XR_CONDITIONS(test_adiahfuirhgarughargha);
	REGISTER_FUNCTION_TO_XR_CONDITIONS(test2_Afjreauhgaeughauegaoegheog);

	this->getRegisteredFunctionByName("test_adiahfuirhgarughargha")(1,2);
	this->getRegisteredFunctionByName("test2_Afjreauhgaeughauegaoegheog")(1, 2,
	xr_vector<int>{});
	*/
}

CScriptXRConditionsStorage::~CScriptXRConditionsStorage() {}

void CScriptXRConditionsStorage::initialize(CLevel* pLevelManager)
{
	R_ASSERT2(pLevelManager,
		"you have to have a valid pointer of LevelManager! early calling?");

	m_pLevel = pLevelManager;

#if defined(IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION) || defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
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
	REGISTER_FUNCTION_TO_SCRIPT(check_npc_name);
	REGISTER_FUNCTION_TO_SCRIPT(check_enemy_name);
	REGISTER_FUNCTION_TO_SCRIPT(is_playing_sound);
	REGISTER_FUNCTION_TO_SCRIPT(actor_alive);
	REGISTER_FUNCTION_TO_SCRIPT(see_npc);
	REGISTER_FUNCTION_TO_SCRIPT(actor_see_npc);
	REGISTER_FUNCTION_TO_SCRIPT(npc_in_actor_frustum);
	REGISTER_FUNCTION_TO_SCRIPT(is_wounded);
	REGISTER_FUNCTION_TO_SCRIPT(dist_to_actor_le);
	REGISTER_FUNCTION_TO_SCRIPT(dist_to_actor_ge);
	REGISTER_FUNCTION_TO_SCRIPT(is_obj_on_job);
	REGISTER_FUNCTION_TO_SCRIPT(distance_to_obj_on_job_le);
	REGISTER_FUNCTION_TO_SCRIPT(obj_in_zone);
	REGISTER_FUNCTION_TO_SCRIPT(one_obj_in_zone);
	REGISTER_FUNCTION_TO_SCRIPT(story_obj_in_zone_by_name);
	REGISTER_FUNCTION_TO_SCRIPT(actor_in_zone);
	REGISTER_FUNCTION_TO_SCRIPT(npc_in_zone);
	REGISTER_FUNCTION_TO_SCRIPT(health_le);
	REGISTER_FUNCTION_TO_SCRIPT(actor_health_le);
	REGISTER_FUNCTION_TO_SCRIPT(npc_community);
	REGISTER_FUNCTION_TO_SCRIPT(hitted_by);
	REGISTER_FUNCTION_TO_SCRIPT(hitted_on_bone);
	REGISTER_FUNCTION_TO_SCRIPT(best_pistol);
	REGISTER_FUNCTION_TO_SCRIPT(deadly_hit);
	REGISTER_FUNCTION_TO_SCRIPT(killed_by);
	REGISTER_FUNCTION_TO_SCRIPT(is_alive_all);
	REGISTER_FUNCTION_TO_SCRIPT(is_alive_one);
	REGISTER_FUNCTION_TO_SCRIPT(is_alive);
	REGISTER_FUNCTION_TO_SCRIPT(is_dead_all);
	REGISTER_FUNCTION_TO_SCRIPT(is_dead_one);
	REGISTER_FUNCTION_TO_SCRIPT(is_dead);
	REGISTER_FUNCTION_TO_SCRIPT(story_object_exist);
	REGISTER_FUNCTION_TO_SCRIPT(actor_has_item);
	REGISTER_FUNCTION_TO_SCRIPT(npc_has_item);
	REGISTER_FUNCTION_TO_SCRIPT(actor_has_item_count);
	REGISTER_FUNCTION_TO_SCRIPT(signal);
	REGISTER_FUNCTION_TO_SCRIPT(counter_greater);
	REGISTER_FUNCTION_TO_SCRIPT(counter_equal);
	REGISTER_FUNCTION_TO_SCRIPT(check_smart_alarm_status);
	REGISTER_FUNCTION_TO_SCRIPT(has_enemy);
	REGISTER_FUNCTION_TO_SCRIPT(has_actor_enemy);
	REGISTER_FUNCTION_TO_SCRIPT(see_enemy);
	REGISTER_FUNCTION_TO_SCRIPT(has_enemy_in_current_loopholes_fov);
	REGISTER_FUNCTION_TO_SCRIPT(talking);
	REGISTER_FUNCTION_TO_SCRIPT(npc_talking);
	REGISTER_FUNCTION_TO_SCRIPT(see_actor);
	REGISTER_FUNCTION_TO_SCRIPT(actor_enemy);
	REGISTER_FUNCTION_TO_SCRIPT(actor_friend);
	REGISTER_FUNCTION_TO_SCRIPT(actor_neutral);
	REGISTER_FUNCTION_TO_SCRIPT(is_factions_enemies);
	REGISTER_FUNCTION_TO_SCRIPT(is_factions_friends);
	REGISTER_FUNCTION_TO_SCRIPT(is_faction_enemy_to_actor);
	REGISTER_FUNCTION_TO_SCRIPT(is_faction_friend_to_actor);
	REGISTER_FUNCTION_TO_SCRIPT(is_faction_neutral_to_actor);
	REGISTER_FUNCTION_TO_SCRIPT(is_squad_friend_to_actor);
	REGISTER_FUNCTION_TO_SCRIPT(is_squad_enemy_to_actor);
	REGISTER_FUNCTION_TO_SCRIPT(is_squad_neutral_to_actor);
	REGISTER_FUNCTION_TO_SCRIPT(fighting_actor);
	REGISTER_FUNCTION_TO_SCRIPT(hit_by_actor);
	REGISTER_FUNCTION_TO_SCRIPT(killed_by_actor);
	REGISTER_FUNCTION_TO_SCRIPT(actor_has_weapon);
	REGISTER_FUNCTION_TO_SCRIPT(actor_active_detector);
	REGISTER_FUNCTION_TO_SCRIPT(heavy_wounded);
	REGISTER_FUNCTION_TO_SCRIPT(time_period);
	REGISTER_FUNCTION_TO_SCRIPT(is_rain);
	REGISTER_FUNCTION_TO_SCRIPT(is_heavy_rain);
	REGISTER_FUNCTION_TO_SCRIPT(is_day);
	REGISTER_FUNCTION_TO_SCRIPT(is_dark_night);
	REGISTER_FUNCTION_TO_SCRIPT(is_jup_a12_mercs_time);
	REGISTER_FUNCTION_TO_SCRIPT(zat_b7_is_night);
	REGISTER_FUNCTION_TO_SCRIPT(zat_b7_is_late_attack_time);
	REGISTER_FUNCTION_TO_SCRIPT(mob_has_enemy);
	REGISTER_FUNCTION_TO_SCRIPT(mob_was_hit);
	REGISTER_FUNCTION_TO_SCRIPT(actor_on_level);
	REGISTER_FUNCTION_TO_SCRIPT(squad_in_zone);
	REGISTER_FUNCTION_TO_SCRIPT(squad_has_enemy);
	REGISTER_FUNCTION_TO_SCRIPT(squad_in_zone_all);
	REGISTER_FUNCTION_TO_SCRIPT(squads_in_zone_b41);
	REGISTER_FUNCTION_TO_SCRIPT(target_squad_name);
	REGISTER_FUNCTION_TO_SCRIPT(target_smart_name);
	REGISTER_FUNCTION_TO_SCRIPT(squad_exist);
	REGISTER_FUNCTION_TO_SCRIPT(is_squad_commander);
	REGISTER_FUNCTION_TO_SCRIPT(squad_npc_count_ge);
#endif
}

void CScriptXRConditionsStorage::destroy() {}

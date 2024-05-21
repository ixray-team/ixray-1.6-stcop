// FIXME: all this "if (packet.r_xx() != yy) xr_not_implemented()" stuff
#include <cstring>
#include "xr_entity_script.h"
#include "xr_packet.h"

using namespace xray_re;

static void r_ctime(xr_packet& packet)
{
	if (packet.r_u8() == 1)
		xr_not_implemented();
}

static void w_ctime(xr_packet& packet, bool as_nil = false)
{
	packet.w_u8(as_nil ? 0xff : 0);
}

////////////////////////////////////////////////////////////////////////////////

static void r_object_collection(xr_packet& packet)
{
	if (packet.r_u16())		// m_count
		xr_not_implemented();
	if (packet.r_u16())		// m_last_id
		xr_not_implemented();
	if (packet.r_u16())		// m_free
		xr_not_implemented();
	if (packet.r_u16())		// m_given
		xr_not_implemented();
}

static void w_object_collection(xr_packet& packet)
{
	packet.w_u16(0);
	packet.w_u16(0);
	packet.w_u16(0);
	packet.w_u16(0);
}

void se_actor::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_creature_actor::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x80) {
		set_save_marker(packet, SM_LOAD, false, "se_actor");
		
		bool start_position_filled = packet.r_bool();
		
		set_save_marker(packet, SM_LOAD, true, "se_actor");
	}
	else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);

		set_save_marker(packet, SM_LOAD, false, "se_actor");

		// CRandomTask
		set_save_marker(packet, SM_LOAD, false, "CRandomTask");
		if (packet.r_u16())			// #inited_tasks
			xr_not_implemented();
		// CRandomTask's id_generator
		set_save_marker(packet, SM_LOAD, false, "object_collection");
		r_object_collection(packet);
		set_save_marker(packet, SM_LOAD, true, "object_collection");
		if (packet.r_u8())			// #rewards
			xr_not_implemented();
		if (packet.r_u16())			// #inited_find_upgrade_tasks
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, true, "CRandomTask");

		// sim_board's id_generator
		set_save_marker(packet, SM_LOAD, false, "object_collection");
		r_object_collection(packet);
		set_save_marker(packet, SM_LOAD, true, "object_collection");

		// CMinigames
		set_save_marker(packet, SM_LOAD, false, "CMinigames");
		if (packet.r_u16() != 2)		// #minigames
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "test_crowkiller"))	// first minigame's name
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "CMGCrowKiller"))	// first minigame's profile
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "valiable"))		// first minigame's state
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, false, "CMGCrowKiller");
		if (packet.r_u8())			// params_list.highscore
			xr_not_implemented();
		if (packet.r_u16() != 60)		// params_list.timer
			xr_not_implemented();
		if (packet.r_bool())			// params_list.win
			xr_not_implemented();
		if (packet.r_u8() != 4)			// #params_list.crows_to_kill
			xr_not_implemented();
		if (packet.r_u32())			// params_list.crows_to_kill
			xr_not_implemented();
		if (packet.r_u16() != 10)		// params_list.money_multiplier
			xr_not_implemented();
		if (packet.r_u16() != 100)		// params_list.champion_multiplier
			xr_not_implemented();
		if (packet.r_u8())			// params_list.selected
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), ""))	// params_list.game_type
			xr_not_implemented();
		if (packet.r_u8())			// high_score
			xr_not_implemented();
		if (packet.r_u16())			// timer
			xr_not_implemented();
		if (packet.r_u16() != 10)		// time_out
			xr_not_implemented();
		if (packet.r_u8())			// killed_counter
			xr_not_implemented();
		if (packet.r_bool())			// win
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, true, "CMGCrowKiller");

		if (std::strcmp(packet.skip_sz(), "test_shooting"))	// second minigame's name
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "CMGShooting"))	// second minigame's profile
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "valiable"))		// second minigame's state
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, false, "CMGShooting");
		if (std::strcmp(packet.skip_sz(), ""))		// params_list.game_type
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), ""))		// params_list.wpn_type
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// params_list.stand_way
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// params_list.look_way
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// params_list.stand_way_back
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// params_list.look_way_back
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// params_list.obj_name
			xr_not_implemented();
		if (packet.r_u8() != 1)				// params_list.win type
			xr_not_implemented();
		if (packet.r_u16())				// params_list.win
			xr_not_implemented();
		if (packet.r_u8())				// params_list.distance
			xr_not_implemented();
		if (packet.r_u8())				// params_list.ammo
			xr_not_implemented();
		if (packet.r_u8())				// #params_list.targets
			xr_not_implemented();
		if (packet.r_u8())				// params_list.target_counter
			xr_not_implemented();
		if (packet.r_u8())				// #inventory_items
			xr_not_implemented();
		if (packet.r_u32())				// prev_time
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// cur_game
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, true, "CMGShooting");
		set_save_marker(packet, SM_LOAD, true, "CMinigames");

		set_save_marker(packet, SM_LOAD, true, "se_actor");
	} else if (m_version >= CSE_VERSION_0x27 && m_version <= CSE_VERSION_SOC) {
		// no script entity
	} else {
		xr_not_implemented();
	}
}

void se_actor::state_write(xr_packet& packet)
{
	cse_alife_creature_actor::state_write(packet);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);
		
		set_save_marker(packet, SM_SAVE, false, "se_actor");
		packet.w_bool(0);		//start_position_filled
		set_save_marker(packet, SM_SAVE, true, "se_actor");

	} else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);

		set_save_marker(packet, SM_SAVE, false, "se_actor");

		// CRandomTask
		set_save_marker(packet, SM_SAVE, false, "CRandomTask");
		packet.w_u16(0);
		// CRandomTask's id_generator
		set_save_marker(packet, SM_SAVE, false, "object_collection");
		w_object_collection(packet);
		set_save_marker(packet, SM_SAVE, true, "object_collection");
		packet.w_u8(0);
		packet.w_u16(0);
		set_save_marker(packet, SM_SAVE, true, "CRandomTask");

		// sim_board's id_generator
		set_save_marker(packet, SM_SAVE, false, "object_collection");
		w_object_collection(packet);
		set_save_marker(packet, SM_SAVE, true, "object_collection");

		// CMinigames
		set_save_marker(packet, SM_SAVE, false, "CMinigames");
		packet.w_u16(2);
		packet.w_sz("test_crowkiller");
		packet.w_sz("CMGCrowKiller");
		packet.w_sz("valiable");
		set_save_marker(packet, SM_SAVE, false, "CMGCrowKiller");
		packet.w_u8(0);
		packet.w_u16(60);
		packet.w_bool(false);
		packet.w_u8(4);
		packet.w_u32(0);
		packet.w_u16(10);
		packet.w_u16(100);
		packet.w_u8(0);
		packet.w_sz("");
		packet.w_u8(0);
		packet.w_u16(0);
		packet.w_u16(10);
		packet.w_u8(0);
		packet.w_bool(false);
		set_save_marker(packet, SM_SAVE, true, "CMGCrowKiller");

		packet.w_sz("test_shooting");
		packet.w_sz("CMGShooting");
		packet.w_sz("valiable");
		set_save_marker(packet, SM_SAVE, false, "CMGShooting");
		packet.w_sz("");
		packet.w_sz("");
		packet.w_sz("nil");
		packet.w_sz("nil");
		packet.w_sz("nil");
		packet.w_sz("nil");
		packet.w_sz("nil");
		packet.w_u8(1);
		packet.w_u16(0);
		packet.w_u8(0);
		packet.w_u8(0);
		packet.w_u8(0);
		packet.w_u8(0);
		packet.w_u8(0);
		packet.w_u32(0);
		packet.w_sz("nil");
		set_save_marker(packet, SM_SAVE, true, "CMGShooting");
		set_save_marker(packet, SM_SAVE, true, "CMinigames");

		set_save_marker(packet, SM_SAVE, true, "se_actor");
	} else if (m_version >= CSE_VERSION_2215 && m_version <= CSE_VERSION_SOC) {
		// stub
	} else {
		xr_not_implemented();
	}
}

////////////////////////////////////////////////////////////////////////////////

se_monster::se_monster(): m_job_online(0), m_was_in_smart_terrain(false),
	m_sim_forced_online(false) {}

void se_monster::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_monster_base::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);

		packet.r_sz(m_off_level_vertex_id);
		packet.r_sz(m_active_section);
	}
	else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);
		packet.r_u8(m_job_online);
		if (m_job_online > 2)
			packet.skip_sz();
		if (m_script_version >= 7)
			packet.r_sz(m_squad_id);
		if (m_script_version >= 8)
			packet.r_bool(m_sim_forced_online);
	} else if (m_version == CSE_VERSION_SOC) {
		if (m_script_version >= 3) {
			packet.r_u8(m_job_online);
			if (m_job_online > 2)
				packet.skip_sz();
		}
		if (m_script_version >= 5)
			packet.r_bool(m_was_in_smart_terrain);
	} else if (m_version == CSE_VERSION_2571) {
		if (m_script_version >= 4)
			packet.r_u8(m_job_online);
	} else if (m_version == CSE_VERSION_2232) {
		if (m_script_version >= 3)
			packet.r_u8(m_job_online);
	} else if (m_version == CSE_VERSION_2215) {
		if (m_script_version >= 2)
			packet.r_bool(m_was_in_smart_terrain);
	} else if (m_version >= CSE_VERSION_0x27 && m_version <= CSE_VERSION_0x2e) {
		// no script entity
	} else {
		xr_not_implemented();
	}
}

void se_monster::state_write(xr_packet& packet)
{
	cse_alife_monster_base::state_write(packet);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);

		packet.w_sz(m_off_level_vertex_id);
		packet.w_sz(m_active_section);
	}
	else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);
		packet.w_u8(m_job_online);
		if (m_job_online > 2)
			xr_not_expected();
		if (m_script_version >= 7)
			packet.w_sz(m_squad_id);
		if (m_script_version >= 8)
			packet.w_bool(m_sim_forced_online);
	} else if (m_version == CSE_VERSION_SOC) {
		if (m_script_version >= 3)
			packet.w_u8(m_job_online);
		if (m_script_version >= 5)
			packet.w_bool(m_was_in_smart_terrain);
	} else if (m_version == CSE_VERSION_2215) {
		if (m_script_version >= 2)
			packet.w_bool(m_was_in_smart_terrain);
	} else {
		xr_not_implemented();
	}
}

////////////////////////////////////////////////////////////////////////////////

se_stalker::se_stalker(): m_job_online(0), m_was_in_smart_terrain(false), m_death_dropped(false),
	m_sim_forced_online(false) {}

void se_stalker::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_human_stalker::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);
		packet.r_sz(m_old_lvid);
		packet.r_sz(m_active_section);
		packet.r_bool(m_death_dropped);
	}
	else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);
		packet.r_u8(m_job_online);
		if (m_job_online > 2)
			packet.skip_sz();
		packet.r_bool(m_death_dropped);
		if (m_script_version >= 7)
			packet.r_sz(m_squad_id);
		if (m_script_version >= 8)
			packet.r_bool(m_sim_forced_online);
	} else if (m_version == CSE_VERSION_SOC) {
		if (m_script_version >= 3) {
			packet.r_u8(m_job_online);
			if (m_job_online > 2)
				packet.skip_sz();
		}
		if (m_script_version >= 5)
			packet.r_bool(m_was_in_smart_terrain);
		if (m_script_version >= 6)
			packet.r_bool(m_death_dropped);
	} else if (m_version == CSE_VERSION_2232) {
		if (m_script_version >= 3)
			packet.r_u8(m_job_online);
	} else if (m_version == CSE_VERSION_2571) {
		if (m_script_version >= 3) {
			packet.r_u8(m_job_online);
		}	
	} else if (m_version == CSE_VERSION_2215) {
		if (m_script_version >= 2)
			packet.r_bool(m_was_in_smart_terrain);
	} else {
		xr_not_implemented();
	}
}

void se_stalker::state_write(xr_packet& packet)
{
	cse_alife_human_stalker::state_write(packet);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);
		
		packet.w_sz(m_old_lvid);
		packet.w_sz(m_active_section);
		packet.w_bool(m_death_dropped);
	} else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);
		packet.w_u8(m_job_online);
		if (m_job_online > 2)
			xr_not_expected();
		packet.w_bool(m_death_dropped);
		if (m_script_version >= 7)
			packet.w_sz(m_squad_id);
		if (m_script_version >= 8)
			packet.w_bool(m_sim_forced_online);
	} else if (m_version == CSE_VERSION_SOC) {
		if (m_script_version >= 3)
			packet.w_u8(m_job_online);
		if (m_script_version >= 5)
			packet.w_bool(m_was_in_smart_terrain);
		if (m_script_version >= 6)
			packet.w_bool(m_death_dropped);
	} else if (m_version == CSE_VERSION_2215) {
		if (m_script_version >= 2)
			packet.w_bool(m_was_in_smart_terrain);
	} else {
		xr_not_implemented();
	}
}

////////////////////////////////////////////////////////////////////////////////

void se_respawn::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_smart_zone::state_read(packet, size);
    if (m_version >= CSE_VERSION_2232) {
		packet.r_seq(packet.r_u8(), m_spawned_obj);
	} else if (m_version == CSE_VERSION_2215) {
		// there is no se_respawn in 2215 so we should not ever get here.
	} else {
		xr_not_implemented();
	}
}

void se_respawn::state_write(xr_packet& packet)
{
	cse_alife_smart_zone::state_write(packet);
	if (m_version >= CSE_VERSION_SOC) {
		packet.w_size_u8(m_spawned_obj.size());
		packet.w_seq(m_spawned_obj);
	} else if (m_version == CSE_VERSION_2215) {
		// se_respawn will be represented as smart_terrain, but we are
		// safe because xrAI won't read data beyond cse_alife_smart_zone.
	} else {
		xr_not_implemented();
	}
}

////////////////////////////////////////////////////////////////////////////////

void se_smart_terrain::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_smart_zone::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);

		set_save_marker(packet, SM_LOAD, false, "se_smart_terrain");

    	packet.r_u8(arriving_npc_count);
    	packet.r_u8(npc_info_count);
    	packet.r_u8(dead_time_count);
    	packet.r_u8(base_on_actor_control_present);
		if (base_on_actor_control_present == 1)
			return;
    	packet.r_u8(is_respawn_point);
		if (is_respawn_point == 1)
			return;
    	packet.r_u8(population);
	  
	  	set_save_marker(packet, SM_LOAD, true, "se_smart_terrain");
	}
	else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);

		set_save_marker(packet, SM_LOAD, false, "se_smart_terrain");

		// CCombat_manager
		set_save_marker(packet, SM_LOAD, false, "CCombat_manager");
		if (packet.r_bool())			// actor_defense_come
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// combat_quest
			xr_not_implemented();
		if (packet.r_u16() != 0xffff)		// task
			xr_not_implemented();
		if (std::strcmp(packet.skip_sz(), "nil"))	// see_actor_enemy
			xr_not_implemented();
		if (packet.r_bool())			// see_actor_enemy_time presence
			xr_not_implemented();
		if (packet.r_u8())			// #squads
			xr_not_implemented();
		if (packet.r_bool())			// force_online
			xr_not_implemented();
		if (packet.r_u8())			// #force_online_squads
			xr_not_implemented();

		// CCover_manager
		set_save_marker(packet, SM_LOAD, false, "CCover_manager");
		if (packet.r_bool())			// is_valid
			xr_not_implemented();
		if (packet.r_u8())			// #cover_table
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, true, "CCover_manager");
		set_save_marker(packet, SM_LOAD, true, "CCombat_manager");

		// actual se_smart_terrain
		if (packet.r_u8())		// #npc_info
			xr_not_implemented();
		if (packet.r_u8())		// #dead_time
			xr_not_implemented();

		set_save_marker(packet, SM_LOAD, true, "se_smart_terrain");
	} else if (m_version == CSE_VERSION_SOC) {
		r_ctime(packet);		// duration_end
		r_ctime(packet);		// idle_end
		if (packet.r_bool())		// gulag_working
			xr_not_implemented();
	} else if (m_version == CSE_VERSION_2232) {
		//script save nothing
	} else if (m_version == CSE_VERSION_2571) {
		//script save nothing
	} else if (m_version == CSE_VERSION_2215) {
		packet.r_u8();			// gulagN
		r_ctime(packet);		// duration_end
		r_ctime(packet);		// idle_end
		if (packet.r_u8())		// #npcs
			xr_not_implemented();
	} else {
		xr_not_implemented();
	}
}

void se_smart_terrain::state_write(xr_packet& packet)
{
	cse_alife_smart_zone::state_write(packet);
	if (m_version >= CSE_VERSION_0x80) {
		xr_assert(m_version <= CSE_VERSION_COP);

		set_save_marker(packet, SM_SAVE, false, "se_smart_terrain");
	
	    packet.w_u8(arriving_npc_count);
	    packet.w_u8(npc_info_count);
	    packet.w_u8(dead_time_count);
	    packet.w_u8(base_on_actor_control_present);
	    packet.w_u8(is_respawn_point);
	    packet.w_u8(population);
	  
	  set_save_marker(packet, SM_SAVE, true, "se_smart_terrain");
	}
	else if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);

		set_save_marker(packet, SM_SAVE, false, "se_smart_terrain");

		// CCombat_manager
		set_save_marker(packet, SM_SAVE, false, "CCombat_manager");
		packet.w_bool(false);
		packet.w_sz("nil");
		packet.w_u16(0xffff);
		packet.w_sz("nil");
		packet.w_bool(false);
		packet.w_u8(0);
		packet.w_bool(false);
		packet.w_u8(0);

		// CCover_manager
		set_save_marker(packet, SM_SAVE, false, "CCover_manager");
		packet.w_bool(false);
		packet.w_u8(0);
		set_save_marker(packet, SM_SAVE, true, "CCover_manager");
		set_save_marker(packet, SM_SAVE, true, "CCombat_manager");

		// actual se_smart_terrain
		packet.w_u8(0);
		packet.w_u8(0);

		set_save_marker(packet, SM_SAVE, true, "se_smart_terrain");
	} else if (m_version == CSE_VERSION_SOC) {
		w_ctime(packet);
		w_ctime(packet);
		packet.w_bool(false);
	} else if (m_version == CSE_VERSION_2215) {
		packet.w_u8(0);
		w_ctime(packet);
		w_ctime(packet);
		packet.w_u8(0);
	} else {
		xr_not_implemented();
	}
}

////////////////////////////////////////////////////////////////////////////////

void se_sim_faction::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_smart_zone::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);
		set_save_marker(packet, SM_LOAD, false, "se_sim_faction");
		if (packet.r_bool())		// community_player
			xr_not_implemented();
		if (packet.r_bool())		// start_position_filled
			xr_not_implemented();
		if (packet.r_u8())		// current_expansion_level
			xr_not_implemented();
		r_ctime(packet);		// last_spawn_time
		if (packet.r_u8())		// #squad_target_cache
			xr_not_implemented();
		if (packet.r_u8())		// #random_tasks
			xr_not_implemented();
		if (packet.r_u8())		// #current_attack_quantity
			xr_not_implemented();
		if (packet.r_u16())		// #squads
			xr_not_implemented();
		set_save_marker(packet, SM_LOAD, true, "se_sim_faction");
	} else if (m_version == CSE_VERSION_SOC || m_version == CSE_VERSION_2215) {
		// stub
	} else {
		xr_not_implemented();
	}
}

void se_sim_faction::state_write(xr_packet& packet)
{
	cse_alife_smart_zone::state_write(packet);
	if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_CS);
		set_save_marker(packet, SM_SAVE, false, "se_sim_faction");
		packet.w_bool(false);
		packet.w_bool(false);
		packet.w_u8(0);
		w_ctime(packet, true);
		packet.w_u8(0);
		packet.w_u8(0);
		packet.w_u8(0);
		packet.w_u16(0);
		set_save_marker(packet, SM_SAVE, true, "se_sim_faction");
	} else if (m_version == CSE_VERSION_SOC) {
		// stub
	} else if (m_version == CSE_VERSION_2215) {
		// stub for aiwrapper, see se_respawn and aiwrapper.ini
	} else {
		xr_not_implemented();
	}
}

////////////////////////////////////////////////////////////////////////////////

void se_zone_anom::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_anomalous_zone::state_read(packet, size);
	if (m_version > CSE_VERSION_0x27) {
		if (packet.r_u8() == 1)		// last_spawn_time
			xr_not_implemented();
	}
}

void se_zone_anom::state_write(xr_packet& packet)
{
	cse_alife_anomalous_zone::state_write(packet);
	packet.w_u8(0);
}

////////////////////////////////////////////////////////////////////////////////

void se_zone_visual::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_zone_visual::state_read(packet, size);
	if (packet.r_u8() == 1)		// last_spawn_time
		xr_not_implemented();
}

void se_zone_visual::state_write(xr_packet& packet)
{
	cse_alife_zone_visual::state_write(packet);
	packet.w_u8(0);
}

////////////////////////////////////////////////////////////////////////////////

se_level_changer::se_level_changer():
	m_enabled(true), m_hint("level_changer_invitation") {}

void se_level_changer::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_level_changer::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x7c_HACK) {
		set_save_marker(packet, SM_LOAD, false, "se_level_changer");
		packet.r_bool(m_enabled);
		packet.r_sz(m_hint);
		set_save_marker(packet, SM_LOAD, true, "se_level_changer");
	}
}

void se_level_changer::state_write(xr_packet& packet)
{
	cse_alife_level_changer::state_write(packet);
	if (m_version >= CSE_VERSION_0x7c_HACK) {
		set_save_marker(packet, SM_SAVE, false, "se_level_changer");
		packet.w_bool(m_enabled);
		packet.w_sz(m_hint);
		set_save_marker(packet, SM_SAVE, true, "se_level_changer");
	}
}



////////////////////////////////////////////////////////////////////////////////

inline se_smart_cover::loophole::loophole(const char* _id, bool _enabled):
	id(_id), enabled(_enabled) {}

void se_smart_cover::state_read(xr_packet& packet, uint16_t size)
{
	cse_smart_cover::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x80) {
		packet.r_sz(m_last_description);
		packet.r_u8(m_loopholes_count);

		if (m_loopholes_count > 0)
		{
			m_loopholes = new std::vector<loophole>();

			m_loopholes->reserve(m_loopholes_count);

			for (int i = 0; i < m_loopholes_count; ++i)
			{
				std::string *id = new std::string;
				packet.r_sz(*id);
				m_loopholes->push_back(loophole(id->c_str(), packet.r_bool()));
			}
		}
	}
}

void se_smart_cover::state_write(xr_packet& packet)
{
	cse_smart_cover::state_write(packet);
	if (m_version >= CSE_VERSION_0x80) {
		packet.w_sz(m_last_description);
		packet.w_u8(m_loopholes_count);

		if (m_loopholes_count > 0)
		{
			for (std::vector<loophole>::iterator it = m_loopholes->begin(), end = m_loopholes->end();
				it != end; ++it){
				packet.w_sz(it->id);
				packet.w_bool(it->enabled);
			}
		}
	}
}

////////////////////////////////////////////////////////////////////////////////

se_invbox::se_invbox(): m_tip("inventory_box_use") {}

void se_invbox::state_read(xr_packet& packet, uint16_t size)
{
	cse_inventory_box::state_read(packet, size);

	if (m_version >= CSE_VERSION_0x80)
	{
		uint16_t cse_alive_inventory_box__unk1_u8 = packet.r_u8();
		uint16_t cse_alive_inventory_box__unk2_u8 = packet.r_u8();
		packet.r_sz(m_tip);
	}
}

void se_invbox::state_write(xr_packet& packet)
{
	cse_inventory_box::state_write(packet);

	if (m_version >= CSE_VERSION_0x80)
	{
		packet.w_u8(1);
		packet.w_u8(0);

		packet.w_sz(m_tip);
	}
}

////////////////////////////////////////////////////////////////////////////////

se_zone_torrid::se_zone_torrid() : m_last_spawn_time_present(0) {}

void se_zone_torrid::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_torrid_zone::state_read(packet, size);

	if (m_version >= CSE_VERSION_0x80)
		m_last_spawn_time_present = packet.r_u8();
}

void se_zone_torrid::state_write(xr_packet& packet)
{
	cse_alife_torrid_zone::state_write(packet);

	if (m_version >= CSE_VERSION_0x80)
		packet.w_u8(m_last_spawn_time_present);
}

#include "StdAfx.h"
#include "game_state_accumulator.h"

//typelist:
#include "command_switch_counter.h"
#include "player_state_params.h"
#include "player_team_win_score.h"
#include "player_spot_params.h"
#include "player_state_blitzkrieg.h"
#include "player_state_multichampion.h"
#include "player_state_mad.h"
#include "player_state_achilles_heel.h"
#include "faster_than_bullets_time.h"
#include "harvest_time.h"
#include "player_state_skewer.h"
#include "double_shot_double_kill.h"
#include "player_state_climber.h"
#include "player_state_ammo_elapsed.h"
#include "player_state_opener.h"
#include "player_state_toughy.h"
#include "invincible_fury.h"
#include "sprinter_stopper.h"
#include "player_state_marksman.h"
#include "player_state_ambassador.h"
#include "player_state_remembrance.h"
#include "player_state_avenger.h"
#include "player_state_cherub.h"
#include "stalker_flair.h"
#include "black_list.h"
#include "silent_shots.h"
#include "killer_victim_velocity_angle.h"

namespace award_system
{
	struct accumulative_entry_t
	{
		using accumulative_create_fn = player_state_param * (*)(game_state_accumulator*);
		enum_accumulative_player_values id;
		accumulative_create_fn create;
	};
	
	static const accumulative_entry_t g_accumulative_entries[] = 
	{
		{ acpv_death_count,					[](game_state_accumulator* a)->player_state_param* { return new player_death_counter(a); } },
		{ acpv_artdeliver_count,			[](game_state_accumulator* a)->player_state_param* { return new player_artdeliver_counter(a); } },
		{ acpv_kill_in_raw,					[](game_state_accumulator* a)->player_state_param* { return new player_rawkill_counter(a); } },
		{ acpv_move_state,					[](game_state_accumulator* a)->player_state_param* { return new player_state_move(a); } },
		{ acpv_move_velocity,				[](game_state_accumulator* a)->player_state_param* { return new player_state_velocity(a); } },
		{ acpv_move_ang_velocity,			[](game_state_accumulator* a)->player_state_param* { return new player_state_ang_velocity(a); } },
		{ acpv_black_list,					[](game_state_accumulator* a)->player_state_param* { return new black_list(a); } },
		{ acpv_command_switch_count,		[](game_state_accumulator* a)->player_state_param* { return new command_switch_counter(a); } },
		{ acpv_double_shot_double_kill_time,[](game_state_accumulator* a)->player_state_param* { return new double_shot_double_kill(a); } },
		{ acpv_faster_than_bullets_time,	[](game_state_accumulator* a)->player_state_param* { return new faster_than_bullets_time(a); } },
		{ acpv_harvest_count,				[](game_state_accumulator* a)->player_state_param* { return new harvest_time(a); } },
		{ acpv_invincible_fury,				[](game_state_accumulator* a)->player_state_param* { return new player_state_invincible_fury(a); } },
		{ acpv_killer_victim_angle,			[](game_state_accumulator* a)->player_state_param* { return new killer_victim_angle(a); } },
		{ acpv_spots,						[](game_state_accumulator* a)->player_state_param* { return new player_spots_counter(a); } },
		{ acpv_enemy_top_player_div,		[](game_state_accumulator* a)->player_state_param* { return new player_spots_with_top_enemy_divider(a); } },
		{ acpv_achilles_heel_ready,			[](game_state_accumulator* a)->player_state_param* { return new achilles_heel_kill(a); } },
		{ acpv_ambassador,					[](game_state_accumulator* a)->player_state_param* { return new player_state_ambassador(a); } },
		{ acpv_ammo_elapsed,				[](game_state_accumulator* a)->player_state_param* { return new player_state_ammo_elapsed(a); } },
		{ acpv_avenger,						[](game_state_accumulator* a)->player_state_param* { return new player_state_avenger(a); } },
		{ acpv_blitzkrieg_time,				[](game_state_accumulator* a)->player_state_param* { return new player_blitzkrieg(a); } },
		{ acpv_cherub_ready,				[](game_state_accumulator* a)->player_state_param* { return new player_state_cherub(a); } },
		{ acpv_climber,						[](game_state_accumulator* a)->player_state_param* { return new player_state_climber(a); } },
		{ acpv_mad,							[](game_state_accumulator* a)->player_state_param* { return new player_state_mad(a); } },
		{ acpv_marksman_count,				[](game_state_accumulator* a)->player_state_param* { return new player_state_marksman(a); } },
		{ acpv_multi_champion,				[](game_state_accumulator* a)->player_state_param* { return new player_multichampion(a); } },
		{ acpv_opener_ready,				[](game_state_accumulator* a)->player_state_param* { return new player_state_opener(a); } },
		{ acpv_remembrance,					[](game_state_accumulator* a)->player_state_param* { return new player_state_remembrance(a); } },
		{ acpv_skewer_count,				[](game_state_accumulator* a)->player_state_param* { return new player_state_skewer(a); } },
		{ acpv_toughy,						[](game_state_accumulator* a)->player_state_param* { return new player_state_toughy(a); } },
		{ acpv_my_team_win_score,			[](game_state_accumulator* a)->player_state_param* { return new player_team_win_score(a); } },
		{ acpv_enemy_team_score,			[](game_state_accumulator* a)->player_state_param* { return new player_enemy_team_score(a); } },
		{ acpv_my_team_win_score_now,		[](game_state_accumulator* a)->player_state_param* { return new player_runtime_win_score(a); } },
		{ acpv_enemy_team_score_now,		[](game_state_accumulator* a)->player_state_param* { return new player_runtime_enemy_team_score(a); } },
		{ acpv_thunder_count,				[](game_state_accumulator* a)->player_state_param* { return new silent_shots(a); } },
		{ acpv_sprinter_victim_velocity,	[](game_state_accumulator* a)->player_state_param* { return new spritnter_stopper(a); } },
		{ acpv_stalker_flair,				[](game_state_accumulator* a)->player_state_param* { return new stalker_flair(a); } }
	};
	
	void game_state_accumulator::init_accumulative_values()
	{
		for (const auto& entry : g_accumulative_entries)
		{
			player_state_param* obj = entry.create(this);
			m_accumulative_values.insert(std::make_pair(entry.id, obj));
		}
	
		// FX: контроль на соответствие enum и данных
		VERIFY(m_accumulative_values.size() == acpv_count);
	}
}
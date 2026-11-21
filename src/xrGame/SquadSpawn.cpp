#include "StdAfx.h"
#include "pch_script.h"
#include "SquadSpawn.h"

#include "Level.h"
#include "Actor.h"
#include "HUDManager.h"
#include "xrServer.h"
#include "alife_simulator.h"
#include "alife_smart_terrain_registry.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "ai_space.h"
#include "level_graph.h"
#include "game_level_cross_table.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_engine_space.h"
#include "../xrScripts/script_process.h"
#include "../xrEngine/CustomHUD.h"
#include "ai_object_location.h"

namespace SquadSpawn
{
	namespace Detail
	{
		bool IsSocCreatureSection(CInifile::Sect& Section)
		{
			if (!Section.line_exist("class") || !Section.line_exist("species") || Section.line_exist("story_id"))
			{
				return false;
			}

			const char* Species = nullptr;
			Section.line_exist("species", &Species);
			return Species != nullptr && xr_strcmp(Species, "actor") != 0;
		}

		bool IsSquadDescription(CInifile::Sect& Section)
		{
			return Section.line_exist("faction") && (Section.line_exist("npc") || Section.line_exist("npc_in_squad"));
		}

		CScriptProcess* GetLevelScriptProcess()
		{
			if (!g_pGameLevel || !ai().get_alife())
			{
				Msg("! ALife simulator is needed to spawn squads");
				return nullptr;
			}

			CScriptProcess* Process = ai().script_engine().script_process(ScriptEngine::eScriptProcessorLevel);
			if (Process == nullptr)
			{
				Msg("! Level script process is not running");
			}
			return Process;
		}

		Fvector GetCameraTarget()
		{
			Fvector Point;
			Point.mad(Device.vCameraPosition, Device.vCameraDirection, HUD().GetCurrentRayQuery().range);
			return Point;
		}

		const char* GetNearestOnlineSmartName()
		{
			const char* Result = nullptr;
			float BestDistance = flt_max;
			for (const auto& [Id, Smart] : ai().alife().smart_terrains().objects())
			{
				if (!Smart->m_bOnline)
				{
					continue;
				}

				const float Distance = Smart->Position().distance_to_sqr(Device.vCameraPosition);
				if (Distance < BestDistance)
				{
					BestDistance = Distance;
					Result = Smart->name_replace();
				}
			}
			return Result;
		}

		void SpawnCallOfPripyat(const char* Section, int Count)
		{
			CScriptProcess* Process = GetLevelScriptProcess();
			if (Process == nullptr)
			{
				return;
			}

			const char* SmartName = GetNearestOnlineSmartName();
			if (SmartName == nullptr)
			{
				Msg("! g_spawn_squad: no online smart terrain near the camera");
				return;
			}

			string512 Command;
			for (int i = 0; i < Count; ++i)
			{
				xr_sprintf(Command, "xr_effects.create_squad(db.actor,nil,{\"%s\",\"%s\"})", Section, SmartName);
				Process->add_script(Command, true, true);
			}
			Msg("* g_spawn_squad: [%s] x%d in smart [%s]", Section, Count, SmartName);
		}

		// Clear Sky squads must enter a smart that is free or already held by their faction,
		// otherwise sim_board:enter_smart aborts. The squad spawns at that smart's spawn point.
		void SpawnClearSky(const char* Section, int Count)
		{
			CScriptProcess* Process = GetLevelScriptProcess();
			if (Process == nullptr)
			{
				return;
			}

			const char* Faction = GetDescriptions()->r_string(Section, "faction");

			static constexpr const char* Script =
				"local board = sim_board.get_sim_board() "
				"local sect, faction, count = \"%s\", \"%s\", %d "
				"if board.players[faction] == nil then log1(\"! g_spawn_squad: faction [\"..faction..\"] is not registered on sim_board\") return end "
				"local gg = game_graph() "
				"local level_id = gg:vertex(db.actor:game_vertex_id()):level_id() "
				"local cam = device().cam_pos "
				"local best, best_dist = nil, 0 "
				"for id, v in pairs(board.smarts) do "
				"local s = v.smrt "
				"local free = s.player_name == \"none\" and v.stayed_squad_quan == 0 "
				"local own = s.player_name == faction and v.stayed_squad_quan > 0 "
				"if (free or own) and gg:vertex(s.m_game_vertex_id):level_id() == level_id then "
				"local d = s.position:distance_to_sqr(cam) "
				"if best == nil or d < best_dist then best, best_dist = s, d end "
				"end "
				"end "
				"if best == nil then log1(\"! g_spawn_squad: no free smart or smart of faction [\"..faction..\"] on this level\") return end "
				"for i = 1, count do "
				"local squad = board:create_squad(faction, sim_squad_scripted.sim_squad_scripted, best, nil, sect) "
				"board:enter_smart(squad, best.id) "
				"squad:update() "
				"end "
				"log1(\"* g_spawn_squad: [\"..sect..\"] x\"..count..\" in smart [\"..best:name()..\"]\")";

			string2048 Command;
			xr_sprintf(Command, Script, Section, Faction, Count);
			Process->add_script(Command, true, true);
		}

		void SpawnShadowOfChernobyl(const char* Section, int Count)
		{
			if (!g_pGameLevel || !ai().get_alife() || !ai().get_level_graph())
			{
				Msg("! ALife simulator is needed to spawn squads");
				return;
			}

			CActor* Actor = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_actor() : nullptr;
			if (Actor == nullptr)
			{
				return;
			}

			const ILevelGraph& LevelGraph = ai().level_graph();
			const u32 ActorVertex = Actor->ai_location().level_vertex_id();
			const u32 CenterVertex = LevelGraph.vertex(ActorVertex, GetCameraTarget());
			if (!LevelGraph.valid_vertex_id(CenterVertex))
			{
				Msg("! g_spawn_squad: no AI node near the camera target");
				return;
			}

			const Fvector Center = LevelGraph.vertex_position(CenterVertex);
			CALifeSimulator& Simulator = Level().Server->game->alife();
			for (int i = 0; i < Count; ++i)
			{
				Fvector Position = Center;
				Position.x += ::Random.randF(-2.0f, 2.0f);
				Position.z += ::Random.randF(-2.0f, 2.0f);

				u32 Vertex = LevelGraph.vertex(CenterVertex, Position);
				if (!LevelGraph.valid_vertex_id(Vertex))
				{
					Vertex = CenterVertex;
				}
				Position = LevelGraph.vertex_position(Vertex);

				const GameGraph::_GRAPH_ID GameVertex = ai().cross_table().vertex(Vertex).game_vertex_id();
				Simulator.spawn_item(Section, Position, Vertex, GameVertex, u16(-1));
			}
			Msg("* g_spawn_squad: [%s] x%d near the camera target", Section, Count);
		}
	}

	CInifile* GetDescriptions()
	{
		if (EngineExternal().ShadowOfChernobylMode())
		{
			return nullptr;
		}

		if (!EngineExternal().ClearSkyMode())
		{
			return pSettings;
		}

		static xr_unique_ptr<CInifile> Descriptions;
		if (!Descriptions)
		{
			string_path Path;
			FS.update_path(Path, "$game_config$", "misc\\squad_descr.ltx");
			Descriptions = xr_make_unique<CInifile>(Path);
		}
		return Descriptions.get();
	}

	void CollectSections(xr_vector<CInifile::Sect*>& Sections)
	{
		CInifile* Descriptions = GetDescriptions();
		if (Descriptions == nullptr)
		{
			for (CInifile::Sect& Section : pSettings->sections())
			{
				if (Detail::IsSocCreatureSection(Section))
				{
					Sections.push_back(&Section);
				}
			}
			return;
		}

		for (CInifile::Sect& Section : Descriptions->sections())
		{
			if (Detail::IsSquadDescription(Section))
			{
				Sections.push_back(&Section);
			}
		}
	}

	bool IsSquadSection(const char* Section)
	{
		CInifile* Descriptions = GetDescriptions();
		if (Descriptions == nullptr)
		{
			return pSettings->section_exist(Section) && Detail::IsSocCreatureSection(pSettings->r_section(Section));
		}
		return Descriptions->section_exist(Section) && Detail::IsSquadDescription(Descriptions->r_section(Section));
	}

	void Spawn(const char* Section, int Count)
	{
		if (!IsSquadSection(Section))
		{
			if (EngineExternal().ShadowOfChernobylMode())
			{
				Msg("! g_spawn_squad: [%s] is not a stalker or monster section", Section);
			}
			else
			{
				Msg("! g_spawn_squad: squad [%s] not found", Section);
			}
			return;
		}

		if (EngineExternal().ShadowOfChernobylMode())
		{
			Detail::SpawnShadowOfChernobyl(Section, Count > 0 ? Count : SocDefaultGroupSize);
		}
		else if (EngineExternal().ClearSkyMode())
		{
			Detail::SpawnClearSky(Section, Count > 0 ? Count : 1);
		}
		else
		{
			Detail::SpawnCallOfPripyat(Section, Count > 0 ? Count : 1);
		}
	}
}

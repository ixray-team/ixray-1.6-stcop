#include "StdAfx.h"
#include "LevelGameDef.h"
#include "ai_space.h"
#include "../xrScripts/script_process.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_engine_space.h"
#include "Level.h"
#include "game_cl_base.h"
#include "../xrEngine/x_ray.h"
#include "../xrPhysics/PhysicsCommon.h"
#include "level_sounds.h"
#include "GamePersistent.h"
#include "../xrEngine/Rain.h"

#include "inventory_upgrade_manager.h"
#include "ElectronicsProblemsManager.h"
#include "ParticlesObject.h"

#include <filesystem>

bool CLevel::Load_GameSpecific_Before()
{
	// AI space
	g_pGamePersistent->SetLoadStageTitle("st_loading_ai_objects");
	g_pGamePersistent->LoadTitle();
	string_path fn_game;

	if (HasLevelResource(ELevelResource::EnvModifiers))
		LoadAuxiliaryResource(ELevelResource::EnvModifiers);
	else
		SetResourceState(ELevelResource::EnvModifiers, EResourceState::NotLoaded);

	const bool wantGameGraph  = HasLevelResource(ELevelResource::GameGraph);
	const bool wantPatrol     = HasLevelResource(ELevelResource::PatrolPaths);
	const bool wantAiMap      = HasLevelResource(ELevelResource::AiMap);
	const bool wantSpawn      = HasLevelResource(ELevelResource::Spawn);

	if (wantSpawn && GamePersistent().GameType() != eGameIDSingle && OnClient() && FS.exist(fn_game, "$level$", "alife.spawn"))
	{
		spawn = FS.r_open(fn_game);
		
		IReader* chunk;

		if (wantPatrol)
		{
			chunk = spawn->open_chunk(3);
			if (chunk)
			{
				ai().patrol_path_storage(*chunk);
				chunk->close();
				SetResourceState(ELevelResource::PatrolPaths, EResourceState::Ready);
			}
			else
			{
				Msg("! Load_GameSpecific_Before: patrol paths chunk is missing in alife.spawn");
				SetResourceState(ELevelResource::PatrolPaths, EResourceState::Missing);
			}
		}

		if (wantGameGraph)
		{
			m_chunk = spawn->open_chunk(4);
			if (m_chunk)
			{
				ai().game_graph(new CGameGraph(*m_chunk));
				SetResourceState(ELevelResource::GameGraph, EResourceState::Ready);
			}
			else
			{
				Msg("! Load_GameSpecific_Before: game graph chunk is missing in alife.spawn");
				SetResourceState(ELevelResource::GameGraph, EResourceState::Missing);
			}
		}
	}
	else if (GamePersistent().GameType() != eGameIDSingle && OnClient())
	{
		if (wantSpawn)
			SetResourceState(ELevelResource::Spawn, EResourceState::Missing);
	}

	if (wantGameGraph && LevelDesc.RemoteTransport && RemoteServerEndpoint() != nullptr && ai().get_game_graph() == nullptr)
	{
		const bool loaded = LoadGameGraphResource();
		SetResourceState(ELevelResource::GameGraph, loaded ? EResourceState::Ready : EResourceState::Missing);
		SetResourceState(ELevelResource::CrossTable, HasLevelResource(ELevelResource::CrossTable) && ai().get_cross_table() != nullptr ? EResourceState::Ready : EResourceState::NotLoaded);
	}

	if (!ai().get_alife() || LevelDesc.RemoteTransport)
	{
		// Parallel (viewer) sessions may not have a session name yet; the level
		// name is equivalent for the AI-map load (and unused without a game graph).
		const bool canLoadAiMap = HasSessionName() || GetLevelDesc().Parallel;
		if (wantAiMap && FS.exist(fn_game, "$level$", "level.ai") && canLoadAiMap)
		{
			if (ai().get_level_graph() == nullptr || !ai().level_graph().has_nodes())
			{
				if (HasSessionName())
					ai().load(net_SessionName());
				else
					ai().load(name().c_str());
			}
			SetResourceState(ELevelResource::AiMap, ai().get_level_graph() && ai().level_graph().has_nodes() ? EResourceState::Ready : EResourceState::Missing);
		}
		else
		{
			SetResourceState(ELevelResource::AiMap, wantAiMap ? EResourceState::Missing : EResourceState::NotLoaded);
		}

		// level.game patrol paths need the level graph; a game graph is optional
		// for the raw loader (CPatrolPoint::load_raw tolerates nulls).
		const bool canLoadPatrol = ai().get_game_graph() != nullptr || GetLevelDesc().Parallel;
		if (wantPatrol && !g_dedicated_server && canLoadPatrol && FS.exist(fn_game, "$level$", "level.game"))
		{
			IReader* stream = FS.r_open(fn_game);
			ai().patrol_path_storage_raw(*stream);
			FS.r_close(stream);
			SetResourceState(ELevelResource::PatrolPaths, EResourceState::Ready);
		}
		else if (wantPatrol && GetResourceState(ELevelResource::PatrolPaths) == EResourceState::NotLoaded)
		{
			SetResourceState(ELevelResource::PatrolPaths, EResourceState::Missing);
		}
	}

	if ((IsGameTypeSingle() || IsServer()) && m_upgrade_manager == nullptr)
		m_upgrade_manager = new inventory::upgrade::Manager();
	if ((IsGameTypeSingle() || IsServer()) && m_electronics_problems_manager == nullptr)
		m_electronics_problems_manager = new CElectronicsProblemsManager();

	if (!IsGameTypeSingle() && OnClient())
	{
		R_ASSERT(m_upgrade_manager == nullptr);
		m_upgrade_manager = new inventory::upgrade::Manager();

		R_ASSERT(m_electronics_problems_manager == nullptr);
		m_electronics_problems_manager = new CElectronicsProblemsManager();
	}

	return (true);
}

bool CLevel::Load_GameSpecific_After()
{
	R_ASSERT(m_StaticParticles.empty());
	string_path fn_game;
	if (HasLevelResource(ELevelResource::StaticParticles))
		LoadAuxiliaryResource(ELevelResource::StaticParticles);
	else
		SetResourceState(ELevelResource::StaticParticles, EResourceState::NotLoaded);

	if	(!g_dedicated_server)
	{
		// loading static sounds
		if (HasLevelResource(ELevelResource::StaticSounds))
			LoadAuxiliaryResource(ELevelResource::StaticSounds);
		else
			SetResourceState(ELevelResource::StaticSounds, EResourceState::NotLoaded);

		// loading sound environment
		if (HasLevelResource(ELevelResource::SoundEnv))
		{
			if (IsParallel())
			{
				if (FS.exist(fn_game, "$level$", "level.snd_env"))
				{
					IReader* stream = FS.r_open(fn_game);
					xr_delete(SoundGeometry.Env);
					SoundGeometry.Env = Sound->create_geometry_env(stream, SoundGeometry.Zones);
					FS.r_close(stream);
				}
				if (FS.exist(fn_game, "$level$", "level.som"))
				{
					IReader* stream = FS.r_open(fn_game);
					xr_delete(SoundGeometry.Som);
					SoundGeometry.Som = Sound->create_geometry_som(stream);
					FS.r_close(stream);
				}
				SetResourceState(ELevelResource::SoundEnv, SoundGeometry.Env != nullptr || SoundGeometry.Som != nullptr ? EResourceState::Ready : EResourceState::Missing);
			}
			else
			{
				if ( FS.exist(fn_game, "$level$", "level.snd_env")) {
					IReader *F				= FS.r_open	(fn_game);
					::Sound->set_geometry_env(F);
					FS.r_close				(F);
				}
				// loading SOM
				if (FS.exist(fn_game, "$level$", "level.som")) {
					IReader *F				= FS.r_open	(fn_game);
					::Sound->set_geometry_som(F);
					FS.r_close				(F);
				}
			}
		}
		else
		{
			SetResourceState(ELevelResource::SoundEnv, EResourceState::NotLoaded);
		}

		// loading random (around player) sounds
		if (HasLevelResource(ELevelResource::Ambience))
			LoadAuxiliaryResource(ELevelResource::Ambience);
		else
			SetResourceState(ELevelResource::Ambience, EResourceState::NotLoaded);

		if (g_pGamePersistent->pEnvironment) {
			if (CEffect_Rain* rain = g_pGamePersistent->pEnvironment->eff_Rain) {
				rain->InvalidateState();
			}
		}

		if ( FS.exist(fn_game, "$level$", "level.fog_vol")) 
		{
			IReader *F				= FS.r_open	(fn_game);
			u16 version				= F->r_u16();
			if(version == 2)
			{
				u32 cnt					= F->r_u32();

				Fmatrix					volume_matrix;
				for(u32 i=0; i<cnt; ++i)
				{
					F->r				(&volume_matrix, sizeof(volume_matrix));
					u32 sub_cnt			= F->r_u32();
					for(u32 is=0; is<sub_cnt; ++is)
					{
						F->r			(&volume_matrix, sizeof(volume_matrix));
					}

				}
			}
			FS.r_close				(F);
		}
	}	

	// loading scripts
	ai().script_engine().remove_script_process(ScriptEngine::eScriptProcessorLevel);

	if (HasLevelResource(ELevelResource::LevelScripts))
		LoadAuxiliaryResource(ELevelResource::LevelScripts);
	else
		SetResourceState(ELevelResource::LevelScripts, EResourceState::NotLoaded);
		
	BlockCheatLoad();

	// game may legitimately be absent on degraded loads; fall back to the
	// persistent environment time factor then.
	const float timeFactor = game ? game->GetEnvironmentGameTimeFactor() : 1.f;
	g_pGamePersistent->Environment().SetGameTime	(GetEnvironmentGameDayTimeSec(), timeFactor);

	return true;
}

void CLevel::BlockCheatLoad()
{
#ifndef	DEBUG
	if( game && (!IsGameTypeSingle()) ) phTimefactor=1.f;
#endif
}

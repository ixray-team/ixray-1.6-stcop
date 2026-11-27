#include "StdAfx.h"
#include "Level.h"
#include "ai_space.h"
#include "level_graph.h"
#include "moving_objects.h"
#include "patrol_path_storage.h"
#include "physics_game.h"
#include "../xrPhysics/ODE/Physics.h"
#include "../xrPhysics/ODE/PHWorld.h"
#include "PHCommander.h"
#include "../xrEngine/Render.h"
#include "../xrEngine/device.h"
#include "../Include/xrRender/RenderDeviceRender.h"
#include "ParticlesObject.h"
#include "level_sounds.h"
#include "GamePersistent.h"
#include "HUDManager.h"
#include "../xrSound/New/SoundMixerInternal.h"
#include "../xrEngine/std_classes.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_process.h"
#include "Creature.h"
#include "movement_manager.h"
#include "GameObject.h"
#include "Actor.h"
#include "script_game_object.h"
#include "game_sv_single.h"
#include "ai/stalker/ai_stalker.h"

void CLevel::OnLevelGraphChanged()
{
	for (u32 i = 0; i < Objects.o_count(); ++i)
	{
		CObject* object = Objects.o_get_by_iterator(i);
		if (object == nullptr || object->getDestroy())
			continue;
		if (CGameObject* gameObject = object->cast_game_object(); gameObject != nullptr && gameObject->lua_game_object() != nullptr)
			gameObject->lua_game_object()->unregister_door();
		if (CAI_Stalker* stalker = object->cast_stalker(); stalker != nullptr)
			stalker->on_level_graph_change();
		if (CCreature* creature = object->cast_creature())
			creature->movement().on_level_graph_change();
	}
}

bool CLevel::LoadGameGraphResource()
{
	const char* spawnName = LevelDesc.SpawnFile.c_str();
	const bool explicitPath = strchr(spawnName, ':') != nullptr || strchr(spawnName, '/') != nullptr || strchr(spawnName, '\\') != nullptr;
	string_path path;
	if (explicitPath)
		xr_strcpy(path, spawnName);
	else if (IsGameTypeSingle())
	{
		FS.update_path(path, "$game_spawn$", spawnName);
		xr_strcat(path, ".spawn");
	}
	else
		FS.update_path(path, "$level$", "alife.spawn");
	IReader* stream = FS.r_open(path);
	if (stream == nullptr)
		return false;
	IReader* chunk = stream->open_chunk(4);
	if (chunk == nullptr)
	{
		FS.r_close(stream);
		return false;
	}

	bool loaded = false;
	if (ai().get_alife() != nullptr && RemoteServer == nullptr)
	{
		if (ai().game_graph().header().vertex_count() != 0)
		{
			ai().SetGameGraphEnabled(true);
			loaded = true;
		}
		if (loaded && !HasLevelResource(ELevelResource::Spawn) && Server != nullptr)
			Server->UnloadSpawnResource();
	}
	else
	{
		loaded = ai().LoadGameGraph(*chunk);
	}
	chunk->close();
	FS.r_close(stream);

	if (loaded)
	{
		ai().SetCrossTableEnabled(HasLevelResource(ELevelResource::CrossTable));
		if (HasLevelResource(ELevelResource::AiMap) && (ai().get_level_graph() == nullptr || !ai().level_graph().has_nodes()))
			ai().load(name().c_str());
		else
			ai().BindCurrentLevel(name().c_str());
	}
	return loaded;
}

bool CLevel::LoadAuxiliaryResource(ELevelResource resource)
{
	switch (resource)
	{
	case ELevelResource::StaticParticles:
	{
		string_path path;
		if (!FS.exist(path, "$level$", "level.ps_static"))
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		IReader* stream = FS.r_open(path);
		if (stream == nullptr)
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		u32 chunkId = 0;
		u32 version = 0;
		for (IReader* object = stream->open_chunk_iterator(chunkId); object != nullptr; object = stream->open_chunk_iterator(chunkId, object))
		{
			if (chunkId == 0 && object->length() == sizeof(u32))
			{
				version = object->r_u32();
				continue;
			}
			u16 gameTypeUsage = version != 0 ? object->r_u16() : 0;
			string256 reference;
			Fmatrix transform;
			object->r_stringZ(reference, sizeof(reference));
			object->r(&transform, sizeof(transform));
			transform.c.y += 0.01f;
			if ((g_pGamePersistent->m_game_params.m_e_game_type & EGameIDs(gameTypeUsage)) == 0 && version != 0)
				continue;
			xr_shared_ptr<CParticlesObject> particles = Particles::Details::Create(reference, false, false);
			if (particles == nullptr)
			{
				Msg("! Level particles '%s' are unavailable; skipping the emitter", reference);
				continue;
			}
			particles->UpdateParent(transform, Fvector().set(0.f, 0.f, 0.f));
			particles->Play(false);
			m_StaticParticles.push_back(particles);
		}
		FS.r_close(stream);
		SetResourceState(resource, EResourceState::Ready);
		return true;
	}

	case ELevelResource::StaticSounds:
		if (m_level_sound_manager == nullptr)
		{
			SetResourceState(resource, EResourceState::Failed);
			return false;
		}
		m_level_sound_manager->Unload();
		m_level_sound_manager->Load();
		if (!FS.exist("$level$", "level.snd_static"))
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		SetResourceState(resource, EResourceState::Ready);
		return true;

	case ELevelResource::Ambience:
		Sounds_Random.clear();
		Sounds_Random_Enabled = false;
		if (!pSettings->section_exist("sounds_random"))
		{
			Sounds_Random_dwNextTime = 0;
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		{
			CInifile::Sect& section = pSettings->r_section("sounds_random");
			Sounds_Random.reserve(section.Data.size());
			for (const auto& line : section.Data)
			{
				Sounds_Random.push_back(ref_sound());
				Sound->create(Sounds_Random.back(), *line.first, st_Effect, sg_SourceType);
			}
		}
		Sounds_Random_dwNextTime = SessionTimeMs() + 50000;
		SetResourceState(resource, EResourceState::Ready);
		return true;

	case ELevelResource::EnvModifiers:
		g_pGamePersistent->Environment().mods_load();
		if (!FS.exist("$level$", "level.env_mod"))
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		SetResourceState(resource, EResourceState::Ready);
		return true;

	case ELevelResource::LevelScripts:
		if (RemoteServer != nullptr)
		{
			SetResourceState(resource, EResourceState::NotLoaded);
			return true;
		}
		if (RemoteServer == nullptr)
			ai().script_engine().remove_script_process(ScriptEngine::eScriptProcessorLevel);
		if (pLevel == nullptr)
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		{
			const char* script = pLevel->section_exist("level_scripts") && pLevel->line_exist("level_scripts", "script")
				? pLevel->r_string("level_scripts", "script") : "";
			ai().script_engine().add_script_process(ScriptEngine::eScriptProcessorLevel, new CScriptProcess(ai().script_engine(), "level", script));
		}
		SetResourceState(resource, EResourceState::Ready);
		if (CObject* entity = CurrentControlEntity(); entity != nullptr)
		{
			CActor* actor = entity->cast_actor();
			if (actor != nullptr && actor->Local() && actor->object() == nullptr)
			{
				actor->CScriptBinder::reload(actor->cNameSect().c_str());
				actor->CScriptBinder::reinit();
				if (Server != nullptr)
				{
					if (CSE_Abstract* serverEntity = Server->ID_to_entity(actor->ID()))
						actor->CScriptBinder::net_Spawn(serverEntity);
				}
			}
		}
		return true;

	default:
		return false;
	}
}

bool CLevel::InitializePhysicsWorld()
{
	if (GetResourceState(ELevelResource::Collision) != EResourceState::Ready)
		return false;
	if (physics_world() != nullptr)
	{
		if (physics_world()->IsFreezed())
			physics_world()->UnFreeze();
		return true;
	}

	create_physics_world(!!psDeviceFlags.test(mtPhysics), &ObjectSpace, &Objects);
	if (physics_world() == nullptr)
		return false;
	if (m_ph_commander_physics_worldstep == nullptr)
		m_ph_commander_physics_worldstep = new CPHCommander();
	physics_world()->set_update_callback(m_ph_commander_physics_worldstep);
	physics_world()->set_default_contact_shotmark(ContactShotMark);
	physics_world()->set_default_character_contact_shotmark(CharacterContactShotMark);
	return true;
}

bool CLevel::SetResourceLoaded(ELevelResource resource, bool load)
{
	if (!IsParallel() || !bReady)
	{
		Msg("! CLevel::SetResourceLoaded: runtime changes require a loaded parallel level");
		return false;
	}
	if (load && GetResourceState(resource) == EResourceState::Ready)
		return true;

	switch (resource)
	{
	case ELevelResource::AiMap:
		if (load)
		{
			ParallelJobs.clear();
			OnLevelGraphChanged();
			LevelDesc.Resources.set(u32(resource), true);
			SetResourceState(resource, EResourceState::Loading);
			ai().load(name().c_str());
			OnLevelGraphChanged();
			const bool ready = ai().get_level_graph() != nullptr && ai().level_graph().has_nodes();
			SetResourceState(resource, ready ? EResourceState::Ready : EResourceState::Missing);
			if (HasLevelResource(ELevelResource::PatrolPaths))
			{
				SetResourceLoaded(ELevelResource::PatrolPaths, false);
				SetResourceLoaded(ELevelResource::PatrolPaths, true);
			}
			return ready;
		}
		ParallelJobs.clear();
		OnLevelGraphChanged();
		ai().moving_objects().on_level_unload();
		ai().unload_level_graph();
		ai().patrol_paths_raw().unload();
		OnLevelGraphChanged();
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		SetResourceState(ELevelResource::PatrolPaths, EResourceState::NotLoaded);
		return true;

	case ELevelResource::GameGraph:
		if (!load)
		{
			ParallelJobs.clear();
			OnLevelGraphChanged();
			ai().UnloadGameGraph();
			LevelDesc.Resources.set(u32(ELevelResource::GameGraph) | u32(ELevelResource::CrossTable), false);
			SetResourceState(ELevelResource::GameGraph, EResourceState::NotLoaded);
			SetResourceState(ELevelResource::CrossTable, EResourceState::NotLoaded);
			OnLevelGraphChanged();
			return true;
		}
		ParallelJobs.clear();
		OnLevelGraphChanged();
		LevelDesc.Resources.set(u32(resource), true);
		SetResourceState(resource, EResourceState::Loading);
		{
			const bool loaded = LoadGameGraphResource();
			OnLevelGraphChanged();
			SetResourceState(resource, loaded ? EResourceState::Ready : EResourceState::Missing);
			if (HasLevelResource(ELevelResource::CrossTable))
				SetResourceState(ELevelResource::CrossTable, ai().get_cross_table() != nullptr ? EResourceState::Ready : EResourceState::Missing);
			return loaded;
		}

	case ELevelResource::CrossTable:
		LevelDesc.Resources.set(u32(resource), load);
		if (!load)
		{
			OnLevelGraphChanged();
			ai().SetCrossTableEnabled(false);
			SetResourceState(resource, EResourceState::NotLoaded);
			OnLevelGraphChanged();
			return true;
		}
		if (ai().get_game_graph() == nullptr)
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		ai().SetCrossTableEnabled(true);
		{
			const bool bound = HasLevelResource(ELevelResource::AiMap) ? (ai().load(name().c_str()), ai().get_cross_table() != nullptr) : ai().BindCurrentLevel(name().c_str());
			OnLevelGraphChanged();
			SetResourceState(resource, bound ? EResourceState::Ready : EResourceState::Missing);
			return bound;
		}

	case ELevelResource::Spawn:
		LevelDesc.Resources.set(u32(resource), load);
		if (!load)
		{
			const bool unloaded = Server == nullptr || Server->UnloadSpawnResource();
			SetResourceState(resource, EResourceState::NotLoaded);
			return unloaded;
		}
		if ((Server == nullptr) || (ai().get_alife() != nullptr && ai().get_game_graph() == nullptr))
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		{
			const bool loaded = Server->LoadSpawnResource();
			SetResourceState(resource, loaded ? EResourceState::Ready : EResourceState::Missing);
			return loaded;
		}

	case ELevelResource::PatrolPaths:
		if (!load)
		{
			ParallelJobs.clear();
			OnLevelGraphChanged();
			ai().patrol_paths_raw().unload();
			LevelDesc.Resources.set(u32(resource), false);
			SetResourceState(resource, EResourceState::NotLoaded);
			return true;
		}
		LevelDesc.Resources.set(u32(resource), true);
		if (ai().get_level_graph() == nullptr || !ai().level_graph().has_nodes())
		{
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		{
			string_path path;
			if (!FS.exist(path, "$level$", "level.game"))
			{
				SetResourceState(resource, EResourceState::Missing);
				return false;
			}
			IReader* stream = FS.r_open(path);
			if (stream == nullptr)
			{
				SetResourceState(resource, EResourceState::Missing);
				return false;
			}
			ai().patrol_path_storage_raw(*stream);
			FS.r_close(stream);
			OnLevelGraphChanged();
			SetResourceState(resource, EResourceState::Ready);
			return true;
		}

	case ELevelResource::StaticParticles:
		if (load)
		{
			LevelDesc.Resources.set(u32(resource), true);
			return LoadAuxiliaryResource(resource);
		}
		for (xr_shared_ptr<CParticlesObject>& particles : m_StaticParticles)
			Particles::Details::Destroy(particles);
		m_StaticParticles.clear();
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		return true;

	case ELevelResource::StaticSounds:
		if (load)
		{
			LevelDesc.Resources.set(u32(resource), true);
			return LoadAuxiliaryResource(resource);
		}
		if (m_level_sound_manager != nullptr)
			m_level_sound_manager->Unload();
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		return true;

	case ELevelResource::SoundEnv:
		if (!load)
		{
			ReleaseSoundGeometry();
			if (GetResourceState(ELevelResource::Collision) == EResourceState::Ready)
			{
				SoundGeometry.Occluder = &ObjectSpace;
				if (Sound->get_geometry_occ() != SoundGeometry.Occluder)
					Sound->swap_geometry_occ(SoundGeometry.Occluder);
			}
			LevelDesc.Resources.set(u32(resource), false);
			SetResourceState(resource, EResourceState::NotLoaded);
			return true;
		}

		LevelDesc.Resources.set(u32(resource), true);
		SetResourceState(resource, EResourceState::Loading);
		{
			CDB::MODEL* environment = nullptr;
			CDB::MODEL* occlusion = nullptr;
			SoundZoneVector zones;
			string_path path;
			if (FS.exist(path, "$level$", "level.snd_env"))
			{
				IReader* stream = FS.r_open(path);
				if (stream != nullptr)
				{
					environment = Sound->create_geometry_env(stream, zones);
					FS.r_close(stream);
				}
			}
			if (FS.exist(path, "$level$", "level.som"))
			{
				IReader* stream = FS.r_open(path);
				if (stream != nullptr)
				{
					occlusion = Sound->create_geometry_som(stream);
					FS.r_close(stream);
				}
			}

			if (environment == nullptr && occlusion == nullptr)
			{
				xr_delete(environment);
				xr_delete(occlusion);
				SetResourceState(resource, EResourceState::Missing);
				return false;
			}

			CDB::MODEL* previousEnvironment = SoundGeometry.Env;
			CDB::MODEL* previousOcclusion = SoundGeometry.Som;
			SoundGeometry.Env = environment;
			SoundGeometry.Som = occlusion;
			SoundGeometry.Zones = std::move(zones);
			SoundGeometry.Occluder = GetResourceState(ELevelResource::Collision) == EResourceState::Ready ? &ObjectSpace : nullptr;
			if (Sound->get_geometry_occ() != SoundGeometry.Occluder)
				Sound->swap_geometry_occ(SoundGeometry.Occluder);
			if (Sound->get_geometry_env() == previousEnvironment)
			{
				Sound->swap_geometry_env(environment);
				XRay::Sound::Mixer::SwapZones(SoundGeometry.Zones);
			}
			if (Sound->get_geometry_som() == previousOcclusion)
				Sound->swap_geometry_som(occlusion);
			xr_delete(previousEnvironment);
			xr_delete(previousOcclusion);
			Msg("* Level sound environment: model=%u triangles, SOM=%u triangles, zones=%u",
				environment != nullptr ? u32(environment->get_tris().size()) : 0,
				occlusion != nullptr ? u32(occlusion->get_tris().size()) : 0, u32(SoundGeometry.Zones.size()));
			SetResourceState(resource, EResourceState::Ready);
			return true;
		}

	case ELevelResource::Ambience:
		if (load)
		{
			LevelDesc.Resources.set(u32(resource), true);
			return LoadAuxiliaryResource(resource);
		}
		Sounds_Random.clear();
		Sounds_Random_dwNextTime = 0;
		Sounds_Random_Enabled = false;
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		return true;

	case ELevelResource::EnvModifiers:
		if (load)
		{
			LevelDesc.Resources.set(u32(resource), true);
			return LoadAuxiliaryResource(resource);
		}
		g_pGamePersistent->Environment().mods_unload();
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		return true;

	case ELevelResource::LevelScripts:
		if (load)
		{
			LevelDesc.Resources.set(u32(resource), true);
			return LoadAuxiliaryResource(resource);
		}
		if (RemoteServer == nullptr)
			ai().script_engine().remove_script_process(ScriptEngine::eScriptProcessorLevel);
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		return true;

	case ELevelResource::Hud:
		if (load)
		{
			if (GetResourceState(resource) == EResourceState::Ready)
				return true;
			if (RemoteServer == nullptr && GetResourceState(ELevelResource::LevelScripts) != EResourceState::Ready && !SetResourceLoaded(ELevelResource::LevelScripts, true))
			{
				SetResourceState(resource, EResourceState::Missing);
				Msg("! Level resource 'hud' requires an initialized level script process");
				return false;
			}
			LevelDesc.Resources.set(u32(resource), true);
			if (g_hud == nullptr)
				g_hud = static_cast<CCustomHUD*>(NEW_INSTANCE(CLSID_HUDMANAGER));
			if (g_hud != nullptr)
			{
				g_hud->Load();
				g_hud->OnConnected();
			}
			SetResourceState(resource, g_hud != nullptr ? EResourceState::Ready : EResourceState::Failed);
			return g_hud != nullptr;
		}
		if (g_hud != nullptr)
			g_hud->OnDisconnected();
		LevelDesc.Resources.set(u32(resource), false);
		SetResourceState(resource, EResourceState::NotLoaded);
		return true;

	case ELevelResource::LevelConfig:
	{
		CInifile* replacement = nullptr;
		string_path path;
		const bool present = load && FS.exist(path, "$level$", "level.ltx");
		if (present)
		{
			replacement = new CInifile(path);
		}
		else
		{
			static const char emptyConfig[] = "; empty level config\r\n";
			IReader reader(const_cast<char*>(emptyConfig), sizeof(emptyConfig) - 1);
			replacement = new CInifile(&reader);
		}
		if (HasLevelResource(ELevelResource::LevelScripts))
		{
			if (RemoteServer == nullptr)
			ai().script_engine().remove_script_process(ScriptEngine::eScriptProcessorLevel);
			SetResourceState(ELevelResource::LevelScripts, EResourceState::NotLoaded);
		}
		xr_delete(pLevel);
		pLevel = replacement;
		LevelDesc.Resources.set(u32(resource), load);
		SetResourceState(resource, load ? (present ? EResourceState::Ready : EResourceState::Missing) : EResourceState::NotLoaded);
		if (load && HasLevelResource(ELevelResource::LevelScripts))
			SetResourceState(ELevelResource::LevelScripts, LoadAuxiliaryResource(ELevelResource::LevelScripts) ? EResourceState::Ready : EResourceState::Missing);
		return load ? present : true;
	}

	case ELevelResource::Collision:
		if (!load)
		{
			if (physics_world() != nullptr && !physics_world()->IsFreezed() && HasLevelResource(ELevelResource::PhysicsWorld))
			{
				physics_world()->Freeze();
				SetResourceState(ELevelResource::PhysicsWorld, EResourceState::NotLoaded);
			}
			{
				xrSRWLockGuard audioGuard(XRay::Sound::Mixer::GetRenderMutex());
				ObjectSpace.ClearStaticGeometry();
			}
			SoundGeometry.Occluder = &ObjectSpace;
			if (Sound->get_geometry_occ() != SoundGeometry.Occluder)
				Sound->swap_geometry_occ(SoundGeometry.Occluder);
			LevelDesc.Resources.set(u32(resource), false);
			SetResourceState(resource, EResourceState::NotLoaded);
			return true;
		}
		{
			CObjectSpace candidate;
			const bool loaded = candidate.Load("$level$", "level",
				[](Fvector* vertices, size_t vertexCount, CDB::TRI* triangles, size_t triangleCount, void* params)
				{
					g_pGameLevel->Load_GameSpecific_CFORM(triangles, triangleCount);
				}, false, false);
			if (!loaded)
			{
				LevelDesc.Resources.set(u32(resource), true);
				SetResourceState(resource, EResourceState::Missing);
				return false;
			}
			{
				xrSRWLockGuard audioGuard(XRay::Sound::Mixer::GetRenderMutex());
				ObjectSpace.SwapStaticGeometry(candidate);
			}
			SoundGeometry.Occluder = &ObjectSpace;
			if (Sound->get_geometry_occ() != SoundGeometry.Occluder)
				Sound->swap_geometry_occ(SoundGeometry.Occluder);
			LevelDesc.Resources.set(u32(resource), true);
			SetResourceState(resource, EResourceState::Ready);
			if (HasLevelResource(ELevelResource::PhysicsWorld))
				SetResourceState(ELevelResource::PhysicsWorld, InitializePhysicsWorld() ? EResourceState::Ready : EResourceState::Missing);
			return true;
		}

	case ELevelResource::PhysicsWorld:
		if (!load)
		{
			if (physics_world() != nullptr && !physics_world()->IsFreezed())
				physics_world()->Freeze();
			LevelDesc.Resources.set(u32(resource), false);
			SetResourceState(resource, EResourceState::NotLoaded);
			return true;
		}
		if (GetResourceState(ELevelResource::Collision) != EResourceState::Ready)
		{
			Msg("! CLevel::SetResourceLoaded: physics requires collision; load collision first");
			SetResourceState(resource, EResourceState::Missing);
			return false;
		}
		LevelDesc.Resources.set(u32(resource), true);
		if (!InitializePhysicsWorld())
		{
			SetResourceState(resource, EResourceState::Failed);
			return false;
		}
		SetResourceState(resource, EResourceState::Ready);
		return true;

	case ELevelResource::RenderScene:
		if (Render == nullptr)
		{
			SetResourceState(resource, EResourceState::Failed);
			return false;
		}
		if (!load)
		{
			Render->level_Unload();
			LevelDesc.Resources.set(u32(resource), false);
			HasRenderScene = false;
			SetResourceState(resource, EResourceState::NotLoaded);
			return true;
		}
		LevelDesc.Resources.set(u32(resource), true);
		SetResourceState(resource, EResourceState::Loading);
		{
			IReader* stream = FS.r_open("$level$", "level");
			if (stream == nullptr)
			{
				SetResourceState(resource, EResourceState::Missing);
				return false;
			}
			Render->level_Unload();
			Render->level_Load(stream);
			FS.r_close(stream);
			Device.m_pRender->ResourcesDeferredUpload();
			HasRenderScene = Render->HasLevelScene();
			SetResourceState(resource, HasRenderScene ? EResourceState::Ready : EResourceState::Missing);
			return HasRenderScene;
		}

	default:
		Msg("! CLevel::SetResourceLoaded: '%s' does not support runtime changes yet", LevelResourceName(resource).data());
		return false;
	}
}

#include "stdafx.h"
#include "IGame_Level.h"
#include "IGame_Persistent.h"

#include "CameraManager.h"
#include "CustomHUD.h"
#include "Feel_Sound.h"
#include "../xrSound/ai_sounds.h"
#include "../xrCore/Kernel/EngineExternal.h"
#include "Render.h"
#include "../xrSound/New/SoundMixerInternal.h"
#include "std_classes.h"
#include "xrLevel.h"
#include "xr_object.h"
#include "x_ray.h"
#include "GameMtlLib.h"
#include "Engine.h"
#include "../xrGame/AnimNotify/AnimNotifyGame.h"

ENGINE_API	IGame_Level*	g_pGameLevel	= nullptr;
extern	bool g_bLoaded;

SLevelServices* CurrentLevelServices()
{
	return g_pGameLevel != nullptr ? &g_pGameLevel->Services : nullptr;
}

void SeqParallelPush(const xr_delegate<void()>& job)
{
	xrCriticalSectionGuard guard(&Engine.Sheduler.UpdateMutex());
	if (g_pGameLevel != nullptr && g_pGameLevel->IsParallel())
		g_pGameLevel->ParallelJobs.push_back(job);
	else
		Device.seqParallel.push_back(job);
}

void SeqParallelRemove(const xr_delegate<void()>& job)
{
	xrCriticalSectionGuard guard(&Engine.Sheduler.UpdateMutex());
	if (g_pGameLevel != nullptr && g_pGameLevel->IsParallel())
		std::erase(g_pGameLevel->ParallelJobs, job);
	else
		Device.remove_from_seq_parallel(job);
}

IGame_Level::IGame_Level() : Objects(this)
{
	PROF_EVENT("IGame_Level::IGame_Level");
	m_pCameras					= new CCameraManager(true);
	// Must stay assigned during construction: member constructors (e.g.
	// pure_relcase) reach the level through the global. Viewer-driven parallel
	// sessions save/restore the slot around NEW_INSTANCE instead.
	g_pGameLevel				= this;
	// A pending descriptor (set right before NEW_INSTANCE by the start flow or
	// the viewer) applies to this instance from the very first member init, so
	// the ctor chain can already see Sandbox/Parallel/resource-mask flags.
	SLevelDesc pendingDesc;
	if (pApp != nullptr && pApp->ConsumeNextLevelDesc(pendingDesc))
		SetLevelDesc(pendingDesc);
	pLevel						= nullptr;
	bReady						= false;
	pCurrentEntity				= nullptr;
	pCurrentViewEntity			= nullptr;
	pCurrentControlEntity		= nullptr;
#ifdef DEBUG
	if (!Device.IsEditorMode())
	{
		Device.DumpResourcesMemoryUsage();
	}
#endif // DEBUG
}

IGame_Level::~IGame_Level() noexcept
{
	StopCollisionStreaming();
	if (Core.ParamsData.test(ECoreParams::nes_texture_storing))
		Device.m_pRender->ResourcesStoreNecessaryTextures();

	xr_delete(pLevel);

	// Render-level unload (only when this session actually owns the renderer's
	// scene: a parallel session without one must not drop the game's scene).
	if (!LevelDesc.Parallel || HasRenderScene)
		Render->level_Unload();
	xr_delete(m_pCameras);
	// Unregister (harmless when the level never registered, e.g. parallel sessions)
	if (!g_dedicated_server)
	{
		Device.seqRender.Remove(this);
	}

	Device.seqFrame.Remove(this);
	CCameraManager::ResetPP();

	if (!LevelDesc.Parallel)
	{
		Sound->set_geometry_occ(nullptr);
		Sound->set_handler(nullptr);
	}

	if (g_pGameLevel == this)
		g_pGameLevel = nullptr;

#ifdef DEBUG
	if (!Device.IsEditorMode())
		Device.DumpResourcesMemoryUsage();
#endif // DEBUG

	ReleaseSoundGeometry();

	u32		m_base = 0, c_base = 0, m_lmaps = 0, c_lmaps = 0;
	if (Device.m_pRender)
		Device.m_pRender->ResourcesGetMemoryUsage(m_base, c_base, m_lmaps, c_lmaps);

	Msg("* [ D3D ]: textures[%d K]", (m_base + m_lmaps) / 1024);
}

void IGame_Level::ReleaseSoundGeometry()
{
	if (!LevelDesc.Parallel || Sound == nullptr)
		return;

	if (Sound->get_geometry_env() == SoundGeometry.Env)
	{
		SoundZoneVector emptyZones;
		Sound->swap_geometry_env(nullptr);
		XRay::Sound::Mixer::SwapZones(emptyZones);
	}
	if (Sound->get_geometry_som() == SoundGeometry.Som)
		Sound->swap_geometry_som(nullptr);
	if (Sound->get_geometry_occ() == SoundGeometry.Occluder)
		Sound->swap_geometry_occ(nullptr);

	xr_delete(SoundGeometry.Env);
	xr_delete(SoundGeometry.Som);
	SoundGeometry.Occluder = nullptr;
	SoundGeometry.Zones.clear();
}

void IGame_Level::net_Stop			()
{
	StopCollisionStreaming();
	for (int i=0; i<6; i++)
		Objects.Update			(false);
	// Destroy all objects
	Objects.Unload				( );
	IR_Release					( );

	bReady						= false;
}

//-------------------------------------------------------------------------------------------
//extern CStatTimer				tscreate;
void  _sound_event		(ref_sound_data_ptr S, float range)
{
	if ( g_pGameLevel && S && S->slot )	g_pGameLevel->SoundEvent_Register	(S,range);
}

void IGame_Level::Load_GameSpecific_CFORM(CDB::TRI* tris, size_t count)
{
	struct translation_pair
	{
		u32 m_id;
		u16 m_index;

		IC translation_pair(u32 id, u16 index)
		{
			m_id = id;
			m_index = index;
		}

		IC bool operator==(const u16& id) const
		{
			return (m_id == id);
		}

		IC bool operator<(const translation_pair& pair) const
		{
			return (m_id < pair.m_id);
		}

		IC bool operator<(const u16& id) const
		{
			return (m_id < id);
		}
	};

	typedef xr_vector<translation_pair>	ID_INDEX_PAIRS;
	ID_INDEX_PAIRS						translator;
	translator.reserve(GMLib.CountMaterial());
	u16									default_id = (u16)GMLib.GetMaterialIdx("default");
	translator.push_back(translation_pair(u32(-1), default_id));

	u16									index = 0, static_mtl_count = 1;
	int max_ID = 0;
	int max_static_ID = 0;
	for (GameMtlIt I = GMLib.FirstMaterial(); GMLib.LastMaterial() != I; ++I, ++index) {
		if (!(*I)->Flags.test(SGameMtl::flDynamic)) {
			++static_mtl_count;
			if (Device.IsEditorMode())
				translator.push_back(translation_pair((*I)->GetID(), (*I)->GetID()));
			else
				translator.push_back(translation_pair((*I)->GetID(), index));
			if ((*I)->GetID() > max_static_ID)	max_static_ID = (*I)->GetID();
		}
		if ((*I)->GetID() > max_ID)				max_ID = (*I)->GetID();
	}
	// Msg("* Material remapping ID: [Max:%d, StaticMax:%d]",max_ID,max_static_ID);
	VERIFY(max_static_ID < 0xFFFF);

	if (static_mtl_count < 128) {
		CDB::TRI* I = tris;
		CDB::TRI* E = tris + count;
		for (; I != E; ++I) {
			ID_INDEX_PAIRS::iterator	i = std::find(translator.begin(), translator.end(), (u16)(*I).material);
			if (i != translator.end()) {
				(*I).material = (*i).m_index;
				SGameMtl* mtl = GMLib.GetMaterialByIdx((*i).m_index);
				(*I).suppress_shadows = mtl->Flags.is(SGameMtl::flSuppressShadows);
				(*I).suppress_wm = mtl->Flags.is(SGameMtl::flSuppressWallmarks);
				continue;
			}
			if (Device.IsEditorMode() == false)
				Debug.fatal(DEBUG_INFO, "Game material '%d' not found", (*I).material);
		}
		return;
	}

	std::sort(translator.begin(), translator.end());
	{
		CDB::TRI* I = tris;
		CDB::TRI* E = tris + count;
		for (; I != E; ++I) {
			ID_INDEX_PAIRS::iterator	i = std::lower_bound(translator.begin(), translator.end(), (u16)(*I).material);
			if ((i != translator.end()) && ((*i).m_id == (*I).material)) {
				(*I).material = (*i).m_index;
				SGameMtl* mtl = GMLib.GetMaterialByIdx((*i).m_index);
				(*I).suppress_shadows = mtl->Flags.is(SGameMtl::flSuppressShadows);
				(*I).suppress_wm = mtl->Flags.is(SGameMtl::flSuppressWallmarks);
				continue;
			}
			if (Device.IsEditorMode() == false)
				Debug.fatal(DEBUG_INFO, "Game material '%d' not found", (*I).material);
		}
	}
}

void IGame_Level::SetResourceState(ELevelResource res, EResourceState state)
{
	ResourceStates[ResourceStateIndex(res)] = state;
	if (state == EResourceState::Missing || state == EResourceState::Failed)
	{
		Msg("! Level resource '%s' -> %s (continuing degraded)",
			LevelResourceName(res).data(), LevelResourceStateName(state).data());
	}
}

static xrCriticalSection lloadcs;
bool IGame_Level::Load			(u32 dwNum)
{
	PROF_EVENT("IGame_Level::Load");
	xrCriticalSectionGuard guard(lloadcs);
	if (bReady) return true;
	StopCollisionStreaming();
	FirstStreamingUpdate = true;
	StreamingStopped = false;

	const bool wantRenderScene = HasLevelResource(ELevelResource::RenderScene);
	const bool wantCollision   = HasLevelResource(ELevelResource::Collision);
	// Initialize level data
	pApp->Level_Set				( dwNum );
	string_path					temp;
	if (!HasLevelResource(ELevelResource::LevelConfig))
	{
		static const char emptyIni[] = "; level config not requested\r\n";
		IReader emptyReader((void*)emptyIni, sizeof(emptyIni) - 1);
		pLevel = new CInifile(&emptyReader);
		SetResourceState(ELevelResource::LevelConfig, EResourceState::NotLoaded);
	}
	else if (!FS.exist(temp, "$level$", "level.ltx"))
	{
		Msg("! IGame_Level::Load: can't find level configuration file '%s', using empty config", temp);
		static const char emptyIni[] = "; empty level config (missing level.ltx)\r\n";
		IReader emptyReader((void*)emptyIni, sizeof(emptyIni) - 1);
		pLevel						= new CInifile(&emptyReader);
		SetResourceState(ELevelResource::LevelConfig, EResourceState::Missing);
	}
	else
	{
		pLevel						= new CInifile	( temp );
		SetResourceState(ELevelResource::LevelConfig, EResourceState::Ready);
	}

	UseSnowmask = true;
	if (pLevel->section_exist("weather") && pLevel->line_exist("weather", "snowmask"))
	{
		UseSnowmask = !!pLevel->r_bool("weather", "snowmask");
	}

	// Open the renderer scene stream only when the render scene is requested
	IReader* LL_Stream			= nullptr;
	if (wantRenderScene)
	{
		g_pGamePersistent->SetLoadStageTitle	("st_opening_stream");
		g_pGamePersistent->LoadTitle	();

		LL_Stream					= FS.r_open	("$level$","level");
		if (LL_Stream == nullptr)
		{
			Msg("! IGame_Level::Load: '$level$/level' is missing, continuing without render scene");
			SetResourceState(ELevelResource::RenderScene, EResourceState::Missing);
		}
		else
		{
			// Header
			hdrLEVEL				H;
			LL_Stream->r_chunk_safe	(fsL_HEADER,&H,sizeof(H));
			if (XRCL_PRODUCTION_VERSION != H.XRLC_version)
			{
				Msg("! IGame_Level::Load: incompatible level version (%d != %d), continuing without render scene",
					H.XRLC_version, XRCL_PRODUCTION_VERSION);
				FS.r_close			(LL_Stream);
				SetResourceState(ELevelResource::RenderScene, EResourceState::Failed);
			}
		}
	}
	else
	{
		SetResourceState(ELevelResource::RenderScene, EResourceState::NotLoaded);
	}

	// CForms
	g_pGamePersistent->SetLoadStageTitle("st_loading_cform");
	g_pGamePersistent->LoadTitle();

	if (wantCollision)
	{
		const bool collisionOk = ObjectSpace.Load
		(
			[](Fvector * V, size_t Vcnt, CDB::TRI * T, size_t Tcnt, void* params)
			{
				g_pGameLevel->Load_GameSpecific_CFORM(T, Tcnt);
			}
		);
		SetResourceState(ELevelResource::Collision, collisionOk ? EResourceState::Ready : EResourceState::Missing);
	}
	else
	{
		ObjectSpace.CreateEmpty	();
		SetResourceState(ELevelResource::Collision, EResourceState::NotLoaded);
	}

	//Sound->set_geometry_occ		( &Static );
	if (!LevelDesc.Parallel)
	{
		Sound->set_geometry_occ(&ObjectSpace);
		Sound->set_handler(_sound_event);
	}
	else
	{
		SoundGeometry.Occluder = &ObjectSpace;
	}

	pApp->LoadSwitch			();


	// HUD + Environment
	if (!HasLevelResource(ELevelResource::Hud))
	{
		SetResourceState(ELevelResource::Hud, EResourceState::NotLoaded);
	}
	else
	{
		if (!g_hud)
			g_hud = (CCustomHUD*)NEW_INSTANCE(CLSID_HUDMANAGER);
		SetResourceState(ELevelResource::Hud, g_hud ? EResourceState::Ready : EResourceState::Missing);
	}

	// Render-level Load
	HasRenderScene				= false;
	if (LL_Stream != nullptr)
	{
		try {
			Render->level_Unload(); // try to unload old data
		}
		catch (...) {
			Msg("Level data empty: %s", temp);
		}
		try {
			Render->level_Load		(LL_Stream);
			HasRenderScene			= Render->HasLevelScene();
			SetResourceState(ELevelResource::RenderScene, HasRenderScene ? EResourceState::Ready : EResourceState::Missing);
		}
		catch (...) {
			Msg("! IGame_Level::Load: renderer failed to load the scene, continuing without it");
			SetResourceState(ELevelResource::RenderScene, EResourceState::Failed);
		}
	}

	// Objects
	if (!Load_GameSpecific_Before())
		Msg("! IGame_Level::Load: Load_GameSpecific_Before reported failure, continuing degraded");

	Objects.Load				();

	// Done
	FS.r_close					( LL_Stream );
	bReady						= true;

	// Parallel sessions are pumped by their owner (viewer); they must not
	// register into the global frame sequence or capture input.
	if (!LevelDesc.Parallel)
	{
		if (!g_dedicated_server) {
			IR_Capture();

			Device.seqRender.Add(this);
		}

		Device.seqFrame.Add			(this);
	}
	return true;
}

int		psNET_DedicatedSleep	= 5;
void	IGame_Level::OnRender		( )
{
	if (!g_dedicated_server && HasRenderScene) {
		{
			PROF_EVENT("IGame_Level::OnRender: Calculate");
			Render->Calculate();
		}
		{
			PROF_EVENT("IGame_Level::OnRender: Render");
			Render->Render();
		}
	}
}


void IGame_Level::OnFrame()
{
	PROF_EVENT("IGame_Level::OnFrame");

	VERIFY(bReady);

	if (IAnimNotifyHandler::IsValid())
	{
		IAnimNotifyHandler::Get().Update();
	}

	std::erase_if(scheduled_tasks, [&](const ScheduledCallbackItem& item)
	{
		if (SessionTimeMs() >= item.execution_time)
		{
			item.task();
			return true;
		}
		return false;
	});

	UpdateCollisionStreaming();
	Objects.Update(LevelDesc.Parallel);
	CCustomHUD* hud = Services.Hud != nullptr ? Services.Hud : g_hud;
	if (hud != nullptr)
		hud->OnFrame();

	// Ambience
	if (!Sounds_Random.empty() && SessionTimeMs() > Sounds_Random_dwNextTime)
	{
		Sounds_Random_dwNextTime = SessionTimeMs() + Random.randI(10000, 20000);
		Fvector pos;
		pos.random_dir().normalize().mul(Random.randF(30, 100)).add(Device.vCameraPosition);
		int id = Random.randI(Sounds_Random.size());
		if (Sounds_Random_Enabled)
		{
			Sounds_Random[id].play_at_pos(nullptr, pos, 0);
			Sounds_Random[id].set_volume(1.f);
			Sounds_Random[id].set_range(10, 200);
		}
	}
}
// ==================================================================================================

void IGame_Level::UpdateCollisionStreaming()
{
	if (StreamingStopped || !ObjectSpace.IsStreamingEnabled())
	{
		return;
	}
	// Camera and tuning belong to the frame thread, never read them from a worker.
	Fvector Position = Device.vCameraPosition;
	const float LoadRadius = StreamingLoadRadius;
	const float UnloadRadius = StreamingUnloadRadius;
	if (FirstStreamingUpdate)
	{
		// The camera may still belong to the previous level until the first object tick.
		if (CObject* Entity = CurrentViewEntity() ? CurrentViewEntity() : CurrentEntity())
		{
			Position = Entity->Position();
		}
		ObjectSpace.UpdateStreaming(Position, LoadRadius, UnloadRadius);
		FirstStreamingUpdate = false;
		return;
	}
	if (StreamingTaskRunning.load(std::memory_order_acquire))
	{
		return;
	}
	// Only publication runs on the frame thread, before Objects.Update/physics.
	// Preparation leaves the live maps and their revision untouched.
	ObjectSpace.ApplyStreamingUpdate(std::move(PendingStreamingUpdate));
	StreamingTaskRunning.store(true, std::memory_order_release);
	StreamingTaskScheduled = true;
	Device.async_tasks.run([this, Position, LoadRadius, UnloadRadius]()
	{
		xr_scope_exit Complete = [this]() { StreamingTaskRunning.store(false, std::memory_order_release); };
		PROF_EVENT("CollisionStreamingTask");
		PendingStreamingUpdate = ObjectSpace.PrepareStreamingUpdate(Position, LoadRadius, UnloadRadius);
	});
}

void IGame_Level::StopCollisionStreaming()
{
	StreamingStopped = true;
	if (StreamingTaskScheduled)
	{
		// Drain the group even if Running is already false: the callable must finish
		// before this level, its format or the global material callback can disappear.
		Device.async_tasks.wait();
		StreamingTaskScheduled = false;
	}
	PendingStreamingUpdate.reset();
}

void CServerInfo::AddItem( const char* name_, const char* value_, u32 color_ )
{
	shared_str s_name( name_ );
	AddItem( s_name, value_, color_ );
}

void CServerInfo::AddItem( shared_str& name_, const char* value_, u32 color_ )
{
	SItem_ServerInfo it;
	//	shared_str s_name = CStringTable().translate( name_ );

	//	xr_strcpy( it.name, s_name.c_str() );
	xr_strcpy( it.name, name_.c_str() );
	xr_strcat( it.name, " = " );
	xr_strcat( it.name, value_ );
	it.color = color_;

	if ( data.size() < max_item )
	{
		data.push_back( it );
	}
}

void IGame_Level::SetEntity( CObject* O  )
{
	if (pCurrentEntity)
		pCurrentEntity->On_LostEntity();

	if (O)
		O->On_SetEntity();

	pCurrentEntity=pCurrentViewEntity=O;
}

void IGame_Level::SetViewEntity( CObject* O  )
{
	if (pCurrentViewEntity)
		pCurrentViewEntity->On_LostEntity();

	if (O)
		O->On_SetEntity();

	pCurrentViewEntity=O;
}

void IGame_Level::SoundEvent_Register( ref_sound_data_ptr S, float range )
{
	PROF_EVENT("IGame_Level::SoundEvent_Register");
	if (!g_bLoaded) return;
	if (!S) return;
	if (S->g_object && S->g_object->getDestroy()) {S->g_object=nullptr; return;}
	if (0u==S->slot) return;

	clamp(range,0.1f,500.f);

	CSound_params p = S->get_params();
	Fvector snd_position = p.position;
	if(S->is_2d())
		snd_position.add(Sound->listener_position());

	VERIFY(_valid(range) );
	range = std::min(range,p.max_ai_distance);
	if (S->g_type != int(0xffffffff) && (S->g_type & SOUND_TYPE_SUPPRESSED)) range *= .3f;
	VERIFY(_valid(snd_position));
	VERIFY(_valid(p.max_ai_distance));
	VERIFY(_valid(p.volume));

	// Monster distance hearing must not be clipped by asset range or occlusion.
	const float QueryRange = EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ? 500.f : range;
	ISpatial_DB* spatial = Services.Spatial != nullptr ? Services.Spatial : g_SpatialSpace;
	if (spatial != nullptr)
		spatial->q_box(snd_ER, 0, ESPATIAL_TYPE::REACTTOSOUND, snd_position, { QueryRange,QueryRange,QueryRange });

	// Iterate
	for (auto& spatial : snd_ER)
	{
		Feel::Sound* L = spatial->dcast_FeelSound();
		if (0==L) continue;
		CObject* CO = spatial->dcast_CObject();
		VERIFY(CO);
		if (CO->getDestroy()) continue;

		// Energy and signal
		VERIFY(_valid(spatial->sphere.P));
		float dist = snd_position.distance_to(spatial->sphere.P);
		const float DistanceRange = L->GetDistanceSoundRange(S->g_type, S->g_object);
		if (DistanceRange >= 0.f)
		{
			const float HearingFactor = std::clamp(L->GetHearingRangeFactor(), 0.f, 1.f);
			if (DistanceRange > 0.f && HearingFactor > 0.f && dist <= DistanceRange * HearingFactor)
			{
				snd_Events.push_back({ L, S, 1.f, snd_position });
			}
			continue;
		}
		if (S->g_type != int(0xffffffff) && (S->g_type & SOUND_TYPE_MELEE)) continue;
		if (dist>p.max_ai_distance) continue;
		const float HearingFactor = std::clamp(L->GetHearingRangeFactor(), 0.f, 1.f);
		if (HearingFactor <= 0.f || dist > range * HearingFactor) continue;
		VERIFY(_valid(dist));
		float Power = (1.f-dist/p.max_ai_distance)*p.volume;
		VERIFY(_valid(Power));
		if (Power>EPS_S)
		{
			float occ = Sound->get_occlusion_to(spatial->sphere.P,snd_position);
			VERIFY(_valid(occ));
			Power *= occ;
			if (Power>EPS_S)
				snd_Events.push_back({ L, S, Power, snd_position });
		}
	}
}

void IGame_Level::SoundEvent_Dispatch()
{
	PROF_EVENT("IGame_Level::SoundEvent_Dispatch");
	for(_esound_delegate& D : snd_Events)
	{
		VERIFY(D.dest && D.source);
		if (D.source->slot)
		{
			D.dest->feel_sound_new
			(
				D.source->g_object,
				D.source->g_type,
				D.source->g_userdata,
				D.Position,
				D.power
			);
		}
	}

	snd_Events.clear();
}

// Lain: added
void   IGame_Level::SoundEvent_OnDestDestroy (Feel::Sound* obj)
{
	PROF_EVENT("IGame_Level::SoundEvent_OnDestDestroy");
	if(!snd_Events.empty())
		snd_Events.erase(std::remove_if(snd_Events.begin(), snd_Events.end(), [obj](const _esound_delegate& d) {return d.dest == obj; }), snd_Events.end() );
}

void   IGame_Level::SoundEvent_net_Relcase(CObject* obj)
{
	PROF_EVENT("IGame_Level::SoundEvent_net_Relcase");
	if (!snd_Events.empty())
		snd_Events.erase(std::remove_if(snd_Events.begin(), snd_Events.end(), [obj](const _esound_delegate& d) {return d.source->g_object == obj; }), snd_Events.end());
}

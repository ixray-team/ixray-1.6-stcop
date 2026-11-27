#include "StdAfx.h"
#include "pch_script.h"
#include "GamePersistent.h"
#include "LevelStartInfo.h"
#include "ImUtils/ImUtils.h"
#include "../xrEngine/Fmesh.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/GameMtlLib.h"
#include "../Include/xrRender/Kinematics.h"
#include "MainMenu.h"
#include "../xrUI/UICursor.h"
#include "game_base_space.h"
#include "Level.h"
#include "game_base_space.h"
#include "stalker_animation_data_storage.h"
#include "stalker_velocity_holder.h"

#include "ActorEffector.h"
#include "Actor.h"
#include "Spectator.h"

#include "../xrUI/UITextureMaster.h"

#include "ai_space.h"
#include "../xrScripts/script_engine.h"

#include "holder_custom.h"
#include "game_cl_base.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "../xrServerEntities/xrServer_Object_Base.h"
#include "ui/UIGameTutorial.h"

#include "../xrEngine/EngineApp.h"
#include "ui/UILoadingScreen.h"

#ifndef MASTER_GOLD
#	include "Creature.h"
#endif // MASTER_GOLD

#include "ai_debug.h"
#include "../../xrUI/ui_base.h"
#include "../xrCore/discord/discord.h"
#include "../xrEngine/string_table.h"
#include "Level_Bullet_Manager.h"
#include "ParticlesObject.h"
#include "../xrEngine/xr_level_controller.h"
#include "../xrEngine/x_ray.h"
#include "game_sv_single.h"
#include "object_factory.h"
#include "xrServer.h"
#include "UIGameCustom.h"
#include "ui/UIMainIngameWnd.h"
#include "ui/UIPdaWnd.h"
#include "HUDManager.h"
#include "MonsterEnemySharingDebug.h"
#include "ImUtils/AIGridEditor.h"
#include "../xrUI/UICursor.h"

extern int g_keypress_on_start;
bool g_MtLevelEarly = true;
bool StartLevelLoad(const shared_str& LevelName);

CGamePersistent::CGamePersistent(void)
{
	m_game_params.m_e_game_type	= eGameIDNoGame;
	m_pUI_core					= nullptr;
	m_pMainMenu					= nullptr;
	m_intro						= nullptr;
	
	if(!Device.IsEditorMode())
	m_intro_event.bind			(this, &CGamePersistent::start_logo_intro);
#ifdef DEBUG
	m_frame_counter				= 0;
	m_last_stats_frame			= u32(-2);
#endif
	// 
	//dSetAllocHandler			(ode_alloc		);
	//dSetReallocHandler			(ode_realloc	);
	//dSetFreeHandler				(ode_free		);

	// 
	bool bDemoMode	= Core.ParamsData.test(ECoreParams::demomode);
	if (bDemoMode)
	{
		string256	fname = {};
		const char*		name	=	strstr(Core.Params,"-demomode ") + 10;
		sscanf				(name,"%255s",fname);
		R_ASSERT2			(fname[0],"Missing filename for 'demomode'");
		Msg					("- playing in demo mode '%s'",fname);
		pDemoFile			=	FS.r_open	(fname);
		Device.seqFrame.Add	(this);
		eDemoStart			= g_pEventManager->Event.Handler_Attach("GAME:demo",this);
		uTime2Change		=	0;
	} else {
		pDemoFile			=	nullptr;
		eDemoStart			=	nullptr;
	}

	eQuickLoad				= g_pEventManager->Event.Handler_Attach("Game:QuickLoad",this);
	Fvector3* DofValue		= Console->GetFVectorPtr("r2_dof");
	if (DofValue)
	{
		SetBaseDof(*DofValue);
	}
	Dof.Initialize();

	g_Discord.SetStatus(g_pStringTable->translate(EngineExternal().GetTitle().c_str()).c_str());

	// Hozar 2002: For some reason 
	// we need it on scripts runing
	g_pGamePersistent = this;
	object_factory();

	luabind::functor<void> ixr_framework;
	if (ai().script_engine().functor("ixr_framework.init", ixr_framework))
	{
		ixr_framework();
	}
}

CGamePersistent::~CGamePersistent(void)
{	
	ShutdownAIGridEditor();
	FS.r_close					(pDemoFile);
	Device.seqFrame.Remove		(this);
	g_pEventManager->Event.Handler_Detach	(eDemoStart,this);
	g_pEventManager->Event.Handler_Detach	(eQuickLoad,this);
}

void CGamePersistent::PreStart(const char* op) {
	pApp->SetLoadingScreen(new UILoadingScreen());
	inherited::PreStart(op);
}

void CGamePersistent::RegisterModel(IRenderVisual* V)
{
	// Check types
	switch (V->getType()){
	case MT_SKELETON_ANIM:
	case MT_SKELETON_RIGID:{
		u16 def_idx		= GMLib.GetMaterialIdx("default_object");
		R_ASSERT2		(GMLib.GetMaterialByIdx(def_idx)->Flags.is(SGameMtl::flDynamic),"'default_object' - must be dynamic");
		IKinematics* K	= PKinematics(V); VERIFY(K);
		int cnt = K->LL_BoneCount();
		for (u16 k=0; k<cnt; k++){
			CBoneData& bd	= K->LL_GetData(k); 
			if (*(bd.game_mtl_name)){
				bd.game_mtl_idx	= GMLib.GetMaterialIdx(*bd.game_mtl_name);
				R_ASSERT2(GMLib.GetMaterialByIdx(bd.game_mtl_idx)->Flags.is(SGameMtl::flDynamic),"Required dynamic game material");
			}else{
				bd.game_mtl_idx	= def_idx;
			}
		}
	}break;
	}
}

extern void clean_game_globals	();
extern void init_game_globals	();

void CGamePersistent::OnAppStart()
{
	CRenderDevice::StartupFrameWaitTask([]() {
		GMLib.Load();
	}, 93, "Loading game materials");

	CRenderDevice::SetStartupStatus(95, "Configuring game globals");
	init_game_globals();
	CRenderDevice::PumpStartupFrames(4);

	CRenderDevice::SetStartupStatus(97, "Loading weather and environment");
	inherited::OnAppStart();
	CRenderDevice::PumpStartupFrames(4);

	CRenderDevice::SetStartupStatus(99, "Starting user interface");
	m_pUI_core = new ui_core();
	m_pMainMenu = new CMainMenu();
	if (ai().script_engine().functor("___ixr_engine_callbacks.on_frame", ixr_framework_onframe))
	{
		FrameworkScriptEngine = &ai().script_engine();
	}
	CRenderDevice::PumpStartupFrames(4);

	CRenderDevice::FinishStartup();
}


void CGamePersistent::OnAppEnd	()
{
	ShutdownAIGridEditor();
	DestroySpawnManagerWindow();

	if(m_pMainMenu->IsActive())
		m_pMainMenu->Activate(false);

	xr_delete					(m_pMainMenu);
	xr_delete					(m_pUI_core);

	inherited::OnAppEnd			();

	clean_game_globals			();

	GMLib.Unload				();

}

void CGamePersistent::Start		(const char* op)
{
	inherited::Start			(op);
}

void CGamePersistent::Disconnect()
{
	if (g_pGameLevel != nullptr)
		Particles::Details::Destroy(g_pGameLevel->AmbientWeather.Particles);

	inherited::Disconnect		();
	// stop all played emitters
	::Sound->stop_emitters		();
	m_game_params.m_e_game_type	= eGameIDNoGame;
}



void CGamePersistent::OnGameStart()
{
	UpdateGameType();
	if (g_pGameLevel == nullptr || !g_pGameLevel->IsParallel())
	{
		if (ai().script_engine().functor("___ixr_engine_callbacks.on_frame", ixr_framework_onframe))
			FrameworkScriptEngine = &ai().script_engine();
		shared_str levelName;
		if (g_MtLevelEarly && g_pGameLevel != nullptr && LevelStartInfo::Find(m_game_params, levelName))
			StartLevelLoad(levelName);
	}
	inherited::OnGameStart();
	UpdateGameType();
}

const char* GameTypeToString(EGameIDs gt, bool bShort)
{
	switch(gt)
	{
	case eGameIDSingle:
		return "single";
		break;
	case eGameIDDeathmatch:
		return (bShort)?"dm":"deathmatch";
		break;
	case eGameIDTeamDeathmatch:
		return (bShort)?"tdm":"teamdeathmatch";
		break;
	case eGameIDArtefactHunt:
		return (bShort)?"ah":"artefacthunt";
		break;
	case eGameIDCaptureTheArtefact:
		return (bShort)?"cta":"capturetheartefact";
		break;
	case eGameIDDominationZone:
		return (bShort)?"dz":"dominationzone";
		break;
	case eGameIDCapturePoints:
		return (bShort)?"cp":"capturepoints";
		break;
	case eGameIDFreeMP:
		return (bShort) ? "fmp" : "freemp";
	default :
		return		"---";
	}
}

EGameIDs ParseStringToGameType(const char* str)
{
	if (!xr_strcmp(str, "single")) 
		return eGameIDSingle;
	else if (!xr_strcmp(str, "deathmatch") || !xr_strcmp(str, "dm")) 
		return eGameIDDeathmatch;
	else if (!xr_strcmp(str, "teamdeathmatch") || !xr_strcmp(str, "tdm")) 
		return eGameIDTeamDeathmatch;
	else if (!xr_strcmp(str, "artefacthunt") || !xr_strcmp(str, "ah")) 
		return eGameIDArtefactHunt;
	else if (!xr_strcmp(str, "capturetheartefact") || !xr_strcmp(str, "cta")) 
		return eGameIDCaptureTheArtefact;
	else if (!xr_strcmp(str, "dominationzone")) 
		return eGameIDDominationZone;
	else if (!xr_strcmp(str, "capturepoints")) 
		return eGameIDCapturePoints;
	else if (!xr_strcmp(str, "freemp") || !xr_strcmp(str, "fmp"))
		return eGameIDFreeMP;
	else
		return eGameIDNoGame; //EGameIDs
}

void CGamePersistent::UpdateGameType()
{
	inherited::UpdateGameType();

	m_game_params.m_e_game_type = ParseStringToGameType(m_game_params.m_game_type);


	if (m_game_params.m_e_game_type == eGameIDSingle)
		g_current_keygroup = _sp;
	else
		g_current_keygroup = _mp;
}

void CGamePersistent::OnGameEnd	()
{
	CinemaFilmMaker_Shutdown();
	Dof.Reset();
	if ((g_pGameLevel == nullptr || !g_pGameLevel->IsParallel()) && FrameworkScriptEngine != nullptr)
	{
		ixr_framework_onframe.reset();
		FrameworkScriptEngine = nullptr;
	}
	inherited::OnGameEnd				();

	xr_delete							(g_stalker_velocity_holder);
}
bool no_amb_effects = false;
void CGamePersistent::WeathersUpdate()
{
	PROF_EVENT("CGamePersistent WeathersUpdate");
	if (g_pGameLevel == nullptr || g_dedicated_server || no_amb_effects || Environment().CurrentEnv == nullptr)
		return;

	auto& [Particles, SoundNextTimes, EffectNextTime, EffectStopTime, WindStart, WindInTime, WindEnd, WindOutTime, WindOn] = g_pGameLevel->AmbientWeather;
	const u32 nowMs = Level().SessionTimeMs();
	const float nowSeconds = nowMs * 0.001f;
	bool indoor = Render->InIndoor();
	if (!indoor)
	{
		CObject* currentView = Level().CurrentViewEntity();
		CActor* actor = currentView != nullptr ? currentView->cast_actor() : nullptr;
		if (actor != nullptr)
			indoor = actor->renderable_ROS()->get_luminocity_hemi() < 0.05f;
	}

	const size_t dataSet = Random.randF() < (1.f - Environment().CurrentEnv->weight) ? 0 : 1;
	CEnvDescriptor* descriptor = Environment().Current[dataSet];
	if (descriptor == nullptr)
		return;
	CEnvAmbient* ambient = descriptor->env_ambient;
	if (ambient != nullptr)
	{
		CEnvAmbient::SSndChannelVec& channels = ambient->get_snd_channels();
		for (size_t index = 0; index < channels.size() && index < SoundNextTimes.size(); ++index)
		{
			CEnvAmbient::SSndChannel& channel = *channels[index];
			if (SoundNextTimes[index] == 0)
			{
				SoundNextTimes[index] = nowMs + channel.get_rnd_sound_first_time();
			}
			else if (nowMs > SoundNextTimes[index])
			{
				ref_sound& sound = channel.get_rnd_sound();
				if (!sound._p)
					continue;
				Fvector position;
				const float angle = Random.randF(PI_MUL_2);
				position.x = std::cos(angle);
				position.y = 0.f;
				position.z = std::sin(angle);
				position.normalize().mul(channel.get_rnd_sound_dist()).add(Device.vCameraPosition);
				position.y += 10.f;
				sound._p->g_type = ESoundTypes::SOUND_TYPE_WORLD_AMBIENT;
				sound.play_at_pos(nullptr, position);
				if (!sound.handle() || Core.ParamsData.test(ECoreParams::nosound))
					continue;
				VERIFY(sound.handle());
				SoundNextTimes[index] = nowMs + iFloor(sound.get_length_sec() * 1000.f) + channel.get_rnd_sound_time();
			}
		}

		if (!indoor && Particles == nullptr && nowMs > EffectNextTime)
		{
			if (CEnvAmbient::SEffect* effect = ambient->get_rnd_effect())
			{
				Environment().wind_gust_factor = effect->wind_gust_factor;
				EffectNextTime = nowMs + ambient->get_rnd_effect_time();
				EffectStopTime = nowMs + effect->life_time;
				WindStart = nowSeconds;
				WindInTime = nowSeconds + effect->wind_blast_in_time;
				WindEnd = nowSeconds + effect->life_time / 1000.f;
				WindOutTime = WindEnd + effect->wind_blast_out_time;
				WindOn = true;

				Particles = Particles::Details::Create(effect->particles.c_str(), false, false);
				Fvector position;
				position.add(Device.vCameraPosition, effect->offset);
				if (Particles != nullptr)
					Particles->play_at_pos(position);
				if (effect->sound.handle())
					effect->sound.play_at_pos(nullptr, position);

				Environment().wind_blast_strength_start_value = Environment().wind_strength_factor;
				Environment().wind_blast_strength_stop_value = effect->wind_blast_strength;
				if (Environment().wind_blast_strength_start_value == 0.f)
					Environment().wind_blast_start_time.set(0.f, effect->wind_blast_direction.x, effect->wind_blast_direction.y, effect->wind_blast_direction.z);
				else
					Environment().wind_blast_start_time.set(0.f, Environment().wind_blast_direction.x, Environment().wind_blast_direction.y, Environment().wind_blast_direction.z);
				Environment().wind_blast_stop_time.set(0.f, effect->wind_blast_direction.x, effect->wind_blast_direction.y, effect->wind_blast_direction.z);
			}
		}
	}

	if (nowSeconds >= WindStart && nowSeconds <= WindInTime && WindOn)
	{
		const float delta = WindInTime - WindStart;
		const float t = delta != 0.f ? (nowSeconds - WindStart) / delta : 0.f;
		Environment().wind_blast_current.slerp(Environment().wind_blast_start_time, Environment().wind_blast_stop_time, t);
		Environment().wind_blast_direction.set(Environment().wind_blast_current.x, Environment().wind_blast_current.y, Environment().wind_blast_current.z);
		Environment().wind_strength_factor = Environment().wind_blast_strength_start_value + t * (Environment().wind_blast_strength_stop_value - Environment().wind_blast_strength_start_value);
	}
	if (indoor || nowMs >= EffectStopTime)
	{
		if (Particles != nullptr)
			Particles->Stop();
		Environment().wind_gust_factor = 0.f;
	}
	if (nowSeconds >= WindEnd && WindOn)
	{
		Environment().wind_blast_strength_start_value = Environment().wind_strength_factor;
		Environment().wind_blast_strength_stop_value = 0.f;
		WindOn = false;
	}
	if (nowSeconds >= WindEnd && nowSeconds <= WindOutTime)
	{
		const float delta = WindOutTime - WindEnd;
		const float t = delta != 0.f ? (nowSeconds - WindEnd) / delta : 0.f;
		Environment().wind_strength_factor = Environment().wind_blast_strength_start_value + t * (Environment().wind_blast_strength_stop_value - Environment().wind_blast_strength_start_value);
	}
	if (nowSeconds > WindOutTime && WindOutTime != 0.f)
		Environment().wind_strength_factor = 0.f;
	if (Particles != nullptr && !Particles->IsPlaying())
		Particles::Details::Destroy(Particles);
}

void CGamePersistent::UpdatePlayDestroyParticles()
{
	// Play req particle systems
	for (xr_shared_ptr<CParticlesObject>& Part : ps_needtoplay)
		Part->Play(false);
	ps_needtoplay.clear();


	if (!ps_active_deffer.empty())
	{
		ps_active.reserve(ps_active.size() + ps_active_deffer.size());

		for (xr_shared_ptr<CParticlesObject>& Part : ps_active_deffer)
		{
			ps_active.push_back(Part);
		}
		ps_active_deffer.clear();
	}

	ps_active.erase(std::remove_if
	(
		ps_active.begin(), ps_active.end(),
		[](const xr_shared_ptr<CParticlesObject>& Obj)->bool
		{
			return Obj->m_NeedDestroy;
		}
	), ps_active.end());
}

void CGamePersistent::UpdateParticles(const IGame_Level* owner)
{
	if (DevicePtr && !g_dedicated_server)
	{
		PROF_EVENT(__FUNCTION__);
		u32 dwTime = Device.dwTimeGlobal;

		CFrustum frustum;
		frustum.CreateFromMatrix(Device.mFullTransform_saved, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);

		extern u32 particles_workers_count;
		static u32 last_count = particles_workers_count;
		static xr_vector<std::pair<u32, xr_vector<xr_shared_ptr<CParticlesObject>>>> workers(last_count);

		if (particles_workers_count <= 1 || (owner != nullptr && owner->IsParallel()))
		{
			for (xr_shared_ptr<CParticlesObject> particle : ps_active)
			{
				if (particle->GetOwnerLevel() == owner && !particle->m_NeedDestroy && !particle->GetHudMode())
				{
					particle->Update(dwTime - particle->dwLastTime, &frustum);
					particle->dwLastTime = dwTime;
				}
			}
			return;
		}

		if (last_count != particles_workers_count)
		{
			last_count = particles_workers_count;
			workers.resize(last_count);
		}

		for (u32 i = 0; i < last_count; ++i)
		{
			workers[i].first = 0;
			workers[i].second.clear();
		}

		std::sort(ps_active.begin(), ps_active.end(),
			[owner](const xr_shared_ptr<CParticlesObject>& a, const xr_shared_ptr<CParticlesObject>& b)
			{
				const bool aOwned = a->GetOwnerLevel() == owner;
				const bool bOwned = b->GetOwnerLevel() == owner;
				return aOwned != bOwned ? aOwned : aOwned && a->GetSpriteCount() > b->GetSpriteCount();
			});

		for (auto& particle : ps_active)
		{
			u32 min_group = 0;
			for (u32 i = 0; i < last_count; ++i)
			{
				if (workers[i].first < workers[min_group].first)
					min_group = i;
			}
			if (particle->GetOwnerLevel() == owner && !particle->m_NeedDestroy && !particle->GetHudMode())
			{
				workers[min_group].second.push_back(particle);
				workers[min_group].first += particle->GetSpriteCount();
			}
		}

		static xr_task_group particle_tasks;

		for (u32 i = 0; i < last_count; ++i)
		{
			particle_tasks.run([i, dwTime, &frustum]()
			{
				string64 worker_name;
				u32 normalized = i;
				xr_sprintf(worker_name, "Particle worker #%u", ++normalized);

				PROF_THREAD(worker_name);

				std::sort(workers[i].second.begin(), workers[i].second.end(), [](xr_shared_ptr<CParticlesObject>& a, xr_shared_ptr<CParticlesObject>& b)
				{
					return Device.vCameraPosition_saved.distance_to_sqr(a->SpatialComponent->sphere.P) < Device.vCameraPosition_saved.distance_to_sqr(b->SpatialComponent->sphere.P);
				});

				for (xr_shared_ptr<CParticlesObject>& particle : workers[i].second)
				{
					if (particle->m_NeedDestroy) continue;
					particle->Update(dwTime - particle->dwLastTime, &frustum);
					particle->dwLastTime = dwTime;
				}
			});
		}

		particle_tasks.wait();
	}
}

void CGamePersistent::destroy_particles(bool all_particles)
{
	PROF_EVENT(__FUNCTION__);
	ps_needtoplay.clear();

	// delete active particles
	if (all_particles)
	{
		ps_active.clear();
	}
	else
	{
		ps_active.erase(std::remove_if
		(
			ps_active.begin(), ps_active.end(),
			[](const xr_shared_ptr<CParticlesObject>& Obj)->bool
			{
				return Obj->GetOwnerLevel() == g_pGameLevel && ((g_pGameLevel != nullptr && g_pGameLevel->IsParallel()) || Obj->m_destroy_on_game_load);
			}
		), ps_active.end());
	}

	VERIFY(ps_needtoplay.empty() && (!all_particles || ps_active.empty()));
}

IC bool AllowIntroLogo()
{
	return nullptr == strstr(Core.Params, "-nointro");
}

void CGamePersistent::start_logo_intro()
{
	if (!g_keypress_on_start || !AllowIntroLogo())
	{
		m_intro_event.clear();
		Console->Show();
		Console->Execute("main_menu on");
		return;
	}

	if(Device.dwPrecacheFrame==0)
	{
		m_intro_event.bind		(this, &CGamePersistent::update_logo_intro);
		if (!g_dedicated_server && 0==xr_strlen(m_game_params.m_game_or_spawn) && nullptr==g_pGameLevel)
		{
			VERIFY				(nullptr==m_intro);
			m_intro				= new CUISequencer();
			m_intro->Start		("intro_logo");
			Msg					("intro_start intro_logo");
			Console->Hide		();
		}
	}
}

void CGamePersistent::update_logo_intro()
{
	if(m_intro && (false==m_intro->IsActive()))
	{
		m_intro_event			= 0;
		xr_delete				(m_intro);
		Msg("intro_delete ::update_logo_intro");
		Console->Execute		("main_menu on");
	}else
	if(!m_intro)
	{
		m_intro_event			= 0;
	}
}

void CGamePersistent::game_loaded()
{
	if (Device.dwPrecacheFrame <= 2 && g_pGameLevel != nullptr)
	{
		IGame_Level::SLevelFlow& flow = g_pGameLevel->Flow;
		SetDiscordStatus();
		if (g_pGameLevel->bReady && g_keypress_on_start && load_screen_renderer.b_need_user_input && m_game_params.m_e_game_type == eGameIDSingle)
		{
			if (!g_pGameLevel->IsParallel())
				pApp->SetLoadStageTitle("");
			VERIFY(nullptr == flow.Intro);
			flow.Intro = new CUISequencer(g_pGameLevel);
			flow.Intro->Start("game_loaded");
			Msg("intro_start game_loaded");
			flow.Intro->m_on_destroy_event.bind(this, &CGamePersistent::update_game_loaded);
		}
		flow.IntroEvent = 0;
	}
}

void CGamePersistent::update_game_loaded()
{
	IGame_Level::SLevelFlow& flow = g_pGameLevel->Flow;
	if (!g_pGameLevel->IsParallel())
		load_screen_renderer.stop();

	luabind::functor<void> lua_update_game_loaded;
	if (ai().script_engine().functor("___ixr_engine_callbacks.load_screen_press_any_key", lua_update_game_loaded))
	{
		lua_update_game_loaded();
	}

	start_game_intro();
}

void CGamePersistent::start_game_intro()
{
	IGame_Level::SLevelFlow& flow = g_pGameLevel->Flow;
	if (!g_keypress_on_start)
	{
		flow.IntroEvent = 0;
		return;
	}

	if (g_pGameLevel && g_pGameLevel->bReady && Device.dwPrecacheFrame <= 2)
	{
		flow.IntroEvent.bind(this, &CGamePersistent::update_game_intro);
		if (0 == _stricmp(m_game_params.m_new_or_load, "new"))
		{
			if (flow.Intro != nullptr && flow.Intro->IsActive())
			{
				return;
			}
			if (flow.Intro != nullptr)
			{
				flow.Intro->Destroy();
			}
			VERIFY(nullptr == flow.Intro);
			flow.Intro = new CUISequencer(g_pGameLevel);
			flow.Intro->Start("intro_game");
			Msg("intro_start intro_game");
		}
	}
}

void CGamePersistent::update_game_intro()
{
	IGame_Level::SLevelFlow& flow = g_pGameLevel->Flow;
	flow.IntroEvent = nullptr;
	if (flow.Intro && !flow.Intro->IsActive())
	{
		xr_delete(flow.Intro);
		Msg("intro_delete ::update_game_intro");
	}
}

void CGamePersistent::OnLevelFlow(CLevel& level)
{
	if (level.IsParallel() && !level.bReady)
		return;
	if (level.Flow.GameLoadedPending && level.Flow.IntroEvent.empty() && (Device.dwPrecacheFrame == 5 || level.IsParallel()))
	{
		if (level.IsParallel())
			level.Flow.GameLoadedPending = false;
		level.Flow.IntroEvent.bind(this, &CGamePersistent::game_loaded);
	}

	if (!g_dedicated_server && !level.Flow.IntroEvent.empty())
		level.Flow.IntroEvent();
	if (level.IsParallel() && level.Flow.Intro != nullptr)
		level.Flow.Intro->OnFrame();
	if (level.Flow.Intro != nullptr && !level.Flow.Intro->IsActive())
		xr_delete(level.Flow.Intro);

	if (level.Flow.RetiringTutorial != nullptr)
	{
		level.Flow.RetiringTutorial->Destroy();
	}
	if (level.Flow.Tutorial != nullptr && !level.Flow.Tutorial->IsActive())
		xr_delete(level.Flow.Tutorial);

	if (!level.IsParallel() && !g_dedicated_server && Device.dwPrecacheFrame == 0 && level.Flow.Intro == nullptr && level.Flow.IntroEvent.empty())
		load_screen_renderer.stop();
}

void CGamePersistent::OnFrameMT()
{
	if (g_pGameLevel != nullptr)
	{
		static_cast<CLevel*>(g_pGameLevel)->OnFrameworkFrame();
		return;
	}
	if (FrameworkScriptEngine != nullptr && ixr_framework_onframe.is_valid())
		inherited::OnFrameMT();
}

void CGamePersistent::OnFrame()
{
	PROF_EVENT("CGamePersistent OnFrame");
	if (g_pGameLevel != nullptr)
	{
		if (!g_pGameLevel->IsParallel())
			OnLevelFlow(*static_cast<CLevel*>(g_pGameLevel));
	}
	else
	{
		if (!g_dedicated_server && !m_intro_event.empty())
			m_intro_event();
		if (!g_dedicated_server && Device.dwPrecacheFrame == 0 && m_intro == nullptr && m_intro_event.empty())
			load_screen_renderer.stop();
	}
	if (0 == Device.dwFrame % 200)
		CUITextureMaster::FreeCachedShaders();

#ifdef DEBUG
	++m_frame_counter;
#endif

	if (!m_pMainMenu->IsActive()) {
		m_pMainMenu->DestroyInternal(false);
	}

if (!g_pGameLevel)
	{
		if (Device.IsEditorMode())
		{
			__super::OnFrame();
		}
		return;
	}
	if(!g_pGameLevel->bReady)	return;

	if(Device.Paused())
	{
		if (Level().IsDemoPlay())
		{
			CObject* current_view_entity = Level().CurrentViewEntity();
			CSpectator* tmp_spectr = current_view_entity != nullptr ? current_view_entity->cast_spectator() : nullptr;
			if (tmp_spectr)
			{
				tmp_spectr->UpdateCL();	//updating spectator in pause (pause ability of demo play)
			}
		}
#ifndef MASTER_GOLD
		if (Level().CurrentViewEntity() && IsGameTypeSingle()) {
			if (!g_actor || (g_actor->ID() != Level().CurrentViewEntity()->ID()))
			{
				CObject* current_view_entity = Level().CurrentViewEntity();
				CCreature	*custom_monster = current_view_entity != nullptr ? current_view_entity->cast_creature() : nullptr;
				if (custom_monster) // can be spectator in multiplayer
					custom_monster->UpdateCamera();
			}
			else 
			{
				CCameraBase* C = nullptr;
				if (g_actor)
				{
					if(!Actor()->Holder())
						C = Actor()->cam_Active();
					else
						C = Actor()->Holder()->Camera();
					if (Level().Cameras().GetCamEffector(cefDemo))
						Level().Cameras().UpdateFromCamera(C);
					else
					{
						Actor()->Cameras().UpdateFromCamera(C);
						Actor()->Cameras().ApplyDevice(Device.fViewportNear);
					}
#ifdef DEBUG
					if(psActorFlags.test(AF_NO_CLIP))
					{
						Actor()->dbg_update_cl			= 0;
						Actor()->dbg_update_shedule		= 0;
						Device.dwTimeDelta				= 0;
						Device.fTimeDelta				= 0.01f;			
						Actor()->UpdateCL				();
						Actor()->shedule_Update			(0);
						Actor()->dbg_update_cl			= 0;
						Actor()->dbg_update_shedule		= 0;

						CSE_Abstract* e					= Level().Server->ID_to_entity(Actor()->ID());
						VERIFY							(e);
						CSE_ALifeCreatureActor*	s_actor = smart_cast<CSE_ALifeCreatureActor*>(e);
						VERIFY							(s_actor);
						for(auto ID : s_actor->children)
						{
							CObject* obj = Level().Objects.net_Find(ID);
							if(obj && Engine.Sheduler.Registered(obj))
							{
								obj->dbg_update_shedule = 0;
								obj->dbg_update_cl = 0;
								obj->shedule_Update	(0);
								obj->UpdateCL();
								obj->dbg_update_shedule = 0;
								obj->dbg_update_cl = 0;
							}
						}
					}
#endif // DEBUG
				}
			}
		}
#else // MASTER_GOLD
		if (g_actor && IsGameTypeSingle())
		{
			CCameraBase* C = nullptr;
			if(!Actor()->Holder())
				C = Actor()->cam_Active();
			else
				C = Actor()->Holder()->Camera();

			if (Level().Cameras().GetCamEffector(cefDemo))
				Level().Cameras().UpdateFromCamera(C);
			else
			{
				Actor()->Cameras().UpdateFromCamera(C);
				Actor()->Cameras().ApplyDevice(Device.fViewportNear);
			}

		}
#endif // MASTER_GOLD
	}

	inherited::OnFrame			();

	if (!Device.Paused())
	{
		// update weathers ambient
		WeathersUpdate();
	}

	if (0 != pDemoFile)
	{
		if (Device.dwTimeGlobal > uTime2Change)
		{
			// Change level + play demo
			if (pDemoFile->elapsed() < 3)	pDemoFile->seek(0);		// cycle

			// Read params
			string512			params;
			pDemoFile->r_string(params, sizeof(params));
			string256			o_server, o_client, o_demo;	u32 o_time;
			if (sscanf(params, "%255[^,],%255[^,],%255[^,],%u", o_server, o_client, o_demo, &o_time) == 4)
			{
				g_pEventManager->Event.Defer("KERNEL:disconnect");
				g_pEventManager->Event.Defer("KERNEL:start", size_t(xr_strdup(_Trim(o_server))), size_t(xr_strdup(_Trim(o_client))));
				g_pEventManager->Event.Defer("GAME:demo", size_t(xr_strdup(_Trim(o_demo))), u64(o_time));
			}
			else
				Msg("! Invalid demo playlist parameters");
			uTime2Change = 0xffffffff;	// Block changer until Event received
		}
	}

#if 0
	if ((m_last_stats_frame + 1) < m_frame_counter)
		profiler().clear		();
#endif
	
	Dof.Update(Device.Paused() ? 0.0f : Device.fTimeDelta);
}

void CGamePersistent::OnEvent(EVENT E, u64 P1, u64 P2)
{
	if (E == eQuickLoad)
	{
		PROF_EVENT("eQuickLoad");
		loading_save_timer.Start();
		loading_save_timer_started = true;
		Msg("* Game Loading Timer: Started from Save Reloading");

		if (Device.Paused())
			Device.Pause(false, true, true, "eQuickLoad");

		if (CurrentGameUI())
		{
			CurrentGameUI()->HideShownDialogs();
			CurrentGameUI()->UIMainIngameWnd->reset_ui();
			CurrentGameUI()->PdaMenu()->Reset();
		}

		if (Level().Flow.Tutorial)
			Level().Flow.Tutorial->Stop();

		if (Level().Flow.RetiringTutorial)
			Level().Flow.RetiringTutorial->Stop();

		LPSTR saved_name = (LPSTR)(P1);

		Level().remove_objects();
		game_sv_Single* game = Level().Server->game != nullptr ? Level().Server->game->cast_game_sv_single() : nullptr;
		R_ASSERT(game);
		game->restart_simulator(saved_name);
		xr_free(saved_name);
		return;
	}
	else if (E == eDemoStart)
	{
		string256 cmd;
		const char* demo = (const char*)(P1);
		xr_sprintf(cmd, "demo_play %s", demo);
		Console->Execute(cmd);
		xr_free(demo);
		uTime2Change = Device.TimerAsync() + u32(P2) * 1000;
	}
}

void CGamePersistent::Statistics	(CGameFont* F)
{
}

float CGamePersistent::MtlTransparent(u32 mtl_idx)
{
	return GMLib.GetMaterialByIdx((u16)mtl_idx)->fVisTransparencyFactor;
}
static bool bRestorePause	= false;
static bool bEntryFlag		= true;

void CGamePersistent::OnAppActivate		()
{
	bool bIsMP = (g_pGameLevel && Level().game && !IsGameTypeSingle());
	bIsMP		&= !Device.Paused();

	if( !bIsMP )
	{
		Device.Pause			(false, !bRestorePause, true, "CGP::OnAppActivate");
	}else
	{
		Device.Pause			(false, true, true, "CGP::OnAppActivate MP");
	}

	bEntryFlag = true;
}

void CGamePersistent::OnAppDeactivate	()
{
	if(!bEntryFlag) return;

	bool bIsMP = (g_pGameLevel && Level().game && !IsGameTypeSingle());

	bRestorePause = false;

	if ( !bIsMP )
	{
		bRestorePause			= Device.Paused();
		Device.Pause			(true, true, true, "CGP::OnAppDeactivate");
	}else
	{
		Device.Pause			(true, false, true, "CGP::OnAppDeactivate MP");
	}
	bEntryFlag = false;
}


bool CGamePersistent::OnRenderPPUI_query()
{
	// A parallel (viewer) session renders its own scene with the stock
	// pipeline: while its pass owns the frame (the level slot is lent to it),
	// the menu's postprocess flag must not suppress the scene combine.
	if (g_pGameLevel != nullptr && g_pGameLevel->IsParallel())
		return false;
	return MainMenu()->OnRenderPPUI_query();
	// enable PP or not
}

extern UI_API void draw_wnds_rects();
void CGamePersistent::OnRenderPPUI_main()
{
	if (g_pGameLevel != nullptr)
	{
		Level().BulletManager().Render();

		if (g_hud)
		{
			MonsterEnemySharingDebug().Draw(HUD().world_prims);
			HUD().draw_prims();
		}
#ifdef DEBUG_DRAW
		RenderAIGridEditorWorld();
#endif
	}

	// A parallel (viewer) session renders its own scene through this path; the
	// main menu's UI (backdrop, dialogs, version text) belongs to the primary
	// frame's menu render, not to the parallel scene - skip it while a parallel
	// level owns the frame, or the menu gets baked into the viewer's frame.
	const bool parallelOwnsFrame = (g_pGameLevel != nullptr && g_pGameLevel->IsParallel());
	if (!parallelOwnsFrame)
	{
		// always
		MainMenu()->OnRenderPPUI_main();
		draw_wnds_rects();
	}
}

void CGamePersistent::OnRenderPPUI_PP()
{
	MainMenu()->OnRenderPPUI_PP();
}


void CGamePersistent::ShowLoadTip(const shared_str& map_name, bool IsReseeded)
{
	const static bool disableLoadScreenTips = EngineExternal()[EEngineExternalRender::DisableLoadScreenTips];
	if (disableLoadScreenTips)
		return;
	if (IsReseeded)
	{
		luabind::functor<void> seed;
		if (ai().script_engine().functor("math.randomseed", seed))
			seed(int(CPU::QPC() & 0x3fffffff));
	}

	string512 buff;
	u8 tipNum;
	luabind::functor<u8> functor;
	const bool isSingle = !xr_strcmp(m_game_params.m_game_type, "single");
	if (isSingle)
	{
		R_ASSERT(ai().script_engine().functor("loadscreen.get_tip_number", functor));
		tipNum = functor(map_name.c_str());
	}
	else
	{
		R_ASSERT(ai().script_engine().functor("loadscreen.get_mp_tip_number", functor));
		tipNum = functor(map_name.c_str());
	}
	xr_sprintf(buff, "%s%d:", g_pStringTable->translate("ls_tip_number").c_str(), tipNum);
	shared_str tip = buff;
	xr_sprintf(buff, isSingle ? "ls_tip_%d" : "ls_mp_tip_%d", tipNum);
	pApp->LoadTitleInt(g_pStringTable->translate("ls_header").c_str(), tip.c_str(), g_pStringTable->translate(buff).c_str());
	IsLoadTipShown = true;
}

void CGamePersistent::LoadTitle(bool change_tip, shared_str map_name)
{
	if (g_pGameLevel != nullptr && g_pGameLevel->IsParallel())
		return;
	pApp->LoadStage();
	if (change_tip)
		ShowLoadTip(map_name, false);
}

void CGamePersistent::SetLoadStageTitle(const char* ls_title)
{
	if (Device.IsEditorMode()) // idk why, but SDK keeps crashing here for some reason, so I decided to just turn off load stages for SDK
		return;

	if (g_pGameLevel != nullptr && g_pGameLevel->IsParallel())
	{
		if (ls_title)
			Log(ls_title);
		return;
	}

	string256 buff;
	if (ls_title)
	{
		xr_sprintf(buff, "%s%s", g_pStringTable->translate(ls_title).c_str(), "...");
		pApp->SetLoadStageTitle(buff);
	}
	else
		pApp->SetLoadStageTitle("");
}

bool CGamePersistent::CanBePaused()
{
	return IsGameTypeSingle	() || (g_pGameLevel && Level().IsDemoPlay());
}
void CGamePersistent::SetPickableEffectorDOF(bool bSet)
{
	Dof.SetPickable(bSet);
}

void CGamePersistent::GetCurrentDof(Fvector3& dof)
{
	Dof.GetCurrent(dof);
}

void CGamePersistent::SetBaseDof(const Fvector3& dof)
{
	Dof.SetBase(dof);
}

void CGamePersistent::SetEffectorDOF(const Fvector& needed_dof)
{
	Dof.SetEffector(needed_dof);
}

void CGamePersistent::RestoreEffectorDOF()
{
	Dof.RestoreEffector();
}

void CGamePersistent::OnSectorChanged(int sector)
{
	if(CurrentGameUI())
		CurrentGameUI()->UIMainIngameWnd->OnSectorChanged(sector);
}

void CGamePersistent::OnAssetsChanged()
{
	IGame_Persistent::OnAssetsChanged	();
	g_pStringTable->rescan				();
}

void CGamePersistent::SetDiscordStatus() const {
	if (g_pGameLevel != nullptr)
	{	
		// Get level name
		xr_string levelName = g_pStringTable->translate("st_discord_level").c_str();

		levelName += '\t';
		levelName += g_pStringTable->translate(Level().name().c_str()).c_str();

		g_Discord.SetPhase(levelName);
	}
}

void CGamePersistent::GetTextureParams(shared_str tex, Frect& out_rect, shared_str& file_name) 
{
	ui_shader sh;
	CUITextureMaster::InitTexture(tex, "hud\\default", sh, out_rect);
	file_name = CUITextureMaster::GetTextureFileName(tex.c_str());
}

void CGamePersistent::ChangeCursorPosition(Fvector2 value) 
{
	if (!psDeviceFlags.test(rsFullscreen))
	{
		Fvector2 posCurrent;
		SDL_GetMouseState(&posCurrent.x, &posCurrent.y);
		posCurrent.add(value);
		SDL_WarpMouseGlobal(posCurrent.x, posCurrent.y);
	}
	UI().GetUICursor().UpdateCursorPosition(value.x, value.y);
}

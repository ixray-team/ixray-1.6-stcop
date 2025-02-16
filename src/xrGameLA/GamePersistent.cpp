#include "stdafx.h"
#include "pch_script.h"
#include "gamepersistent.h"
#include "../fmesh.h"
#include "../xr_ioconsole.h"
#include "../gamemtllib.h"
#include "../../Include/xrRender/Kinematics.h"
#include "MainMenu.h"
#include "UICursor.h"
#include "game_base_space.h"
#include "level.h"
#include "ParticlesObject.h"
#include "actor.h"
#include "game_base_space.h"
#include "player_hud.h"
#include "stalker_animation_data_storage.h"
#include "stalker_velocity_holder.h"
#include "alife_simulator.h"
#include "CustomTimersManager.h"
#include "HUDManager.h"
#include "ui/UIPdaWnd.h"

#include "ActorEffector.h"
#include "actor.h"
#include "script_engine.h"
#include "ui/UITextureMaster.h"
#include "../xrEngine/Application.h"
#include "ui/UILoadingScreen.h"

#ifndef MASTER_GOLD
#	include "custommonster.h"
#endif // MASTER_GOLD

#ifndef _EDITOR
#	include "ai_debug.h"
#endif // _EDITOR

#include "../x_ray.h"

#ifdef DEBUG_MEMORY_MANAGER
	static	void *	ode_alloc	(size_t size)								{ return Memory.mem_alloc(size,"ODE");			}
	static	void *	ode_realloc	(void *ptr, size_t oldsize, size_t newsize)	{ return Memory.mem_realloc(ptr,newsize,"ODE");	}
	static	void	ode_free	(void *ptr, size_t size)					{ return xr_free(ptr);							}
#else // DEBUG_MEMORY_MANAGER
	static	void *	ode_alloc	(size_t size)								{ return xr_malloc(size);			}
	static	void *	ode_realloc	(void *ptr, size_t oldsize, size_t newsize)	{ return xr_realloc(ptr,newsize);	}
	static	void	ode_free	(void *ptr, size_t size)					{ return xr_free(ptr);				}
#endif // DEBUG_MEMORY_MANAGER

static float diff_far	= 70.0f;
static float diff_near	= -70.0f;

bool m_bCamReady = false;

CGamePersistent::CGamePersistent(void)
{
	m_bPickableDOF				= false;
	m_game_params.m_e_game_type	= eGameIDNoGame;
	ambient_effect_next_time	= 0;
	ambient_effect_stop_time	= 0;
	ambient_particles			= 0;

	ambient_effect_wind_start = 0.f;
	ambient_effect_wind_in_time = 0.f;
	ambient_effect_wind_end = 0.f;
	ambient_effect_wind_out_time = 0.f;
	ambient_effect_wind_on = false;

	ZeroMemory(ambient_sound_next_time, sizeof(ambient_sound_next_time));

	m_pUI_core					= NULL;
	m_pMainMenu					= NULL;
	m_intro						= NULL;
	m_intro_event.bind			(this,&CGamePersistent::start_logo_intro);
#ifdef DEBUG
	m_frame_counter				= 0;
	m_last_stats_frame			= u32(-2);
#endif
	// 
	dSetAllocHandler			(ode_alloc		);
	dSetReallocHandler			(ode_realloc	);
	dSetFreeHandler				(ode_free		);

	// 
	BOOL	bDemoMode	= (0!=strstr(Core.Params,"-demomode "));
	if (bDemoMode)
	{
		string256	fname;
		LPCSTR		name	=	strstr(Core.Params,"-demomode ") + 10;
		sscanf				(name,"%s",fname);
		R_ASSERT2			(fname[0],"Missing filename for 'demomode'");
		Msg					("- playing in demo mode '%s'",fname);
		pDemoFile			=	FS.r_open	(fname);
		Device.seqFrame.Add	(this);
		eDemoStart			= g_pEventManager->Event.Handler_Attach("GAME:demo",this);
		uTime2Change		=	0;
	} else {
		pDemoFile			=	NULL;
		eDemoStart			=	NULL;
	}

	eQuickLoad					= g_pEventManager->Event.Handler_Attach("Game:QuickLoad",this);

	Fvector3* DofValue		= Console->GetFVectorPtr("r2_dof");
	SetBaseDof				(*DofValue);
}

CGamePersistent::~CGamePersistent(void)
{	
	FS.r_close					(pDemoFile);
	Device.seqFrame.Remove		(this);
	g_pEventManager->Event.Handler_Detach	(eDemoStart,this);
	g_pEventManager->Event.Handler_Detach	(eQuickLoad,this);
}

void CGamePersistent::PreStart(LPCSTR op) {
	pApp->SetLoadingScreen(new UILoadingScreen());
	__super::PreStart(op);
}

void CGamePersistent::RegisterModel(IRenderVisual* V)
{
	// Check types
	switch (V->getType()){
	case MT_SKELETON_ANIM:
	case MT_SKELETON_RIGID:{
		u16 def_idx		= GMLib.GetMaterialIdx("default_object");
		R_ASSERT2		(GMLib.GetMaterialByIdx(def_idx)->Flags.is(SGameMtl::flDynamic),"'default_object' - must be dynamic");
		IKinematics* K	= smart_cast<IKinematics*>(V); VERIFY(K);
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
	// load game materials
	GMLib.Load					();
	init_game_globals			();
	__super::OnAppStart			();
	m_pUI_core					= new ui_core();
	m_pMainMenu					= new CMainMenu();
}


void CGamePersistent::OnAppEnd	()
{
	if(m_pMainMenu->IsActive())
		m_pMainMenu->Activate(false);

	xr_delete					(m_pMainMenu);
	xr_delete					(m_pUI_core);

	__super::OnAppEnd			();

	clean_game_globals			();

	GMLib.Unload				();

}

void CGamePersistent::Start		(LPCSTR op)
{
	__super::Start				(op);
}

void CGamePersistent::Disconnect()
{
	// destroy ambient particles
	CParticlesObject::Destroy(ambient_particles);

	__super::Disconnect			();
	// stop all played emitters
	::Sound->stop_emitters		();
	m_game_params.m_e_game_type	= eGameIDNoGame;
}

#include "../xrEngine/xr_level_controller.h"

void CGamePersistent::OnGameStart()
{
	__super::OnGameStart		();
	
	UpdateGameType				();

	diff_far	= pSettings->r_float("zone_pick_dof", "far"); 
	diff_near	= pSettings->r_float("zone_pick_dof", "near"); 

}

void CGamePersistent::UpdateGameType			()
{
	__super::UpdateGameType		();
	//  [7/11/2005]
	if (!xr_strcmp(m_game_params.m_game_type, "single")) m_game_params.m_e_game_type = eGameIDSingle;
	else m_game_params.m_e_game_type = eGameIDNoGame;
	//  [7/11/2005]

	g_current_keygroup = _sp;
}

void CGamePersistent::OnGameEnd	()
{
	__super::OnGameEnd					();

	xr_delete							(g_stalker_animation_data_storage);
	xr_delete							(g_stalker_velocity_holder);
}

void CGamePersistent::WeathersUpdate()
{
    if (g_pGameLevel && !g_dedicated_server)
    {
        CActor* actor   = smart_cast<CActor*>(Level().CurrentViewEntity());
        BOOL    bIndoor = TRUE;
        if (actor)
            bIndoor = actor->renderable_ROS()->get_luminocity_hemi() < 0.05f;

        int                   data_set    = (Random.randF() < (1.f - Environment().CurrentEnv->weight)) ? 0 : 1;

        CEnvDescriptor* const current_env = Environment().Current[0];
        VERIFY(current_env);

        CEnvDescriptor* const _env = Environment().Current[data_set];
        VERIFY(_env);

        CEnvAmbient* env_amb = _env->env_ambient;
        if (env_amb)
        {
            CEnvAmbient::SSndChannelVec&  vec = current_env->env_ambient->get_snd_channels();
            CEnvAmbient::SSndChannelVecIt I   = vec.begin();
            CEnvAmbient::SSndChannelVecIt E   = vec.end();

            // start sound
            for (u32 idx = 0; I != E; ++I, ++idx)
            {
                CEnvAmbient::SSndChannel& ch = **I;
                R_ASSERT(idx < 20);
                if (ambient_sound_next_time[idx] == 0)   // first
                {
                    ambient_sound_next_time[idx] = Device.dwTimeGlobal + ch.get_rnd_sound_first_time();
                }
                else if (Device.dwTimeGlobal > ambient_sound_next_time[idx])
                {
                    ref_sound& snd = ch.get_rnd_sound();

                    Fvector    pos;
                    float      angle = ::Random.randF(PI_MUL_2);
                    pos.x            = _cos(angle);
                    pos.y            = 0;
                    pos.z            = _sin(angle);
                    pos.normalize().mul(ch.get_rnd_sound_dist()).add(Device.vCameraPosition);
                    pos.y += 10.f;
                    snd.play_at_pos(0, pos);

#ifdef DEBUG
                    if (!snd._handle() && strstr(Core.Params, "-nosound"))
                        continue;
#endif   // DEBUG

                    VERIFY(snd._handle());
                    u32 _length_ms               = iFloor(snd.get_length_sec() * 1000.0f);
                    ambient_sound_next_time[idx] = Device.dwTimeGlobal + _length_ms + ch.get_rnd_sound_time();
                    // Msg("- Playing ambient sound channel [%s] file[%s]", ch.m_load_section.c_str(), snd._handle()->file_name());
                }
            }
            /*
            if (Device->dwTimeGlobal > ambient_sound_next_time)
            {
                ref_sound* snd          = env_amb->get_rnd_sound();
                ambient_sound_next_time = Device->dwTimeGlobal + env_amb->get_rnd_sound_time();
                if (snd)
                {
                    Fvector pos;
                    float   angle = ::Random.randF(PI_MUL_2);
                    pos.x         = _cos(angle);
                    pos.y         = 0;
                    pos.z         = _sin(angle);
                    pos.normalize().mul(env_amb->get_rnd_sound_dist()).add(Device->vCameraPosition);
                    pos.y += 10.f;
                    snd->play_at_pos(0, pos);
                }
            }
            */
            // start effect
            if ((FALSE == bIndoor) && (0 == ambient_particles) && Device.dwTimeGlobal > ambient_effect_next_time)
            {
                CEnvAmbient::SEffect* eff = env_amb->get_rnd_effect();
                if (eff)
                {
                    Environment().wind_gust_factor = eff->wind_gust_factor;
                    ambient_effect_next_time = Device.dwTimeGlobal + env_amb->get_rnd_effect_time();
                    ambient_effect_stop_time = Device.dwTimeGlobal + eff->life_time;
                    ambient_effect_wind_start = Device.fTimeGlobal;
                    ambient_effect_wind_in_time = Device.fTimeGlobal + eff->wind_blast_in_time;
                    ambient_effect_wind_end = Device.fTimeGlobal + eff->life_time / 1000.f;
                    ambient_effect_wind_out_time = Device.fTimeGlobal + eff->life_time / 1000.f + eff->wind_blast_out_time;
                    ambient_effect_wind_on = true;

                    ambient_particles = CParticlesObject::Create(eff->particles.c_str(), FALSE, false);
                    Fvector pos;
                    pos.add(Device.vCameraPosition, eff->offset);
                    ambient_particles->play_at_pos(pos);
                    if (eff->sound._handle())
                        eff->sound.play_at_pos(0, pos);

                    Environment().wind_blast_strength_start_value = Environment().wind_strength_factor;
                    Environment().wind_blast_strength_stop_value = eff->wind_blast_strength;

                    if (Environment().wind_blast_strength_start_value == 0.f)
                    {
                        Environment().wind_blast_start_time.set(0.f, eff->wind_blast_direction.x, eff->wind_blast_direction.y, eff->wind_blast_direction.z);
                    }
                    else
                    {
                        Environment().wind_blast_start_time.set(0.f, Environment().wind_blast_direction.x, Environment().wind_blast_direction.y, Environment().wind_blast_direction.z);
                    }
                    Environment().wind_blast_stop_time.set(0.f, eff->wind_blast_direction.x, eff->wind_blast_direction.y, eff->wind_blast_direction.z);
                }
            }
        }
        if (Device.fTimeGlobal >= ambient_effect_wind_start && Device.fTimeGlobal <= ambient_effect_wind_in_time && ambient_effect_wind_on)
        {
            float delta = ambient_effect_wind_in_time - ambient_effect_wind_start;
            float t;
            if (delta != 0.f)
            {
                float cur_in = Device.fTimeGlobal - ambient_effect_wind_start;
                t = cur_in / delta;
            }
            else
            {
                t = 0.f;
            }
            Environment().wind_blast_current.slerp(Environment().wind_blast_start_time, Environment().wind_blast_stop_time, t);

            Environment().wind_blast_direction.set(Environment().wind_blast_current.x, Environment().wind_blast_current.y, Environment().wind_blast_current.z);
            Environment().wind_strength_factor = Environment().wind_blast_strength_start_value + t * (Environment().wind_blast_strength_stop_value - Environment().wind_blast_strength_start_value);
        }

        // stop if time exceed or indoor
        if (bIndoor || Device.dwTimeGlobal >= ambient_effect_stop_time)
        {
            if (ambient_particles)
                ambient_particles->Stop();

            Environment().wind_gust_factor = 0.f;
        }

        if (Device.fTimeGlobal >= ambient_effect_wind_end && ambient_effect_wind_on)
        {
            Environment().wind_blast_strength_start_value = Environment().wind_strength_factor;
            Environment().wind_blast_strength_stop_value = 0.f;

            ambient_effect_wind_on = false;
        }

        if (Device.fTimeGlobal >= ambient_effect_wind_end && Device.fTimeGlobal <= ambient_effect_wind_out_time)
        {
            float delta = ambient_effect_wind_out_time - ambient_effect_wind_end;
            float t;
            if (delta != 0.f)
            {
                float cur_in = Device.fTimeGlobal - ambient_effect_wind_end;
                t = cur_in / delta;
            }
            else
            {
                t = 0.f;
            }
            Environment().wind_strength_factor = Environment().wind_blast_strength_start_value + t * (Environment().wind_blast_strength_stop_value - Environment().wind_blast_strength_start_value);
        }
        if (Device.fTimeGlobal > ambient_effect_wind_out_time && ambient_effect_wind_out_time != 0.f)
        {
            Environment().wind_strength_factor = 0.0;
        }

        // if particles not playing - destroy
        if (ambient_particles && !ambient_particles->IsPlaying())
            CParticlesObject::Destroy(ambient_particles);
    }
}

bool allow_intro ()
{
	return 0 == strstr(Core.Params,"-nointro");
}

#include "UI/UIGameTutorial.h"

extern int			ps_intro;

void CGamePersistent::start_logo_intro		()
{
	if (!ps_intro){
		m_intro_event			= 0;
		if (!xr_strlen(m_game_params.m_new_or_load))
		{
			Console->Show			();
			Console->Execute		("main_menu on");
		}
		return;
	}

	if (Device.dwPrecacheFrame==0)
	{
		m_intro_event.bind		(this,&CGamePersistent::update_logo_intro);
		if (!g_dedicated_server && 0==xr_strlen(m_game_params.m_game_or_spawn) && NULL==g_pGameLevel)
		{
			if (NULL!=m_intro)	return;
			m_intro				= new CUISequencer();
			m_intro->Start		("intro_logo");
			Console->Hide		();
		}
	}
}
void CGamePersistent::update_logo_intro			()
{
	if(m_intro && (false==m_intro->IsActive())){
		m_intro_event			= 0;
		xr_delete				(m_intro);
		Console->Execute		("main_menu on");
	}
	else if (!m_intro)
	{
		m_intro_event			= 0;
	}
}

void CGamePersistent::game_loaded()
{
	if(Device.dwPrecacheFrame<=2)
	{
		if(	g_pGameLevel							&&
			g_pGameLevel->bReady					&&
			load_screen_renderer.b_need_user_input	&& 
			m_game_params.m_e_game_type == GAME_SINGLE)
		{
	//		pApp->ClearTitle();

			if (NULL!=m_intro)	return;

			m_intro				= new CUISequencer();
			m_intro->Start		("game_loaded");
			m_intro->m_on_destroy_event.bind(this, &CGamePersistent::update_game_loaded);
		}
		m_intro_event			= 0;
	}
}

void CGamePersistent::update_game_loaded()
{
	xr_delete				(m_intro);
	start_game_intro		();
#ifdef USE_TIMERS_MANAGER
	ai().alife().timers().GameLoaded(true);
#endif
}
#include "ai_space.h"
#include "alife_spawn_registry.h"
void CGamePersistent::start_game_intro		()
{
	if (g_pGameLevel && g_pGameLevel->bReady && Device.dwPrecacheFrame<=2){
		m_intro_event.bind		(this,&CGamePersistent::update_game_intro);

		LPCSTR spawn_name = ai().alife().spawns().GetSpawnName();
		bool load_spawn = (0==stricmp(m_game_params.m_new_or_load,"load") && 0==xr_strcmp(m_game_params.m_game_or_spawn, spawn_name));	//skyloader: flag if load save and (save == spawn_name), for example, all.sav
		if (0==stricmp(m_game_params.m_new_or_load,"new") || load_spawn)
		{
			if (NULL!=m_intro)	return;
			m_intro				= new CUISequencer();
			m_intro->Start		("intro_game");

			if (ps_intro && !load_spawn)
				Msg("intro_start intro_game");
			else
				m_intro->Stop(); //<= skyloader: call functions from sequencer (call the first scene in sid bunker)
		}
			
	}
}
#include "script_engine.h"
void synchronization_callback()
{
	string256					fn;
	luabind::functor<void>		callback;
	xr_strcpy					(fn, pSettings->r_string("lost_alpha_cfg", "on_synchronization_done"));
	R_ASSERT					(ai().script_engine().functor<void>(fn, callback));
	callback					();
}

void CGamePersistent::update_game_intro			()
{
	if(m_intro && (false==m_intro->IsActive())){
		xr_delete				(m_intro);
		m_intro_event			= 0;
		synchronization_callback();
	}
	else if(!m_intro)
	{
		m_intro_event			= 0;
		synchronization_callback();
	}
}
#include "holder_custom.h"
extern CUISequencer * g_tutorial;
extern CUISequencer * g_tutorial2;

void CGamePersistent::OnFrame	()
{
	if(Device.dwPrecacheFrame==5 && m_intro_event.empty())
	{
		m_intro_event.bind			(this,&CGamePersistent::game_loaded);
	}

	if(g_tutorial2)
	{ 
		g_tutorial2->Destroy	();
		xr_delete				(g_tutorial2);
	}

	if(g_tutorial && !g_tutorial->IsActive())
	{
		xr_delete(g_tutorial);
	}
	if(0==Device.dwFrame%200)
		CUITextureMaster::FreeCachedShaders();

#ifdef DEBUG
	++m_frame_counter;
#endif
	if (!g_dedicated_server && !m_intro_event.empty())	
		m_intro_event();
	if(!g_dedicated_server && Device.dwPrecacheFrame==0 && !m_intro && m_intro_event.empty())
		load_screen_renderer.stop();

	if( !m_pMainMenu->IsActive() )
		m_pMainMenu->DestroyInternal(false);

	if(!g_pGameLevel)			return;
	if(!g_pGameLevel->bReady)	return;

	if(Device.Paused()){
#ifndef MASTER_GOLD
		if (Level().CurrentViewEntity()) {
			if (!g_actor || (g_actor->ID() != Level().CurrentViewEntity()->ID())) {
				CCustomMonster	*custom_monster = smart_cast<CCustomMonster*>(Level().CurrentViewEntity());
				if (custom_monster) // can be spectator in multiplayer
					custom_monster->UpdateCamera();
			}
			else 
			{
				CCameraBase* C = NULL;
				if (g_actor)
				{
					if(!Actor()->Holder())
						C = Actor()->cam_Active();
					else
						C = Actor()->Holder()->Camera();

				Actor()->Cameras().UpdateFromCamera(C);
				Actor()->Cameras().ApplyDevice(VIEWPORT_NEAR);
				}
			}
		}
#else // MASTER_GOLD
		if (g_actor)
		{
			CCameraBase* C = NULL;
			if(!Actor()->Holder())
				C = Actor()->cam_Active();
			else
				C = Actor()->Holder()->Camera();

			Actor()->Cameras().UpdateFromCamera		(C);
			Actor()->Cameras().ApplyDevice	(VIEWPORT_NEAR);
		}
#endif // MASTER_GOLD
	}
	__super::OnFrame			();

	if(!Device.Paused())
		Engine.Sheduler.Update		();

	// update weathers ambient
	if(!Device.Paused())
		WeathersUpdate				();

	if (m_bCamReady == false) {
		m_bCamReady = true;
	}

	if	(0!=pDemoFile)
	{
		if	(Device.dwTimeGlobal>uTime2Change){
			// Change level + play demo
			if			(pDemoFile->elapsed()<3)	pDemoFile->seek(0);		// cycle

			// Read params
			string512			params;
			pDemoFile->r_string	(params,sizeof(params));
			string256			o_server, o_client, o_demo;	u32 o_time;
			sscanf				(params,"%[^,],%[^,],%[^,],%d",o_server,o_client,o_demo,&o_time);

			// Start _new level + demo
			g_pEventManager->Event.Defer	("KERNEL:disconnect");
			g_pEventManager->Event.Defer	("KERNEL:start",size_t(xr_strdup(_Trim(o_server))),size_t(xr_strdup(_Trim(o_client))));
			g_pEventManager->Event.Defer	("GAME:demo",	size_t(xr_strdup(_Trim(o_demo))), u64(o_time));
			uTime2Change		= 0xffffffff;	// Block changer until Event received
		}
	}

#ifdef DEBUG
	if ((m_last_stats_frame + 1) < m_frame_counter)
		profiler().clear		();
#endif

	UpdateDof();
}

#include "game_sv_single.h"
#include "xrServer.h"
#include "UIGameCustom.h"
#include "ui/UIMainIngameWnd.h"
#include "ui/UIPdaWnd.h"

void CGamePersistent::OnEvent(EVENT E, u64 P1, u64 P2)
{
	if(E==eQuickLoad)
	{
		loading_save_timer.Start();
		pApp->LoadBegin();
		if (Device.Paused())
			Device.Pause		(FALSE, TRUE, TRUE, "eQuickLoad");

		CUIGameCustom* ui_game_custom = NULL;
		if ((ui_game_custom = CurrentGameUI()) != NULL)
		{
			ui_game_custom->HideShownDialogs();
			ui_game_custom->UIMainIngameWnd->reset_ui();

			//CurrentGameUI()->PdaMenu().Reset();
			// resetting pda is not enough...

			xr_delete(ui_game_custom->m_PdaMenu);
			ui_game_custom->m_PdaMenu = new CUIPdaWnd();
		}

		if(g_tutorial)
			g_tutorial->Stop();

		if(g_tutorial2)
			g_tutorial2->Stop();

		
		LPSTR		saved_name	= (LPSTR)(P1);

		Level().remove_objects	();
		game_sv_Single			*game = smart_cast<game_sv_Single*>(Level().Server->game);
		R_ASSERT				(game);
		game->restart_simulator	(saved_name);
		xr_free					(saved_name);
		pApp->LoadEnd();
		return;
	}else
	if(E==eDemoStart)
	{
		string256			cmd;
		LPCSTR				demo	= LPCSTR(P1);
		xr_sprintf				(cmd,"demo_play %s",demo);
		Console->Execute	(cmd);
		xr_free				(demo);
		uTime2Change		= Device.TimerAsync() + u32(P2)*1000;
	}
}

void CGamePersistent::Statistics	(CGameFont* F)
{
#ifdef DEBUG
#	ifndef _EDITOR
		m_last_stats_frame		= m_frame_counter;
		profiler().show_stats	(F,!!psAI_Flags.test(aiStats));
#	endif
#endif
}

float CGamePersistent::MtlTransparent(u32 mtl_idx)
{
	return GMLib.GetMaterialByIdx((u16)mtl_idx)->fVisTransparencyFactor;
}
static BOOL bRestorePause	= FALSE;
static BOOL bEntryFlag		= TRUE;

void CGamePersistent::OnAppActivate		()
{
	bool bIsMP = (g_pGameLevel && Level().game && GameID() != GAME_SINGLE);
	bIsMP		&= !Device.Paused();

	if( !bIsMP )
	{
		Device.Pause			(FALSE, !bRestorePause, TRUE, "CGP::OnAppActivate");
	}else
	{
		Device.Pause			(FALSE, TRUE, TRUE, "CGP::OnAppActivate MP");
	}

	bEntryFlag = TRUE;
}

void CGamePersistent::OnAppDeactivate	()
{
	if(!bEntryFlag) return;

	bool bIsMP = (g_pGameLevel && Level().game && GameID() != GAME_SINGLE);

	bRestorePause = FALSE;

	if ( !bIsMP )
	{
		bRestorePause			= Device.Paused();
		Device.Pause			(TRUE, TRUE, TRUE, "CGP::OnAppDeactivate");
	}else
	{
		Device.Pause			(TRUE, FALSE, TRUE, "CGP::OnAppDeactivate MP");
	}
	bEntryFlag = FALSE;
}

bool CGamePersistent::OnRenderPPUI_query()
{
	return MainMenu()->OnRenderPPUI_query();
	// enable PP or not
}

void CGamePersistent::OnRenderPPUI_main()
{
	// always
	MainMenu()->OnRenderPPUI_main();
}

void CGamePersistent::OnRenderPPUI_PP()
{
	MainMenu()->OnRenderPPUI_PP();
}
#include "../xrEngine/string_table.h"

void CGamePersistent::LoadTitle(LPCSTR str)
{
	string512			buff;
	xr_sprintf				(buff, "%s...", CStringTable().translate(str).c_str());
//	pApp->LoadTitleInt	(buff);
}

bool CGamePersistent::CanBePaused()
{
	return IsGameTypeSingle	();
}

void CGamePersistent::SetPickableEffectorDOF(bool bSet)
{
	m_bPickableDOF = bSet;
	if(!bSet)
		RestoreEffectorDOF();
}

void CGamePersistent::GetCurrentDof(Fvector3& dof)
{
	dof = m_dof[1];
}

void CGamePersistent::SetBaseDof(const Fvector3& dof)
{
	if (!m_bPickableDOF)
		m_dof[0] = m_dof[1] = m_dof[2] = m_dof[3] = dof;
	else
		m_dof[3] = dof;
}

void CGamePersistent::SetEffectorDOF(const Fvector& needed_dof)
{
	if(m_bPickableDOF)	return;
	m_dof[0]	= needed_dof;
	m_dof[2]	= m_dof[1]; //current
}

void CGamePersistent::RestoreEffectorDOF()
{
	SetEffectorDOF			(m_dof[3]);
}

#include "UIGameSP.h"

void CGamePersistent::UpdateDof()
{
	if(m_bPickableDOF)
	{
		Fvector pick_dof;
		pick_dof.y	= HUD().GetCurrentRayQuery().range;
		pick_dof.x	= pick_dof.y+diff_near;
		pick_dof.z	= pick_dof.y+diff_far;
		m_dof[0]	= pick_dof;
		m_dof[2]	= m_dof[1]; //current
	}
	if(m_dof[1].similar(m_dof[0]))
						return;

	float td			= Device.fTimeDelta;
	Fvector				diff;
	diff.sub			(m_dof[0], m_dof[2]);
	diff.mul			(td/0.2f); //0.2 sec
	m_dof[1].add		(diff);
	(m_dof[0].x<m_dof[2].x)?clamp(m_dof[1].x,m_dof[0].x,m_dof[2].x):clamp(m_dof[1].x,m_dof[2].x,m_dof[0].x);
	(m_dof[0].y<m_dof[2].y)?clamp(m_dof[1].y,m_dof[0].y,m_dof[2].y):clamp(m_dof[1].y,m_dof[2].y,m_dof[0].y);
	(m_dof[0].z<m_dof[2].z)?clamp(m_dof[1].z,m_dof[0].z,m_dof[2].z):clamp(m_dof[1].z,m_dof[2].z,m_dof[0].z);
}

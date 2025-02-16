#include "stdafx.h"
#include "pch_script.h"
#include "../fdemorecord.h"
#include "../fdemoplay.h"
#include "../environment.h"
#include "../igame_persistent.h"
#include "../xrParticles/stdafx.h"
#include "../xrParticles/ParticlesObject.h"
#include "Level.h"
#include "xrServer.h"
#include "net_queue.h"
#include "game_cl_base.h"
#include "entity_alive.h"
#include "hudmanager.h"
#include "ai_space.h"
#include "ai_debug.h"
#include "PHdynamicdata.h"
#include "Physics.h"
#include "ShootingObject.h"
#include "player_hud.h"
#include "Level_Bullet_Manager.h"
#include "../xrScripts/script_process.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_engine_space.h"
#include "team_base_zone.h"
#include "infoportion.h"
#include "patrol_path_storage.h"
#include "date_time.h"
#include "space_restriction_manager.h"
#include "seniority_hierarchy_holder.h"
#include "space_restrictor.h"
#include "client_spawn_manager.h"
#include "autosave_manager.h"
#include "ClimableObject.h"
#include "level_graph.h"
#include "mt_config.h"
#include "phcommander.h"
#include "fast_entity_update.h"
#include "map_manager.h"
#include "../CameraManager.h"
#include "level_sounds.h"
#include "car.h"
#include "trade_parameters.h"
#include "game_cl_base_weapon_usage_statistic.h"
#include "clsid_game.h"
#include "MainMenu.h"
#include "..\XR_IOConsole.h"
#include "actor.h"
#include "alife_simulator.h"
#include "CustomTimersManager.h"

#ifdef DEBUG
#	include "level_debug.h"
#	include "ai/stalker/ai_stalker.h"
#	include "physicobject.h"
#endif

#include "debug_renderer.h"
#include "ai/stalker/ai_stalker.h"
#include <functional>

ENGINE_API bool g_dedicated_server;

extern BOOL	g_bDebugDumpPhysicsStep;

int		psLUA_GCSTEP			= 10;
CPHWorld	*ph_world			= 0;
float		g_cl_lvInterp		= 0;
u32			lvInterpSteps		= 0;
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLevel::CLevel():IPureClient	(Device.GetTimerGlobal())
#ifdef PROFILE_CRITICAL_SECTIONS
	,DemoCS(MUTEX_PROFILE_ID(DemoCS))
#endif // PROFILE_CRITICAL_SECTIONS
{
	g_bDebugEvents				= strstr(Core.Params,"-debug_ge")?TRUE:FALSE;

	Server						= nullptr;

	game						= nullptr;
	game_events					= new NET_Queue_Event();

	game_configured				= FALSE;
	m_bGameConfigStarted		= FALSE;

	eChangeRP					= g_pEventManager->Event.Handler_Attach	("LEVEL:ChangeRP",this);
	eDemoPlay					= g_pEventManager->Event.Handler_Attach	("LEVEL:PlayDEMO",this);
	eChangeTrack				= g_pEventManager->Event.Handler_Attach	("LEVEL:PlayMusic",this);
	eEnvironment				= g_pEventManager->Event.Handler_Attach	("LEVEL:Environment",this);

	eEntitySpawn				= g_pEventManager->Event.Handler_Attach	("LEVEL:spawn",this);

	m_pBulletManager			= new CBulletManager();

	if(!g_dedicated_server)
		m_map_manager				= new CMapManager();
	else
		m_map_manager				= nullptr;

//	m_pFogOfWarMngr				= new CFogOfWarMngr();
//----------------------------------------------------
	m_bNeed_CrPr				= false;
	m_bIn_CrPr					= false;
	m_dwNumSteps				= 0;
	m_dwDeltaUpdate				= u32(fixed_step*1000);
	m_dwLastNetUpdateTime		= 0;

	physics_step_time_callback	= (PhysicsStepTimeCallback*) &PhisStepsCallback;
	m_seniority_hierarchy_holder= new CSeniorityHierarchyHolder();

	if(!g_dedicated_server)
	{
		m_level_sound_manager		= new CLevelSoundManager();
		m_space_restriction_manager = new CSpaceRestrictionManager();
		m_client_spawn_manager		= new CClientSpawnManager();
		m_autosave_manager			= new CAutosaveManager();
	#ifdef DEBUG_DRAW
		m_debug_renderer			= new CDebugRenderer();
	#endif
	#ifdef DEBUG
		m_level_debug				= new CLevelDebug();
	#endif
	
	}else
	{
		m_level_sound_manager		= nullptr;
		m_client_spawn_manager		= nullptr;
		m_autosave_manager			= nullptr;
		m_space_restriction_manager = nullptr;
	#ifdef DEBUG_DRAW
		m_debug_renderer			= nullptr;
	#endif
	#ifdef DEBUG
		m_level_debug				= nullptr;
	#endif
		
	}


	
	m_ph_commander				= new CPHCommander();
	m_ph_commander_scripts		= new CPHCommander();

	m_fast_updater				= new CFastEntityUpdater();

#ifdef DEBUG
	m_bSynchronization			= false;
#endif	
	//---------------------------------------------------------
	pStatGraphR = nullptr;
	pStatGraphS = nullptr;
	//---------------------------------------------------------
	pObjects4CrPr.clear();
	pActors4CrPr.clear();
	//---------------------------------------------------------
	g_player_hud = new player_hud();
	g_player_hud->load_default();
	//---------------------------------------------------------
	pCurrentControlEntity = nullptr;

	//---------------------------------------------------------
	m_dwCL_PingLastSendTime = 0;
	m_dwCL_PingDeltaSend = 1000;
	m_dwRealPing = 0;

	//---------------------------------------------------------	
	m_sDemoName[0] = 0;
	m_bDemoSaveMode = FALSE;
	m_dwStoredDemoDataSize = 0;
	m_pStoredDemoData = nullptr;
	m_pOldCrashHandler = nullptr;
	m_we_used_old_crach_handler	= false;

	Msg("%s", Core.Params);
}

extern CAI_Space *g_ai_space;

CLevel::~CLevel()
{
	xr_delete			(g_player_hud);

	Msg							("- Destroying level");

	g_pEventManager->Event.Handler_Detach	(eEntitySpawn,	this);

	g_pEventManager->Event.Handler_Detach	(eEnvironment,	this);
	g_pEventManager->Event.Handler_Detach	(eChangeTrack,	this);
	g_pEventManager->Event.Handler_Detach	(eDemoPlay,		this);
	g_pEventManager->Event.Handler_Detach	(eChangeRP,		this);

	if (ph_world)
	{
		ph_world->Destroy		();
		xr_delete				(ph_world);
	}

	// destroy PSs
	for (POIt p_it=m_StaticParticles.begin(); m_StaticParticles.end()!=p_it; ++p_it)
		Particles::Details::Destroy(*p_it);
	m_StaticParticles.clear		();

	// Unload sounds
	// unload prefetched sounds
	sound_registry.clear		();

	// unload static sounds
	for (u32 i=0; i<static_Sounds.size(); ++i){
		static_Sounds[i]->destroy();
		xr_delete				(static_Sounds[i]);
	}
	static_Sounds.clear			();

	xr_delete					(m_level_sound_manager);

	xr_delete					(m_space_restriction_manager);

	xr_delete					(m_seniority_hierarchy_holder);
	
	xr_delete					(m_client_spawn_manager);

	xr_delete					(m_autosave_manager);

#ifdef DEBUG_DRAW
	xr_delete					(m_debug_renderer);
#endif

	if (!g_dedicated_server)
		ai().script_engine().remove_script_process(ScriptEngine::eScriptProcessorLevel);

	xr_delete					(game);
	xr_delete					(game_events);


	//by Dandy
	//destroy fog of war
//	xr_delete					(m_pFogOfWar);
	//destroy bullet manager
	xr_delete					(m_pBulletManager);
	//-----------------------------------------------------------
	xr_delete					(pStatGraphR);
	xr_delete					(pStatGraphS);

	//-----------------------------------------------------------
	xr_delete					(m_ph_commander);
	xr_delete					(m_fast_updater);
	xr_delete					(m_ph_commander_scripts);
	//-----------------------------------------------------------
	pObjects4CrPr.clear();
	pActors4CrPr.clear();

	ai().unload					();
	//-----------------------------------------------------------	
#ifdef DEBUG	
	xr_delete					(m_level_debug);
#endif
	//-----------------------------------------------------------
	xr_delete					(m_map_manager);
//	xr_delete					(m_pFogOfWarMngr);
	//-----------------------------------------------------------
	Demo_Clear					();
	m_aDemoData.clear			();

	// here we clean default trade params
	// because they should be new for each saved/loaded game
	// and I didn't find better place to put this code in
	CTradeParameters::clean		();

	if (m_we_used_old_crach_handler)
		Debug.set_crashhandler	(m_pOldCrashHandler);

}

shared_str	CLevel::name		() const
{
	return						(m_name);
}

void CLevel::GetLevelInfo( CServerInfo* si )
{
	Server->GetServerInfo( si );
}


void CLevel::PrefetchSound		(LPCSTR name)
{
	// preprocess sound name
	string_path					tmp;
	xr_strcpy					(tmp,name);
	xr_strlwr					(tmp);
	if (strext(tmp))			*strext(tmp)=0;
	shared_str	snd_name		= tmp;
	// find in registry
	SoundRegistryMapIt it		= sound_registry.find(snd_name);
	// if find failed - preload sound
	if (it==sound_registry.end())
		sound_registry[snd_name].create(snd_name.c_str(),st_Effect,sg_SourceType);
}

// Game interface ////////////////////////////////////////////////////
int	CLevel::get_RPID(LPCSTR /**name/**/)
{
	/*
	// Gain access to string
	LPCSTR	params = pLevel->r_string("respawn_point",name);
	if (0==params)	return -1;

	// Read data
	Fvector4	pos;
	int			team;
	sscanf		(params,"%f,%f,%f,%d,%f",&pos.x,&pos.y,&pos.z,&team,&pos.w); pos.y += 0.1f;

	// Search respawn point
	svector<Fvector4,maxRP>	&rp = Level().get_team(team).RespawnPoints;
	for (int i=0; i<(int)(rp.size()); ++i)
		if (pos.similar(rp[i],EPS_L))	return i;
	*/
	return -1;
}

BOOL		g_bDebugEvents = FALSE	;


void CLevel::cl_Process_Event				(u16 dest, u16 type, NET_Packet& P)
{
	//			Msg				("--- event[%d] for [%d]",type,dest);
	CObject*	 O	= Objects.net_Find	(dest);
	if (0==O)		{
#ifdef DEBUG
		Msg("* WARNING: c_EVENT[%d] to object: can't find object with id [%d]",type,dest);
#endif // DEBUG
		return;
	}
	CGameObject* GO = smart_cast<CGameObject*>(O);
	if (!GO)		{
		Msg("! ERROR: c_EVENT[%d] to object: is not gameobject",dest);
		return;
	}
	if (type != GE_DESTROY_REJECT)
	{
		if (type == GE_DESTROY)
			Game().OnDestroy(GO);
		GO->OnEvent		(P,type);
	}
	else { // handle GE_DESTROY_REJECT here
		u32				pos = P.r_tell();
		u16				id = P.r_u16();
		P.r_seek		(pos);

		bool			ok = true;

		CObject			*D	= Objects.net_Find	(id);
		if (0==D)		{
			Msg			("! ERROR: c_EVENT[%d] : unknown dest",id);
			ok			= false;
		}

		CGameObject		*GD = smart_cast<CGameObject*>(D);
		if (!GD)		{
			Msg			("! ERROR: c_EVENT[%d] : non-game-object",id);
			ok			= false;
		}

		GO->OnEvent		(P,GE_OWNERSHIP_REJECT);
		if (ok)
		{
			Game().OnDestroy(GD);
			GD->OnEvent	(P,GE_DESTROY);
		};
	}
};

void CLevel::ProcessGameEvents		()
{
	// Game events
	{
		NET_Packet			P;
		u32 svT				= timeServer()-NET_Latency;

		/*
		if (!game_events->queue.empty())	
			Msg("- d[%d],ts[%d] -- E[svT=%d],[evT=%d]",Device.dwTimeGlobal,timeServer(),svT,game_events->queue.begin()->timestamp);
		*/

		while	(game_events->available(svT))
		{
			u16 ID,dest,type;
			game_events->get	(ID,dest,type,P);

			switch (ID)
			{
			case M_SPAWN:
				{
					u16 dummy16;
					P.r_begin(dummy16);
					cl_Process_Spawn(P);
				}break;
			case M_EVENT:
				{
					cl_Process_Event(dest, type, P);
				}break;
			default:
				{
					VERIFY(0);
				}break;
			}			
		}
	}
	if (OnServer() && GameID()!= GAME_SINGLE)
		Game().m_WeaponUsageStatistic->Send_Check_Respond();
}

#ifdef DEBUG_MEMORY_MANAGER
	extern Flags32				psAI_Flags;
	extern float				debug_on_frame_gather_stats_frequency;

struct debug_memory_guard {
	inline debug_memory_guard	()
	{
		mem_alloc_gather_stats				(!!psAI_Flags.test(aiDebugOnFrameAllocs));
		mem_alloc_gather_stats_frequency	(debug_on_frame_gather_stats_frequency);
	}

	inline ~debug_memory_guard	()
	{
//		mem_alloc_gather_stats				(false);
	}
};
#endif // DEBUG_MEMORY_MANAGER

void CLevel::OnFrame	()
{
#ifdef DEBUG_MEMORY_MANAGER
	debug_memory_guard					__guard__;
#endif // DEBUG_MEMORY_MANAGER

	m_feel_deny.update					();

	if (GameID()!=GAME_SINGLE)			psDeviceFlags.set(rsDisableObjectsAsCrows,true);
	else								psDeviceFlags.set(rsDisableObjectsAsCrows,false);

	// commit events from bullet manager from prev-frame
	Device.Statistic->TEST0.Begin		();
	BulletManager().CommitEvents		();
	Device.Statistic->TEST0.End			();

	// Client receive
	if (net_isDisconnected())	
	{
		if (OnClient() && GameID() != GAME_SINGLE) 
			ClearAllObjects();

		g_pEventManager->Event.Defer				("kernel:disconnect");
		return;
	} else {

		Device.Statistic->netClient1.Begin();

		ClientReceive					();

		Device.Statistic->netClient1.End	();
	}

	ProcessGameEvents	();


	if (m_bNeed_CrPr)					make_NetCorrectionPrediction();

	if(!g_dedicated_server )
	{
		if (g_mt_config.test(mtMap)) 
			Device.seqParallel.push_back(xr_make_delegate(m_map_manager, &CMapManager::Update));
		else								
			MapManager().Update		();
	}
	// Inherited update
	inherited::OnFrame		();

	// Draw client/server stats
	if ( !g_dedicated_server && psDeviceFlags.test(rsStatistic))
	{
		CGameFont* F = UI().Font().pFontDI;
		if (!psNET_direct_connect) 
		{
			if ( IsServer() )
			{
				const IServerStatistic* S = Server->GetStatistic();
				F->SetHeight	(0.015f);
				F->OutSetI	(0.0f,0.5f);
				F->SetColor	(color_xrgb(0,255,0));
				F->OutNext	("IN:  %4d/%4d (%2.1f%%)",	S->bytes_in_real,	S->bytes_in,	100.f*float(S->bytes_in_real)/float(S->bytes_in));
				F->OutNext	("OUT: %4d/%4d (%2.1f%%)",	S->bytes_out_real,	S->bytes_out,	100.f*float(S->bytes_out_real)/float(S->bytes_out));
				F->OutNext	("client_2_sever ping: %d",	net_Statistic.getPing());
				F->OutNext	("SPS/Sended : %4d/%4d", S->dwBytesPerSec, S->dwBytesSended);
				F->OutNext	("sv_urate/cl_urate : %4d/%4d", psNET_ServerUpdate, psNET_ClientUpdate);

				F->SetColor	(color_xrgb(255,255,255));
				struct net_stats_functor
				{
					xrServer* m_server;
					CGameFont* F;
					void operator()(IClient* C)
					{
						m_server->UpdateClientStatistic(C);
						F->OutNext("0x%08x: P(%d), BPS(%2.1fK), MRR(%2d), MSR(%2d), Retried(%2d), Blocked(%2d)",
							//Server->game->get_option_s(*C->Name,"name",*C->Name),
							C->ID.value(),
							C->stats.getPing(),
							float(C->stats.getBPS()),// /1024,
							C->stats.getMPS_Receive(),
							C->stats.getMPS_Send(),
							C->stats.getRetriedCount(),
							C->stats.dwTimesBlocked
						);
					}
				};
				net_stats_functor tmp_functor;
				tmp_functor.m_server = Server;
				tmp_functor.F = F;
				Server->ForEachClientDo(tmp_functor);
			}
			if (IsClient())
			{
				IPureClient::UpdateStatistic();

				F->SetHeight(0.015f);
				F->OutSetI	(0.0f,0.5f);
				F->SetColor	(color_xrgb(0,255,0));
				F->OutNext	("client_2_sever ping: %d",	net_Statistic.getPing());
				F->OutNext	("sv_urate/cl_urate : %4d/%4d", psNET_ServerUpdate, psNET_ClientUpdate);

				F->SetColor	(color_xrgb(255,255,255));
				F->OutNext("P(%d), MRR(%2d), MSR(%2d), Retried(%2d), Blocked(%2d), Sended(%2d), SPS(%2d)", // BPS(%2.1fK),
					//Server->game->get_option_s(C->Name,"name",C->Name),
					//					C->Name,
					net_Statistic.getPing(),
					float(net_Statistic.getBPS()),// /1024,
					net_Statistic.getMPS_Receive	(),
					net_Statistic.getMPS_Send	(),
					net_Statistic.getRetriedCount(),
					net_Statistic.dwTimesBlocked,
					net_Statistic.dwBytesSended
//					net_Statistic.dwBytesPerSec
					);
			}
		}
	}
	
	g_pGamePersistent->Environment().SetGameTime(GetEnvironmentGameDayTimeSec(), game->GetEnvironmentGameTimeFactor());

	//Device.Statistic->cripting.Begin	();
	if (!g_dedicated_server)
	{
		//TODO: why CLevel::OnFrame is calling before CLevel::Load_GameSpecific_After?
		//is it logic error?
		CScriptProcess * levelScript = ai().script_engine().script_process(ScriptEngine::eScriptProcessorLevel);
		if (levelScript != nullptr)
			levelScript->update();
	}
	//Device.Statistic->Scripting.End	();
	m_ph_commander->update				();
	m_ph_commander_scripts->update		();
	m_fast_updater->Update				();
//	autosave_manager().update			();

	//просчитать полет пуль
	Device.Statistic->TEST0.Begin		();
	BulletManager().CommitRenderSet		();
	Device.Statistic->TEST0.End			();
#ifdef USE_TIMERS_MANAGER
	if (GameID()==GAME_SINGLE && ai().alife().initialized())
		ai().alife().timers().Update();
#endif
	// update static sounds
	if(!g_dedicated_server)
	{
		if (g_mt_config.test(mtLevelSounds)) 
			Device.seqParallel.push_back(xr_make_delegate(m_level_sound_manager, &CLevelSoundManager::Update));
		else								
			m_level_sound_manager->Update	();
	}
	// deffer LUA-GC-STEP
	if (!g_dedicated_server)
	{
		if (g_mt_config.test(mtLUA_GC))
			Device.seqParallel.push_back(xr_make_delegate(this, &CLevel::script_gc));
		else
			script_gc();
	}
	//-----------------------------------------------------
	if (pStatGraphR)
	{	
		static	float fRPC_Mult = 10.0f;
		static	float fRPS_Mult = 1.0f;

		pStatGraphR->AppendItem(float(m_dwRPC)*fRPC_Mult, 0xffff0000, 1);
		pStatGraphR->AppendItem(float(m_dwRPS)*fRPS_Mult, 0xff00ff00, 0);
	};
}

void	CLevel::script_gc				()
{
	/* KTO: хз для чего это, потом включите
	{
		PROF_EVENT("m_ph_commander");
		ai().script_engine().script_process(ScriptEngine::eScriptProcessorLevel)->update();
		m_ph_commander->update();
		m_ph_commander_scripts->update();
	}*/
	lua_gc	(ai().script_engine().lua(), LUA_GCSTEP, psLUA_GCSTEP);
}

#ifdef DEBUG_PRECISE_PATH
void test_precise_path	();
#endif

#ifdef DEBUG
extern	Flags32	dbg_net_Draw_Flags;
#endif

extern void draw_wnds_rects();

void CLevel::OnRender()
{
	inherited::OnRender	();

	if (!game)
		return;

	Game().OnRender();
	//отрисовать трассы пуль
	//Device.Statistic->TEST1.Begin();
	BulletManager().Render();
	//Device.Statistic->TEST1.End();
	//отрисовать интерфейc пользователя
	HUD().RenderUI();

	draw_wnds_rects();




#ifdef DEBUG
	ph_world->OnRender	();
#endif

#ifdef DEBUG
	if (ai().get_level_graph() && dynamic_cast<CLevelGraph*>(&ai().level_graph()))
		dynamic_cast<CLevelGraph*>(&ai().level_graph())->render();

#ifdef DEBUG_PRECISE_PATH
	test_precise_path		();
#endif

	CAI_Stalker				*stalker = smart_cast<CAI_Stalker*>(Level().CurrentEntity());
	if (stalker)
		stalker->OnRender	();

	if (bDebug)	{
		for (u32 I=0; I < Level().Objects.o_count(); I++) {
			CObject*	_O		= Level().Objects.o_get_by_iterator(I);

			CPhysicObject		*physic_object = smart_cast<CPhysicObject*>(_O);
			if (physic_object)
				physic_object->OnRender();

			CSpaceRestrictor	*space_restrictor = smart_cast<CSpaceRestrictor*>	(_O);
			if (space_restrictor)
				space_restrictor->OnRender();
			CClimableObject		*climable		  = smart_cast<CClimableObject*>	(_O);
			if(climable)
				climable->OnRender();
			CTeamBaseZone	*team_base_zone = smart_cast<CTeamBaseZone*>(_O);
			if (team_base_zone)
				team_base_zone->OnRender();
			
			if (GameID() != GAME_SINGLE)
			{
				CInventoryItem* pIItem = smart_cast<CInventoryItem*>(_O);
				if (pIItem) pIItem->OnRender();
			}

			
			if (dbg_net_Draw_Flags.test(1<<11)) //draw skeleton
			{
				CGameObject* pGO = smart_cast<CGameObject*>	(_O);
				if (pGO && pGO != Level().CurrentViewEntity() && !pGO->H_Parent())
				{
					if (pGO->Position().distance_to_sqr(Device.vCameraPosition) < 400.0f)
					{
						pGO->dbg_DrawSkeleton();
					}
				}
			};
		}
		//  [7/5/2005]
		if (Server && Server->game) Server->game->OnRender();
		//  [7/5/2005]
		ObjectSpace.dbgRender	();

		//---------------------------------------------------------------------
		UI().Font().pFontStat->OutSet		(170,630);
		UI().Font().pFontStat->SetHeight	(16.0f);
		UI().Font().pFontStat->SetColor	(0xffff0000);

		if(Server)UI().Font().pFontStat->OutNext	("Client Objects:      [%d]",Server->GetEntitiesNum());
		UI().Font().pFontStat->OutNext	("Server Objects:      [%d]",Objects.o_count());
		UI().Font().pFontStat->OutNext	("Interpolation Steps: [%d]", Level().GetInterpolationSteps());
		UI().Font().pFontStat->SetHeight	(8.0f);
		//---------------------------------------------------------------------
	}
#endif

#ifdef DEBUG
	if (bDebug) {
		DBG().draw_object_info				();
		DBG().draw_text						();
		DBG().draw_level_info				();
	}
#endif

#ifdef DEBUG_DRAW
	debug_renderer().render					();
#endif

#ifdef LOG_PLANNER
	if (psAI_Flags.is(aiVision)) {
		for (u32 I=0; I < Level().Objects.o_count(); I++) {
			CObject						*object = Objects.o_get_by_iterator(I);
			CAI_Stalker					*stalker = smart_cast<CAI_Stalker*>(object);
			if (!stalker)
				continue;
			stalker->dbg_draw_vision	();
		}
	}


	if (psAI_Flags.test(aiDrawVisibilityRays)) {
		for (u32 I=0; I < Level().Objects.o_count(); I++) {
			CObject						*object = Objects.o_get_by_iterator(I);
			CAI_Stalker					*stalker = smart_cast<CAI_Stalker*>(object);
			if (!stalker)
				continue;

			stalker->dbg_draw_visibility_rays	();
		}
	}
#endif
}

void CLevel::OnEvent(EVENT E, u64 P1, u64 /**P2/**/)
{
	if (E==eEntitySpawn)	{
		char	Name[128];	Name[0]=0;
		sscanf	(LPCSTR(P1),"%s", Name);
		Level().g_cl_Spawn	(Name,0xff, M_SPAWN_OBJECT_LOCAL, Fvector().set(0,0,0));
	} else if (E==eChangeRP && P1) {
	} else if (E==eDemoPlay && P1) {
		char* name = (char*)P1;
		string_path RealName;
		xr_strcpy		(RealName,name);
		xr_strcat			(RealName,".xrdemo");
		Cameras().AddCamEffector(new CDemoPlay(RealName,1.3f,0));
	} else if (E==eChangeTrack && P1) {
		// int id = atoi((char*)P1);
		// Environment->Music_Play(id);
	} else if (E==eEnvironment) {
		// int id=0; float s=1;
		// sscanf((char*)P1,"%d,%f",&id,&s);
		// Environment->set_EnvMode(id,s);
	} else return;
}

void	CLevel::AddObject_To_Objects4CrPr	(CGameObject* pObj)
{
	if (!pObj) return;
	for	(OBJECTS_LIST_it OIt = pObjects4CrPr.begin(); OIt != pObjects4CrPr.end(); OIt++)
	{
		if (*OIt == pObj) return;
	}
	pObjects4CrPr.push_back(pObj);

}
void	CLevel::AddActor_To_Actors4CrPr		(CGameObject* pActor)
{
	if (!pActor) return;
	if (pActor->CLS_ID != CLSID_OBJECT_ACTOR) return;
	for	(OBJECTS_LIST_it AIt = pActors4CrPr.begin(); AIt != pActors4CrPr.end(); AIt++)
	{
		if (*AIt == pActor) return;
	}
	pActors4CrPr.push_back(pActor);
}

void	CLevel::RemoveObject_From_4CrPr		(CGameObject* pObj)
{
	if (!pObj) return;
	
	OBJECTS_LIST_it OIt = std::find(pObjects4CrPr.begin(), pObjects4CrPr.end(), pObj);
	if (OIt != pObjects4CrPr.end())
	{
		pObjects4CrPr.erase(OIt);
	}

	OBJECTS_LIST_it AIt = std::find(pActors4CrPr.begin(), pActors4CrPr.end(), pObj);
	if (AIt != pActors4CrPr.end())
	{
		pActors4CrPr.erase(AIt);
	}
}

void CLevel::make_NetCorrectionPrediction	()
{
	m_bNeed_CrPr	= false;
	m_bIn_CrPr		= true;
	u64 NumPhSteps = ph_world->m_steps_num;
	ph_world->m_steps_num -= m_dwNumSteps;
	if(g_bDebugDumpPhysicsStep&&m_dwNumSteps>10)
	{
		Msg("!!!TOO MANY PHYSICS STEPS FOR CORRECTION PREDICTION = %d !!!",m_dwNumSteps);
		m_dwNumSteps = 10;
	};
//////////////////////////////////////////////////////////////////////////////////
	ph_world->Freeze();

	//setting UpdateData and determining number of PH steps from last received update
	for	(OBJECTS_LIST_it OIt = pObjects4CrPr.begin(); OIt != pObjects4CrPr.end(); OIt++)
	{
		CGameObject* pObj = *OIt;
		if (!pObj) continue;
		pObj->PH_B_CrPr();
	};
//////////////////////////////////////////////////////////////////////////////////
	//first prediction from "delivered" to "real current" position
	//making enought PH steps to calculate current objects position based on their updated state	
	
	for (u32 i =0; i<m_dwNumSteps; i++)	
	{
		ph_world->Step();

		for	(OBJECTS_LIST_it AIt = pActors4CrPr.begin(); AIt != pActors4CrPr.end(); AIt++)
		{
			CGameObject* pActor = *AIt;
			if (!pActor || pActor->CrPr_IsActivated()) continue;
			pActor->PH_B_CrPr();
		};
	};
//////////////////////////////////////////////////////////////////////////////////
	for	(OBJECTS_LIST_it OIt = pObjects4CrPr.begin(); OIt != pObjects4CrPr.end(); OIt++)
	{
		CGameObject* pObj = *OIt;
		if (!pObj) continue;
		pObj->PH_I_CrPr();
	};
//////////////////////////////////////////////////////////////////////////////////
	if (!InterpolationDisabled())
	{
		for (u32 i =0; i<lvInterpSteps; i++)	//second prediction "real current" to "future" position
		{
			ph_world->Step();
#ifdef DEBUG
/*
			for	(OBJECTS_LIST_it OIt = pObjects4CrPr.begin(); OIt != pObjects4CrPr.end(); OIt++)
			{
				CGameObject* pObj = *OIt;
				if (!pObj) continue;
				pObj->PH_Ch_CrPr();
			};
*/
#endif
		}
		//////////////////////////////////////////////////////////////////////////////////
		for	(OBJECTS_LIST_it OIt = pObjects4CrPr.begin(); OIt != pObjects4CrPr.end(); OIt++)
		{
			CGameObject* pObj = *OIt;
			if (!pObj) continue;
			pObj->PH_A_CrPr();
		};
	};
	ph_world->UnFreeze();

	ph_world->m_steps_num = NumPhSteps;
	m_dwNumSteps = 0;
	m_bIn_CrPr = false;

	pObjects4CrPr.clear();
	pActors4CrPr.clear();
};

u32			CLevel::GetInterpolationSteps	()
{
	return lvInterpSteps;
};

void		CLevel::UpdateDeltaUpd	( u32 LastTime )
{
	u32 CurrentDelta = LastTime - m_dwLastNetUpdateTime;
	if (CurrentDelta < m_dwDeltaUpdate) 
		CurrentDelta = iFloor(float(m_dwDeltaUpdate * 10 + CurrentDelta) / 11);

	m_dwLastNetUpdateTime = LastTime;
	m_dwDeltaUpdate = CurrentDelta;

	if (0 == g_cl_lvInterp) ReculcInterpolationSteps();
	else 
		if (g_cl_lvInterp>0)
		{
			lvInterpSteps = iCeil(g_cl_lvInterp / fixed_step);
		}
};

void		CLevel::ReculcInterpolationSteps ()
{
	lvInterpSteps			= iFloor(float(m_dwDeltaUpdate) / (fixed_step*1000));
	if (lvInterpSteps > 60) lvInterpSteps = 60;
	if (lvInterpSteps < 3)	lvInterpSteps = 3;
};

bool		CLevel::InterpolationDisabled	()
{
	return g_cl_lvInterp < 0; 
};

void 		CLevel::PhisStepsCallback		( u32 Time0, u32 Time1 )
{
	if (!Level().game)				return;
	if (GameID() == GAME_SINGLE)	return;

//#pragma todo("Oles to all: highly inefficient and slow!!!")
//fixed (Andy)
	/*
	for (xr_vector<CObject*>::iterator O=Level().Objects.objects.begin(); O!=Level().Objects.objects.end(); ++O) 
	{
		if( (*O)->CLS_ID == CLSID_OBJECT_ACTOR){
			CActor* pActor = smart_cast<CActor*>(*O);
			if (!pActor || pActor->Remote()) continue;
				pActor->UpdatePosStack(Time0, Time1);
		}
	};
	*/
};

void				CLevel::SetNumCrSteps		( u32 NumSteps )
{
	m_bNeed_CrPr = true;
	if (m_dwNumSteps > NumSteps) return;
	m_dwNumSteps = NumSteps;
	if (m_dwNumSteps > 1000000)
	{
		VERIFY(0);
	}
};


ALife::_TIME_ID CLevel::GetGameTime()
{
	return			(game->GetGameTime());
//	return			(Server->game->GetGameTime());
}

ALife::_TIME_ID CLevel::GetEnvironmentGameTime()
{
	return			(game->GetEnvironmentGameTime());
//	return			(Server->game->GetGameTime());
}

// gr1ph start

#include "game_sv_single.h"
#include "game_cl_single.h"

void CLevel::SetGameTime(u32 new_hours, u32 new_mins)
{
	float time_factor = Level().GetGameTimeFactor();
	u32 year = 1, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time(Level().GetGameTime(), year, month, day, hours, mins, secs, milisecs);
	u64 new_time = generate_time(year,month,day,new_hours,new_mins,secs,milisecs);
	game_sv_Single* server_game = smart_cast<game_sv_Single*>(Level().Server->game);  
	game_cl_Single* client_game = smart_cast<game_cl_Single*>(Level().game);			 
	server_game->SetGameTimeFactor(new_time, time_factor);
	server_game->SetEnvironmentGameTimeFactor(new_time, time_factor);
	client_game->SetEnvironmentGameTimeFactor(new_time, time_factor);
	client_game->SetGameTimeFactor(new_time, time_factor);
}

u8 CLevel::GetDayTime() 
{ 
	u32 dummy32;
	u32 hours;
	GetGameDateTime(dummy32, dummy32, dummy32, hours, dummy32, dummy32, dummy32);
	VERIFY	(hours<256);
	return	u8(hours); 
}

float CLevel::GetGameDayTimeSec()
{
	return	(float(s64(GetGameTime() % (24*60*60*1000)))/1000.f);
}

u32 CLevel::GetGameDayTimeMS()
{
	return	(u32(s64(GetGameTime() % (24*60*60*1000))));
}

float CLevel::GetEnvironmentTimeFactor() const
{
	if (!game)
		return 0.0f;
	return game->GetEnvironmentGameTimeFactor();
}

u64 CLevel::GetEnvironmentGameTime() const
{
	if (!game)
		return 0;
	return game->GetEnvironmentGameTime();
}

void CLevel::SetEnvironmentTimeFactor(const float fTimeFactor)
{
	if (!game)
		return;
	game->SetEnvironmentGameTimeFactor(fTimeFactor);
}

float CLevel::GetEnvironmentGameDayTimeSec()
{
	return	(float(s64(GetEnvironmentGameTime() % (24*60*60*1000)))/1000.f);
}

void CLevel::GetGameDateTime	(u32& year, u32& month, u32& day, u32& hours, u32& mins, u32& secs, u32& milisecs)
{
	split_time(GetGameTime(), year, month, day, hours, mins, secs, milisecs);
}

void CLevel::GetGameTimeHour(u32& hours)
{
	u32 dummy1;
	split_time(GetGameTime(), dummy1, dummy1, dummy1, hours, dummy1, dummy1, dummy1);
}

void CLevel::GetGameTimeMinute(u32& minute)
{
	u32 dummy1;
	split_time(GetGameTime(), dummy1, dummy1, dummy1, dummy1, minute, dummy1, dummy1);
}

float CLevel::GetGameTimeFactor()
{
	return			(game->GetGameTimeFactor());
}

void CLevel::SetGameTimeFactor(const float fTimeFactor)
{
	game->SetGameTimeFactor(fTimeFactor);
}

void CLevel::SetGameTimeFactor(ALife::_TIME_ID GameTime, const float fTimeFactor)
{
	game->SetGameTimeFactor(GameTime, fTimeFactor);
}
void CLevel::SetEnvironmentGameTimeFactor(u64 const& GameTime, float const& fTimeFactor)
{
	if (!game)
		return;

	game->SetEnvironmentGameTimeFactor(GameTime, fTimeFactor);
//	Server->game->SetGameTimeFactor(fTimeFactor);
}/*
void CLevel::SetGameTime(ALife::_TIME_ID GameTime)
{
	game->SetGameTime(GameTime);
//	Server->game->SetGameTime(GameTime);
}
*/
bool CLevel::IsServer ()
{
//	return (!!Server);
	if (IsDemoPlay())
	{
		return IsServerDemo();
	};	
	if (!Server) return false;
	return (Server->GetClientsCount() != 0);

}

bool CLevel::IsClient ()
{
//	return (!Server);
	if (IsDemoPlay())
	{
		return IsClientDemo();
	};	
	if (!Server) return true;
	return (Server->GetClientsCount() == 0);
}

void CLevel::OnSessionTerminate		(LPCSTR reason)
{
	MainMenu()->OnSessionTerminate(reason);
}

u32	GameID()
{
	return GAME_SINGLE;	
}

#include "../IGame_Persistent.h"

bool	IsGameTypeSingle()
{
	return g_pGamePersistent->GameType()==GAME_SINGLE || g_pGamePersistent->GameType()==GAME_ANY;
}

#ifdef BATTLEYE

bool CLevel::TestLoadBEClient()
{
	return battleye_system.TestLoadClient();
}

#endif // BATTLEYE

GlobalFeelTouch::GlobalFeelTouch()
{
}

GlobalFeelTouch::~GlobalFeelTouch()
{
}

struct delete_predicate_by_time 
{
	bool operator () (Feel::Touch::DenyTouch const & left, DWORD const expire_time) const
	{
		if (left.Expire <= expire_time)
			return true;
		return false;
	};
};
struct objects_ptrs_equal
{
	bool operator() (Feel::Touch::DenyTouch const & left, CObject const * const right) const
	{
		if (left.O == right)
			return true;
		return false;
	}
};

void GlobalFeelTouch::update()
{
	//we ignore P and R arguments, we need just delete evaled denied objects...
	xr_vector<Feel::Touch::DenyTouch>::iterator new_end =
		std::remove_if(feel_touch_disable.begin(), feel_touch_disable.end(),
			std::bind(delete_predicate_by_time(), std::placeholders::_1, Device.dwTimeGlobal));
	feel_touch_disable.erase(new_end, feel_touch_disable.end());
}

bool GlobalFeelTouch::is_object_denied(CObject const* O)
{
	if (std::find_if(feel_touch_disable.begin(), feel_touch_disable.end(),
		std::bind(objects_ptrs_equal(), std::placeholders::_1, O)) == feel_touch_disable.end())
		return false;
	return true;
}

#include "../../xrNetServer/NET_AuthCheck.h"

struct path_excluder_predicate
{
	explicit path_excluder_predicate(xr_auth_strings_t const * ignore) :
		m_ignore(ignore)
	{
	}
	bool  is_allow_include(LPCSTR path)
	{
		if (!m_ignore)
			return true;

		return allow_to_include_path(*m_ignore, path);
	}
	xr_auth_strings_t const *	m_ignore;
};

void CLevel::ReloadEnvironment()
{
	Msg("! Not implemented");
/*	g_pGamePersistent->DestroyEnvironment();
	Msg("---Environment destroyed");
	Msg("---Start to destroy configs");
	CInifile** s = (CInifile**)(&pSettings);
	xr_delete(*s);
	xr_delete(pGameIni);
	Msg("---Start to rescan configs");
	FS.get_path("$game_config$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
	FS.get_path("$game_scripts$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
	FS.rescan_pathes();

	Msg("---Start to create configs");
	string_path					fname;
	FS.update_path(fname, "$game_config$", "system.ltx");
	Msg("---Updated path to system.ltx is %s", fname);

	pSettings = new CInifile(fname, TRUE);
	CHECK_OR_EXIT(0 != pSettings->section_count(), make_string<const char*>("Cannot find file %s.\nReinstalling application may fix this problem.", fname));

	xr_auth_strings_t			tmp_ignore_pathes;
	xr_auth_strings_t			tmp_check_pathes;
	fill_auth_check_params(tmp_ignore_pathes, tmp_check_pathes);

	path_excluder_predicate			tmp_excluder(&tmp_ignore_pathes);
	CInifile::allow_include_func_t	tmp_functor;
	tmp_functor.bind(&tmp_excluder, &path_excluder_predicate::is_allow_include);
	pSettingsAuth = new CInifile(
		fname,
		TRUE,
		TRUE,
		FALSE,
		0,
		tmp_functor
		);

	FS.update_path(fname, "$game_config$", "game.ltx");
	pGameIni = new CInifile(fname, TRUE);
	CHECK_OR_EXIT(0 != pGameIni->section_count(), make_string<const char*>("Cannot find file %s.\nReinstalling application may fix this problem.", fname));

	Msg("---Create environment");
	g_pGamePersistent->CreateEnvironment();

	Msg("---Call level_weathers.restart_weather_manager");
	luabind::functor<void>	lua_function;
	string256		fn;
	xr_strcpy(fn, "level_weathers.restart_weather_manager");
	R_ASSERT2(ai().script_engine().functor<void>(fn, lua_function), make_string<const char*>("Can't find function %s", fn));
	lua_function();
	Msg("---Done");*/
}


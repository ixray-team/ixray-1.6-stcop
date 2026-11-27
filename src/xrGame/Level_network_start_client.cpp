#include "StdAfx.h"
//#include "PHdynamicdata.h"
//#include "Physics.h"
#include "Level.h"
#include "../xrEngine/x_ray.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrCore/BackgroundLoad.h"

#include "ai_space.h"
#include "player_hud.h"
#include "../xrCore/IniReadAhead.h"
#include "player_hud.h"
#include "../xrCore/IniReadAhead.h"
#include "game_cl_base.h"
#include "NET_Queue.h"
#include "file_transfer.h"
#include "HUDManager.h"

#include "../xrPhysics/IPHWorld.h"


#include "PHCommander.h"
#include "physics_game.h"

bool CLevel::net_Start_client	( const char* options )
{
	return false;
}
#include "../xrEngine/string_table.h"
bool	CLevel::net_start_client1				()
{
	PROF_EVENT("CLevel::net_start_client1");
	if (!LevelDesc.Parallel)
		pApp->LoadBegin();
	// name_of_server
	string64					name_of_server = "";
//	xr_strcpy						(name_of_server,*m_caClientOptions);
	if (strchr(*m_caClientOptions, '/'))
		strncpy_s(name_of_server,*m_caClientOptions, strchr(*m_caClientOptions, '/')-*m_caClientOptions);

	if (strchr(name_of_server,'/'))	*strchr(name_of_server,'/') = 0;

	// Startup client
	string256					temp;
	xr_sprintf						(temp,"%s %s",
								g_pStringTable->translate("st_client_connecting_to").c_str(), name_of_server);

	g_pGamePersistent->SetLoadStageTitle				(temp);

	g_pGamePersistent->LoadTitle();
	return true;
}

#include "xrServer.h"

bool	CLevel::net_start_client2				()
{
	PROF_EVENT("CLevel::net_start_client2");
	if (Server != nullptr && IsGameTypeSingle())
	{
		Server->create_direct_client();
		if (LevelDesc.RemoteTransport)
		{
			Server->AttachLocalHostClient(this);
			RemoteClientID.set(1);
			SetClientID(RemoteClientID);
			m_bConnectResultReceived = true;
		}
		else
		{
			//offline account creation
			m_bConnectResultReceived = false;
			while (!m_bConnectResultReceived)
			{
				ClientReceive();
				Server->Update();
			}
		}
	}

	if (RemoteServer != nullptr)
	{
		ClientConnectionOptions options;
		ParseConnectionOptions(m_caClientOptions.c_str(), options);
		if (!CreateConnection(options))
		{
			Msg("! net_start_client2: failed to attach the linked local client");
			net_start_result_total = false;
			m_bConnectResultReceived = true;
			return true;
		}
		m_bConnectResultReceived = true;
	}

	connected_to_server = Connect2Server(*m_caClientOptions);

	return true;
}
void rescan_mp_archives()
{
	FS_Path* mp_archs_path = FS.get_path("$game_arch_mp$");
	FS.rescan_path(mp_archs_path->m_Path,
		mp_archs_path->m_Flags.is(FS_Path::flRecurse)
	);
}

bool	CLevel::net_start_client3				()
{
	PROF_EVENT("CLevel::net_start_client3");
	if(connected_to_server)
	{
		const char*					level_name = nullptr;
		const char*					level_ver = nullptr;
		const char*					download_url = nullptr;

		if (UseDirectConnection())	//single
		{
			shared_str const & server_options = Server->GetConnectOptions();
			level_name	= name().c_str();//Server->level_name		(server_options).c_str();
			level_ver	= Server->level_version		(server_options).c_str(); //1.0
			g_pGamePersistent->RequestLearnedModels(level_name);
		} else					//multiplayer
		{
			level_name		= get_net_DescriptionData().map_name;
			level_ver		= get_net_DescriptionData().map_version;
			download_url	= get_net_DescriptionData().download_url;
			rescan_mp_archives(); //because if we are using UseDirectConnection(), we not download map...
		}
		// Determine internal level-ID
		int						level_id = pApp->Level_ID(level_name, level_ver, true);
		if (level_id==-1)	
		{
			Disconnect			();

			connected_to_server = false;
			Msg("! Level (name:%s), (version:%s), not found, try to download from:%s",
				level_name, level_ver, download_url);
			map_data.m_name					= level_name;
			map_data.m_map_version			= level_ver;
			map_data.m_map_download_url		= download_url;
			map_data.m_map_loaded			= false;
			return false;
		}
#ifdef DEBUG
		Msg("--- net_start_client3: level_id [%d], level_name[%s], level_version[%s]", level_id, level_name, level_ver);
#endif // #ifdef DEBUG
		map_data.m_name					= level_name;
		map_data.m_map_version			= level_ver;
		map_data.m_map_download_url		= download_url;
		map_data.m_map_loaded			= true;
		
		deny_m_spawn			= false;
		// Load level
		extern void WaitLevelLoad();
		WaitLevelLoad();
		if (!Load(level_id))
		{
			Msg("! net_start_client3: level load failed for '%s'", level_name);
			map_data.m_map_loaded = false;
			net_start_result_total = false;
			return true;
		}
		map_data.m_level_geom_crc32 = 0;
		if (!IsGameTypeSingle() && HasLevelResource(ELevelResource::RenderScene))
			CalculateLevelCrc32		();
	}
	return true;
}

bool CLevel::net_start_client4()
{
	PROF_EVENT("CLevel::net_start_client4");
	if (connected_to_server)
	{
		// Begin spawn
		if (!LevelDesc.Parallel)
			g_pGamePersistent->LoadTitle();

		// Send physics to single or multithreaded mode
		if (HasLevelResource(ELevelResource::PhysicsWorld))
		{
			SetResourceState(ELevelResource::PhysicsWorld, InitializePhysicsWorld() ? EResourceState::Ready : EResourceState::Missing);
		}
		else
		{
			SetResourceState(ELevelResource::PhysicsWorld, EResourceState::NotLoaded);
		}

		// Send network to single or multithreaded mode
		// *note: release version always has "mt_*" enabled
		// (parallel sessions are pumped by their owner instead)
		if (!LevelDesc.Parallel)
		{
			Device.seqFrameMT.Remove(&NetworkProcessor);
			Device.seqFrame.Remove(&NetworkProcessor);
			if (psDeviceFlags.test(mtNetwork))	Device.seqFrameMT.Add(&NetworkProcessor, REG_PRIORITY_HIGH + 2);
			else								Device.seqFrame.Add(&NetworkProcessor, REG_PRIORITY_LOW - 2);
		}

		if (!UseDirectConnection())
		{
			// Waiting for connection/configuration completition
			CTimer	timer_sync;	timer_sync.Start();
			while (!net_isCompleted_Connect())	Sleep(5);
			Msg("* connection sync: %d ms", timer_sync.GetElapsed_ms());
			while (!net_isCompleted_Sync()) { ClientReceive(); Sleep(5); }
		}
	}
	return true;
}

void CLevel::ClientSendProfileData	()
{
#ifdef DEBUG
	Msg("* Sending profile data");
#endif
	NET_Packet								NP;
	NP.w_begin								(M_CREATE_PLAYER_STATE);
	game_PlayerState	tmp_player_state	(nullptr);
	tmp_player_state.net_Export				(NP, true);
	SecureSend								(NP,net_flags(true, true, true, true));
}

bool CLevel::net_start_client5()
{
	PROF_EVENT("CLevel::net_start_client5");
	if (!LevelDesc.Parallel)
	{
		BackgroundLoad::SetStage(EBackgroundLoadStage::FinishLoading);
		if (g_player_hud)
			g_player_hud->FinishDefaultLoad();
		g_pGamePersistent->ObjectPool.FinishPrefetch();
	}
	if (connected_to_server)
	{
		if (!g_dedicated_server)
		{
			if (!LevelDesc.Parallel)
			{
				g_pGamePersistent->SetLoadStageTitle("st_loading_textures");
				g_pGamePersistent->LoadTitle();
			}
			// Deferred upload finalizes asynchronously loaded models/textures;
			// required in every session or spawned objects may lack visuals.
			Device.m_pRender->DeferredLoad(false);
			Device.m_pRender->ResourcesDeferredUpload();
		}
		sended_request_connection_data = false;
		deny_m_spawn = true;
	}

	return true;
}

bool CLevel::net_start_client6()
{
	PROF_EVENT("CLevel::net_start_client6");
	if (!LevelDesc.Parallel)
	{
		PROF_EVENT("client6: FinishPrefetch");
		g_pGamePersistent->ObjectPool.FinishPrefetch();
	}
	if (connected_to_server)
	{
		// Sync
		{
			PROF_EVENT("client6: synchronize_map_data");
			if (!synchronize_map_data())
				return false;
		}

		if (!game_configured)
		{
			if (!LevelDesc.Parallel)
				pApp->LoadEnd();
			return true;
		}
		if (!g_dedicated_server && g_hud && LevelDesc.Resources.test(u32(ELevelResource::Hud)))
		{
			PROF_EVENT("client6: HUD");
			g_hud->Load();
			g_hud->OnConnected();
		}

		if (game)
		{
			if (!g_dedicated_server)
			{
				PROF_EVENT("client6: game OnConnected");
				game->OnConnected();
			}

			if (game->Type() != eGameIDSingle)
			{
				m_file_transfer = new file_transfer::client_site();
			}
		}

		if (!LevelDesc.Parallel)
		{
			PROF_EVENT("client6: load title");
			g_pGamePersistent->SetLoadStageTitle("st_client_synchronising");
			pApp->LoadForceFinish();
			g_pGamePersistent->LoadTitle();
			Device.PreCache(60, true, true);
		}
	}

	net_start_result_total = connected_to_server;

	{
		PROF_EVENT("client6: ProcessPostponed");
		ProcessPostponed(false);
	}
	if (!LevelDesc.Parallel)
	{
		PROF_EVENT("client6: LoadEnd");
		pApp->LoadEnd();
	}
	return true;
}

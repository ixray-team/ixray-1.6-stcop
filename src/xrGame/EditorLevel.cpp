#include "StdAfx.h"

#include "EditorLevel.h"
#include "../xrPhysics/IPHWorld.h"
#include "PHCommander.h"
#include "physics_game.h"
#include "game_cl_base.h"
#include "NET_Queue.h"
#include "file_transfer.h"
#include "HUDManager.h"
#include "Level_Bullet_Manager.h"
#include "UIGameCustom.h"
#include "../xrEngine/XR_IOConsole.h"
#include "xrServer_Objects_ALife_Monsters.h"

CLevelEditor::CLevelEditor()
{
}

CLevelEditor::~CLevelEditor()
{
}
extern pureFrame* g_pNetProcessor;
static void 	build_callback(Fvector* V, int Vcnt, CDB::TRI* T, int Tcnt, void* params)
{
	g_pGameLevel->Load_GameSpecific_CFORM(T, Tcnt);
}

BOOL CLevelEditor::net_Start(LPCSTR op_server, LPCSTR op_client)
{
	auto& p = g_pGamePersistent->m_game_params;
	xr_strcpy(p.m_game_type ,"single");
	xr_strcpy(p.m_alife, "alife");
	xr_strcpy(p.m_new_or_load, "editor");
	xr_strcpy(p.m_game_or_spawn, "editor");
	p.m_e_game_type = eGameIDSingle;

	Server = new xrServer();
	map_data.m_name = "test";
	m_caServerOptions = op_server;
	m_caClientOptions = op_client;
	
	GameDescriptionData game_descr;
	if ((Server->Connect(m_caServerOptions, game_descr)) != xrServer::ErrNoError)
	{
		net_start_result_total = false;
		Msg("! Failed to start server.");
		return false;
	}
	Server->SLS_Default();
	if (psNET_direct_connect)
	{
		Server->create_direct_client();
		//offline account creation
		m_bConnectResultReceived = false;
		while (!m_bConnectResultReceived)
		{
			ClientReceive();
			Server->Update();
		}
	}

	connected_to_server = Connect2Server(*m_caClientOptions);

	if (connected_to_server)
	{
		LPCSTR					level_name = NULL;
		LPCSTR					level_ver = NULL;
		LPCSTR					download_url = NULL;

		if (psNET_direct_connect)	//single
		{
			shared_str const& server_options = Server->GetConnectOptions();
			level_name = name().c_str();//Server->level_name		(server_options).c_str();
			level_ver = Server->level_version(server_options).c_str(); //1.0
		}
		else					//multiplayer
		{
			R_ASSERT(false);
		}
	
#ifdef DEBUG
		Msg("--- net_start_client3: level_id [%d], level_name[%s], level_version[%s]", 0, level_name, level_ver);
#endif // #ifdef DEBUG
		map_data.m_name = level_name;
		map_data.m_map_version = level_ver;
		map_data.m_map_download_url = download_url;
		map_data.m_map_loaded = true;

		deny_m_spawn = FALSE;
		Device.seqRender.Add(this);
		Device.seqFrame.Add(this);
		R_ASSERT(Load_GameSpecific_Before());
		Objects.Load();
		EditorScene->LoadCForm(&ObjectSpace, build_callback);
		bReady = true;
		map_data.m_level_geom_crc32 = 0;
	}

	if (connected_to_server) 
	{
		g_pGamePersistent->LoadTitle();

		// Send physics to single or multithreaded mode
		create_physics_world(!!psDeviceFlags.test(mtPhysics), &ObjectSpace, &Objects);

		R_ASSERT(physics_world());

		m_ph_commander_physics_worldstep = new CPHCommander();
		physics_world()->set_update_callback(m_ph_commander_physics_worldstep);

		physics_world()->set_default_contact_shotmark(ContactShotMark);
		physics_world()->set_default_character_contact_shotmark(CharacterContactShotMark);

		VERIFY(physics_world());
		physics_world()->set_step_time_callback((PhysicsStepTimeCallback*)&PhisStepsCallback);


		// Send network to single or multithreaded mode
		// *note: release version always has "mt_*" enabled
		Device.seqFrameMT.Remove(g_pNetProcessor);
		Device.seqFrame.Remove(g_pNetProcessor);
		if (psDeviceFlags.test(mtNetwork))	Device.seqFrameMT.Add(g_pNetProcessor, REG_PRIORITY_HIGH + 2);
		else								Device.seqFrame.Add(g_pNetProcessor, REG_PRIORITY_LOW - 2);

		if (!psNET_direct_connect)
		{
			// Waiting for connection/configuration completition
			CTimer	timer_sync;	timer_sync.Start();
			while (!net_isCompleted_Connect())	Sleep(5);
			Msg("* connection sync: %d ms", timer_sync.GetElapsed_ms());
			while (!net_isCompleted_Sync()) { ClientReceive(); Sleep(5); }
		}
	}

	deny_m_spawn = TRUE;
	sended_request_connection_data = FALSE;
	{
		IReader F(nullptr, 0, 0);
		pLevel = new CInifile(&F);
	}
	if (connected_to_server) {
		// Sync
		while (!synchronize_map_data()) {}

		if (!game_configured)
		{
			return true;
		}
		if (!g_dedicated_server)
		{
			g_hud->Load();
			g_hud->OnConnected();
		}

		if (game)
		{
			game->OnConnected();
			if (game->Type() != eGameIDSingle)
			{
				m_file_transfer = new file_transfer::client_site();
			}
		}

		g_pGamePersistent->LoadTitle();
		Device.PreCache(60, true, true);
		net_start_result_total = TRUE;

	}
	else {
		net_start_result_total = FALSE;
	}
	if (net_start_result_total)
	{
		NET_Packet		NP;
		NP.w_begin(M_CLIENTREADY);
		Game().local_player->net_Export(NP, TRUE);
		Send(NP, net_flags(TRUE, TRUE));

		if (OnClient() && Server)
		{
			Server->SLS_Clear();
		};
	};
	{
		BulletManager().Clear();
		BulletManager().Load();


		if (net_start_result_total) {
			if (strstr(Core.Params, "-$")) 
			{
				string256				buf, cmd, param;
				sscanf(strstr(Core.Params, "-$") + 2, "%[^ ] %[^ ] ", cmd, param);
				xr_strconcat(buf, cmd, " ", param);
				Console->Execute(buf);
			}
		}
		else {
			Msg("! Failed to start client. Check the connection or level existance.");
			return false;
		}

		if (!g_dedicated_server)
		{
			if (CurrentGameUI())
				CurrentGameUI()->OnConnected();
		}

	}

	/*spawn_item("wpn_svd", Fvector().set(0, 0, 0), 0, 0);
	spawn_item("bread", Fvector().set(0, 0, 0), 0, 0);*/
	return TRUE;
}

void CLevelEditor::LoadEditor(shared_str LevelName)
{
	string_path fn_game;
	// loading sound environment

	xr_string File = LevelName.c_str();

	if (FS.exist(fn_game, "$level$", (File + "\\level.snd_env").c_str()))
	{
		IReader* F = FS.r_open(fn_game);
		::Sound->set_geometry_env(F);
		FS.r_close(F);
	}
	// loading SOM
	if (FS.exist(fn_game, "$level$", (File + "\\level.som").c_str()))
	{
		IReader* F = FS.r_open(fn_game);
		::Sound->set_geometry_som(F);
		FS.r_close(F);
	}
}

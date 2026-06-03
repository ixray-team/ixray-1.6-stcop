#include "stdafx.h"
#include "pch_script.h"
#include "Level.h"
#include "Level_Bullet_Manager.h"
#include "xrServer.h"
#include "xrMessages.h"
#include "game_cl_base.h"
#include "PHCommander.h"
#include "NET_Queue.h"
#include "MainMenu.h"
#include "space_restriction_manager.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "stalker_animation_data_storage.h"
#include "client_spawn_manager.h"
#include "seniority_hierarchy_holder.h"
#include "UIGameCustom.h"
#include "../xrEngine/string_table.h"
#include "file_transfer.h"
#include "ui/UIGameTutorial.h"
#include "ui/UIPdaWnd.h"
#include "../xrNetServer/NET_AuthCheck.h"
#include "Actor.h"
#include "holder_custom.h"

#include "../xrPhysics/PhysicsCommon.h"

// lua to cpp
#include "ScriptsSubsystems/Condlist/script_xr_conditions.h"
#include "ScriptsSubsystems/Condlist/script_xr_effects.h"
#include "ScriptsSubsystems/Condlist/script_xr_logic.h"

constexpr int max_objects_size			= 2*1024;
constexpr int max_objects_size_in_save	= 8*1024;

extern bool	g_b_ClearGameCaptions;

void CLevel::remove_objects	()
{
	PROF_EVENT("remove_objects");
	if (!IsGameTypeSingle()) Msg("CLevel::remove_objects - Start");
	bool						b_stored = psDeviceFlags.test(rsDisableObjectsAsCrows);
	
	int loop = 5;
	while(loop)
	{
		if (OnServer()) 
		{
			R_ASSERT				(Server);
			Server->SLS_Clear		();
		}

		if (OnClient())
			ClearAllObjects			();

		for (int i=0; i<20; ++i) 
		{
			snd_Events.clear		();
			psNET_Flags.set			(NETFLAG_MINIMIZEUPDATES,false);
			// ugly hack for checks that update is twice on frame
			// we need it since we do updates for checking network messages
			++(Device.dwFrame);
			psDeviceFlags.set		(rsDisableObjectsAsCrows,true);
			ClientReceive			();
			ProcessGameEvents		();
			Objects.Update			(false);
			#ifdef DEBUG
			Msg						("Update objects list...");
			#endif // #ifdef DEBUG
			Objects.dump_all_objects();
		}

		if(Objects.o_count()==0)
			break;
		else
		{
			--loop;
			Msg						("Objects removal next loop. Active objects count=%d", Objects.o_count());
		}

	}

	BulletManager().Clear		();
	ph_commander().clear		();
	ph_commander_scripts().clear();

	space_restriction_manager().clear	();

	psDeviceFlags.set			(rsDisableObjectsAsCrows, b_stored);
	g_b_ClearGameCaptions		= true;

	ai().script_engine().collect_all_garbage	();

	stalker_animation_data_storage().clear		();
	
	VERIFY										(Render);
	Render->models_Clear						(false);
	
	Render->clear_static_wallmarks				();

#ifdef DEBUG
	if(!g_dedicated_server)
		if (!client_spawn_manager().registry().empty())
			client_spawn_manager().dump				();
#endif // DEBUG
	if(!g_dedicated_server)
	{
		VERIFY										(client_spawn_manager().registry().empty());
		client_spawn_manager().clear			();
	}

	g_pGamePersistent->destroy_particles		(false);
	::Sound->stop_emitters();

//.	xr_delete									(m_seniority_hierarchy_holder);
//.	m_seniority_hierarchy_holder				= new CSeniorityHierarchyHolder();
	if (!IsGameTypeSingle()) Msg("CLevel::remove_objects - End");
}

#ifdef DEBUG
	extern void	show_animation_stats	();
#endif // DEBUG

extern CUISequencer * g_tutorial;
extern CUISequencer * g_tutorial2;

void CLevel::net_Stop()
{
	Msg("- Disconnect");
	script_client_events.clear();

	if(CurrentGameUI())
	{
		CurrentGameUI()->HideShownDialogs();
		CurrentGameUI()->PdaMenu()->Reset();
	}

	if(g_tutorial && !g_tutorial->Persistent())
		g_tutorial->Stop();

	if(g_tutorial2 && !g_tutorial->Persistent())
		g_tutorial2->Stop();

	bReady						= false;
	m_bGameConfigStarted		= false;

	if (m_file_transfer)
		xr_delete(m_file_transfer);

	if (IsDemoPlay() && m_current_spectator)	//destroying demo spectator ...
	{
		m_current_spectator->setDestroy	(true);
		SetControlEntity(nullptr); //m_current_spectator == CurrentControlEntity()
		m_current_spectator = nullptr;
		
	}else 
	if(IsDemoSave() && !IsDemoInfoSaved())
		SaveDemoInfo();

	remove_objects				();
	
	//WARNING ! remove_objects() uses this flag, so position of this line must e here ..
	game_configured				= false;
	
	IGame_Level::net_Stop		();
	IPureClient::Disconnect		();

	if (Server) 
	{
		Server->Disconnect		();
		xr_delete				(Server);
	}

	ai().script_engine().collect_all_garbage();

	#if defined(IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION) || \
	defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	if (m_pScriptXRCondition)
	{
		m_pScriptXRCondition->destroy();
	}

	if (m_pScriptXREffects)
	{
		m_pScriptXREffects->destroy();
	}

	if (m_pScriptXRParser)
	{
		m_pScriptXRParser->destroy();
	}
	#endif

#ifdef DEBUG
	show_animation_stats		();
#endif // DEBUG
}


void CLevel::ClientSend()
{
	if (IsGameTypeSingle() || OnClient())
	{
		if ( !net_HasBandwidth() ) return;
	};

	NET_Packet				P;
	u32						start	= 0;
	//----------- for E3 -----------------------------
//	if () 
	{
//		if (!(Game().local_player) || Game().local_player->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD)) return;
		if (CurrentControlEntity()) 
		{
			CObject* pObj = CurrentControlEntity();

			if (CActor* act = pObj != nullptr ? pObj->cast_actor() : nullptr)
			{
				if (act->Holder() != nullptr)
				{
					if (CObject* holder = act->Holder()->cast_game_object())
					{
						pObj = holder;
					}
				}
			}

			if (!pObj->getDestroy() && pObj->net_Relevant())
			{				
				P.w_begin		(M_CL_UPDATE);
				
				P << pObj->ID();
				P.w_u32			(0);	//reserved place for client's ping

				pObj->net_Export			(P);

				if (P.B.data.size()>9)				
				{
					if (!OnServer())
					{
						Send(P, net_flags(false));
					}
				}				
			}			
		}		
	};
	if (m_file_transfer)
	{
		m_file_transfer->update_transfer();
		m_file_transfer->stop_obsolete_receivers();
	}
	if (OnClient()) 
	{
		Flush_Send_Buffer();
		return;
	}
	//-------------------------------------------------
	while (true)
	{
		P.w_begin						(M_UPDATE);
		start	= Objects.net_Export	(&P, start, max_objects_size);

		if (P.B.data.size()>2)
		{
			Device.Statistic->TEST3.Begin();
				Send	(P, net_flags(false));
			Device.Statistic->TEST3.End();
		}else
			break;
	}
}

void CLevel::ClientSave()
{
	u32 Iter = 0;
	u32 ChunkID = 0;

	while (Iter < Objects.o_count())
	{
		NET_Packet Packet;

		Packet.w_begin(M_SAVE_PACKET);
		CObject* O = Objects.o_get_by_iterator(Iter);
		Iter++;
		if (!O || O->getDestroy())
			continue;

		CGameObject* GO = O->cast_game_object();
		if (!GO || !GO->net_SaveRelevant())
			continue;

		Packet << GO->ID();
		Packet.w_chunk_open16(ChunkID);
		GO->net_Save(Packet);
		Packet.w_chunk_close16(ChunkID);

		if (Packet.B.data.size() > 2)
		{
			Send(Packet, net_flags(false));
		}
	}
}

extern bool g_SV_Disable_Auth_Check;

void CLevel::Send(NET_Packet& P, u32 dwFlags, u32 dwTimeout)
{
	if (IsDemoPlayStarted() || IsDemoPlayFinished()) return;
	// optimize the case when server located in our memory

	if (psNET_direct_connect)
	{
		ClientID	_clid;
		_clid.set(1);
		Server->OnMessage(P, _clid);
	}
	else if (Server && game_configured && OnServer())
	{
		Server->OnMessageSync(P, Game().local_svdpnid);
	}
	else
	{
		IPureClient::Send(P, dwFlags, dwTimeout);
	}

	if (g_pGameLevel && Level().game && !IsGameTypeSingle() && !g_SV_Disable_Auth_Check) {
		// anti-cheat
		phTimefactor = 1.f;
	}
}

void CLevel::net_Update	()
{
	if(game_configured){
		// If we have enought bandwidth - replicate client data on to server
		Device.Statistic->netClient2.Begin	();
		ClientSend					();
		Device.Statistic->netClient2.End		();
	}
	// If server - perform server-update
	if (Server && OnServer())	{
		Device.Statistic->netServer.Begin();
		Server->Update					();
		Device.Statistic->netServer.End	();
	}
}

struct _NetworkProcessor	: public pureFrame
{
	virtual void	_BCL OnFrame	( )
	{
		if (g_pGameLevel && !Device.Paused() )	g_pGameLevel->net_Update();
	}
}	NET_processor;

pureFrame*	g_pNetProcessor	= &NET_processor;

const int ConnectionTimeOut = 60000; //1 min

bool CLevel::Connect2Server(const char* options)
{
	NET_Packet					P;
	m_bConnectResultReceived	= false	;
	m_bConnectResult			= true	;

	if(!psNET_direct_connect)
	{
		xr_auth_strings_t	tmp_ignore;
		xr_auth_strings_t	tmp_check;
		fill_auth_check_params	(tmp_ignore, tmp_check);
		FS.auth_generate		(tmp_ignore, tmp_check);
	}

	if (!Connect(options))		return	false;
	//---------------------------------------------------------------------------
	if (psNET_direct_connect)
	{
		m_bConnectResultReceived = true;
	}
	else
	{
		size_t EndTime = CPU::GetTickCount() + ConnectionTimeOut;
		while (!HasSessionName())
		{
			Sleep(5);
			size_t CurTime = CPU::GetTickCount();
			if (CurTime > EndTime || net_isFails_Connect())
			{
				OnConnectRejected();
				Disconnect();
				return	false;
			}
		}

		EndTime = CPU::GetTickCount() + ConnectionTimeOut;
		while (!m_bConnectResultReceived)
		{
			ClientReceive();
			Sleep(5);
			if (Server)
				Server->Update();

			size_t CurTime = CPU::GetTickCount();
			if (CurTime > EndTime)
			{
				NET_Packet	P;
				P.write_start();
				P.r_pos = 0;

				P.w_u8(0);
				P.w_u8(0);
				P.w_stringZ("Data verification failed. Cheater?");

				OnConnectResult(&P);
			}

			if (net_isFails_Connect())
			{
				OnConnectRejected();
				Disconnect();
				return	false;
			}
		}
	}

	Msg							("%c client : connection %s - <%s>", m_bConnectResult ?'*':'!', m_bConnectResult ? "accepted" : "rejected", m_sConnectResult.c_str());
	if		(!m_bConnectResult) 
	{
		if(Server)
		{
			Server->Disconnect		();
			xr_delete				(Server);
		}
		OnConnectRejected			();	
		Disconnect					();
		return false		;
	};

	
	if(psNET_direct_connect)
		net_Syncronised = true;
	else
		net_Syncronize	();

	while (!net_IsSyncronised()) {
		Sleep(1);
		if (net_Disconnected)
		{
			OnConnectRejected	();	
			Disconnect			();
			return false;
		}
	};

	//---------------------------------------------------------------------------
	//P.w_begin	(M_CLIENT_REQUEST_CONNECTION_DATA);
	//Send		(P, net_flags(true, true, true, true));
	//---------------------------------------------------------------------------
	return true;
};

void CLevel::OnBuildVersionChallenge()
{
	NET_Packet P;
	P.w_begin				(M_CL_AUTH);
#ifdef USE_DEBUG_AUTH
	u64 auth = MP_DEBUG_AUTH;
	Msg("* Sending auth value ...");
#else
	u64 auth = FS.auth_get();
#endif //#ifdef DEBUG
	P.w_u64					(auth);
	SecureSend				(P, net_flags(true, true, true, true));
};

void CLevel::OnConnectResult(NET_Packet*	P)
{
	// multiple results can be sent during connection they should be "AND-ed"
	m_bConnectResultReceived	= true;
	u8	result					= P->r_u8();
	u8  res1					= P->r_u8();
	string512 ResultStr;	
	P->r_stringZ_s				(ResultStr);
	ClientID tmp_client_id;
	P->r_clientID				(tmp_client_id);
	SetClientID					(tmp_client_id);
	if (!result)				
	{
		m_bConnectResult	= false			;	
		switch (res1)
		{
		case ecr_data_verification_failed:		//Standart error
			{
				if (strstr(ResultStr, "Data verification failed. Cheater?"))
					MainMenu()->SetErrorDialog(CMainMenu::ErrDifferentVersion);
			}break;	
		case ecr_password_verification_failed:		//login+password
			{
				MainMenu()->SetErrorDialog(CMainMenu::ErrInvalidPassword);
			}break;
		case ecr_have_been_banned:
			{
				if (!xr_strlen(ResultStr))
				{
					MainMenu()->OnSessionTerminate(
						g_pStringTable->translate("st_you_have_been_banned").c_str()
					);
				} else
				{
					MainMenu()->OnSessionTerminate(
						g_pStringTable->translate(ResultStr).c_str()
					);
				}
			}break;
		case ecr_profile_error:
			{
				if (!xr_strlen(ResultStr))
				{
					MainMenu()->OnSessionTerminate(
						g_pStringTable->translate("st_profile_error").c_str()
					);
				} else
				{
					MainMenu()->OnSessionTerminate(
						g_pStringTable->translate(ResultStr).c_str()
					);
				}
			}
		}
	};	
	m_sConnectResult			= ResultStr;
	if (IsDemoSave() && result)
	{
		P->r_u8(); //server client or not
		shared_str server_options;
		P->r_stringZ(server_options);
		StartSaveDemo(server_options);
	}
};

void			CLevel::ClearAllObjects				()
{
	u32 CLObjNum = Level().Objects.o_count();

	bool ParentFound = true;
	
	while (ParentFound)
	{	
		ParentFound = false;
		for (u32 i=0; i<CLObjNum; i++)
		{
			CObject* pObj = Level().Objects.o_get_by_iterator(i);
			if (!pObj->H_Parent()) continue;
			//-----------------------------------------------------------
			NET_Packet			GEN;
			GEN.w_begin			(M_EVENT);
			//---------------------------------------------		
			GEN.w_u32			(Level().timeServer());
			GEN.w_u16			(GE_OWNERSHIP_REJECT);
			GEN << pObj->H_Parent()->ID() << pObj->ID();
			game_events->insert	(GEN);
			if (g_bDebugEvents)	ProcessGameEvents();
			//-------------------------------------------------------------
			ParentFound = true;
			//-------------------------------------------------------------
#ifdef DEBUG
			Msg ("Rejection of %s[%d] from %s[%d]", *(pObj->cNameSect()), pObj->ID(), *(pObj->H_Parent()->cNameSect()), pObj->H_Parent()->ID());
#endif
		};
		ProcessGameEvents();
	};

	CLObjNum = Level().Objects.o_count();

	for (u32 i=0; i<CLObjNum; i++)
	{
		CObject* pObj = Level().Objects.o_get_by_iterator(i);
		if (pObj->H_Parent() != nullptr)
		{
			if (IsGameTypeSingle())
			{
				FATAL("pObj->H_Parent()==nullptr");
			} else
			{
				Msg("! ERROR: object's parent is not nullptr");
			}
		}
		
		//-----------------------------------------------------------
		NET_Packet			GEN;
		GEN.w_begin			(M_EVENT);
		//---------------------------------------------		
		GEN.w_u32			(Level().timeServer());
		GEN.w_u16			(GE_DESTROY);
		GEN << pObj->ID();
		game_events->insert	(GEN);
		if (g_bDebugEvents)	ProcessGameEvents();
		//-------------------------------------------------------------
		ParentFound = true;
		//-------------------------------------------------------------
#ifdef DEBUG
		Msg ("Destruction of %s[%d]", *(pObj->cNameSect()), pObj->ID());
#endif
	};
	ProcessGameEvents();
};

void				CLevel::OnInvalidHost			()
{
	IPureClient::OnInvalidHost();
	if (MainMenu()->GetErrorDialogType() == CMainMenu::ErrNoError)
		MainMenu()->SetErrorDialog(CMainMenu::ErrInvalidHost);
};

void				CLevel::OnInvalidPassword		()
{
	IPureClient::OnInvalidPassword();
	MainMenu()->SetErrorDialog(CMainMenu::ErrInvalidPassword);
};

void				CLevel::OnSessionFull			()
{
	IPureClient::OnSessionFull();
	if (MainMenu()->GetErrorDialogType() == CMainMenu::ErrNoError)
		MainMenu()->SetErrorDialog(CMainMenu::ErrSessionFull);
}

void				CLevel::OnConnectRejected		()
{
	IPureClient::OnConnectRejected();

//	if (MainMenu()->GetErrorDialogType() != CMainMenu::ErrNoError)
//		MainMenu()->SetErrorDialog(CMainMenu::ErrServerReject);
}

CScriptXRConditionsStorage* CLevel::getScriptXRConditions(void) const
{
	return m_pScriptXRCondition;
}

CScriptXRParser* CLevel::getScriptXRParser(void) const
{
	return m_pScriptXRParser;
}

void				CLevel::net_OnChangeSelfName			(NET_Packet* P)
{
	if (!P) return;
	string64 NewName			;
	P->r_stringZ(NewName)		;
	if (!strstr(*m_caClientOptions, "/name="))
	{
		string1024 tmpstr;
		xr_strcpy(tmpstr, *m_caClientOptions);
		xr_strcat(tmpstr, "/name=");
		xr_strcat(tmpstr, NewName);
		m_caClientOptions = tmpstr;
	}
	else
	{
		string1024 tmpstr;
		xr_strcpy(tmpstr, *m_caClientOptions);
		*(strstr(tmpstr, "name=")+5) = 0;
		xr_strcat(tmpstr, NewName);
		const char* ptmp = strstr(strstr(*m_caClientOptions, "name="), "/");
		if (ptmp)
			xr_strcat(tmpstr, ptmp);
		m_caClientOptions = tmpstr;
	}
}

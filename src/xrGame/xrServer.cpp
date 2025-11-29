// xrServer.cpp: implementation of the xrServer class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "xrServer.h"
#include "xrMessages.h"
#include "xrServer_Objects_ALife_All.h"
#include "Level.h"
#include "game_cl_base.h"
#include "game_sv_mp.h"
#include "game_cl_base_weapon_usage_statistic.h"
#include "ai_space.h"
#include "object_broker.h"
#include "file_transfer.h"
#include "screenshot_server.h"
#include "xrServer_info.h"
#include "xrServer_Objects.h"

#pragma warning(push)
#pragma warning(disable:4995)
#include <functional>

#include "SaveObjectHelpers.h"
#pragma warning(pop)
#include "alife_simulator.h"
#include "alife_object_registry.h"
#include "game_sv_single.h"

#include "ui/UIInventoryUtilities.h"

#include "FreeMP/game_sv_freemp.h"

#include "../xrNetServer/NET_AuthCheck.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/x_ray.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/string_table.h"

#ifdef DEBUG_DRAW
#	define USE_DESIGNER_KEY
#endif

#ifdef USE_DESIGNER_KEY
#	include "xrServer_Objects_ALife_Monsters.h"
#endif


xrClientData::xrClientData()
	: IClient(Device.GetTimerGlobal())
{
	ps = nullptr;
	Clear();
}

void xrClientData::Clear()
{
	owner = nullptr;
	net_Ready = false;
	net_Accepted = false;
	net_PassUpdates = true;
	m_ping_warn.m_maxPingWarnings = 0;
	m_ping_warn.m_dwLastMaxPingWarningTime = 0;
	m_admin_rights.m_has_admin_rights = false;
}

xrClientData::~xrClientData()
{
	xr_delete(ps);
}

xrServer::xrServer()
	: IPureServer(Device.GetTimerGlobal(), g_dedicated_server)
{
	m_file_transfers = nullptr;
	m_aDelayedPackets.clear();
	m_server_logo = nullptr;
	m_server_rules = nullptr;
	m_last_updates_size = 0;
	m_last_update_time = 0;
}

xrServer::~xrServer()
{
	struct ClientDestroyer
	{
		static bool true_generator(IClient*)
		{
			return true;
		}
	};

	IClient* TempClient = net_players.GetFoundClient(&ClientDestroyer::true_generator);
	while (TempClient)
	{
		client_Destroy(TempClient);
		TempClient = net_players.GetFoundClient(&ClientDestroyer::true_generator);
	}

	m_aDelayedPackets.clear();
	entities.clear();
	delete_data(m_info_uploaders);
	xr_delete(m_server_logo);
	xr_delete(m_server_rules);
}


//--------------------------------------------------------------------

CSE_Abstract* xrServer::ID_to_entity(ALife::_OBJECT_ID ID)
{
	// #pragma todo("??? to all : ID_to_entity - must be replaced to 'game->entity_from_eid()'")	
	if (ALife::INVALID_OBJECT_ID==ID)
	{
		return nullptr;
	}
	xrS_entities::iterator I = entities.find(ID);
	if (entities.end()!=I)
	{
		return I->second;
	}
	return nullptr;
}

IClient* xrServer::client_Create()
{
	return new xrClientData();
}

IClient* xrServer::client_Find_Get(ClientID ID)
{
	DWORD dwPort = 0;
	ip_address tmp_ip_address;


	if (!psNET_direct_connect)
	{
		GetClientAddress(ID, tmp_ip_address, &dwPort);
	}
	else
	{
		tmp_ip_address.set("127.0.0.1");
	}

	IClient* newCL = client_Create();
	newCL->ID = ID;
	if (!psNET_direct_connect)
	{
		newCL->m_cAddress = tmp_ip_address;
		newCL->m_dwPort = dwPort;
	}

	newCL->server = this;
	net_players.AddNewClient(newCL);

	return newCL;
};

u32 g_sv_Client_Reconnect_Time = 3;

void xrServer::client_Destroy(IClient* C)
{
	// Delete assosiated entity
	auto alife_client = net_players.FindAndEraseClient(
		[C](const IClient* client)
		{
			return client == C;
		}
	);
	// VERIFY(alife_client);
	if (alife_client)
	{
		CSE_Abstract* pOwner = static_cast<xrClientData*>(alife_client)->owner;
		CSE_Spectator* pS = smart_cast<CSE_Spectator*>(pOwner);
		if (pS)
		{
			NET_Packet P;
			P.w_begin(M_EVENT);
			P.w_u32(Level().timeServer());
			P.w_u16(GE_DESTROY);
			P.w_u16(pS->ID);
			SendBroadcast(C->ID, P, net_flags(true, true));
		};

		DelayedPacket pp;
		pp.SenderID = alife_client->ID;
		xr_deque<DelayedPacket>::iterator it;
		do
		{
			it = std::find(m_aDelayedPackets.begin(), m_aDelayedPackets.end(), pp);
			if (it != m_aDelayedPackets.end())
			{
				m_aDelayedPackets.erase(it);
				Msg("removing packet from delayed event storage");
			}
			else
			{
				break;
			}
		} while (true);

		if (pOwner)
		{
			game->CleanDelayedEventFor(pOwner->ID);
		}

		xrClientData* xr_client = static_cast<xrClientData*>(alife_client);
		m_disconnected_clients.Add(xr_client);
	}
}

void xrServer::GetPooledState(xrClientData* xrCL)
{
	xrClientData* pooled_client = m_disconnected_clients.Get(xrCL);
	if (!pooled_client)
	{
		return;
	}

	NET_Packet tmp_packet;
	u16 tmp_fake;
	tmp_packet.w_begin(M_SPAWN);
	pooled_client->ps->net_Export(tmp_packet, true);
	tmp_packet.r_begin(tmp_fake);
	xrCL->ps->net_Import(tmp_packet);
	xrCL->ps->flags__ = 0;
	xrCL->flags.bReconnect = true;
	xr_delete(pooled_client);
}

//--------------------------------------------------------------------
int g_Dump_Update_Write = 0;

#ifdef DEBUG
int g_sv_SendUpdate = 0;
#endif

void xrServer::Update()
{
	if (Level().IsDemoPlayStarted() || Level().IsDemoPlayFinished())
	{
		return; // diabling server when demo is playing
	}

	ProcessMessagesQueue();

	NET_Packet Packet;

#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
	VERIFY						(verify_entities());
#	endif
#endif

	ProceedDelayedPackets();
	// game update
	game->ProcessDelayedEvent();
	game->Update();

	// spawn queue
	u32 svT = Device.TimerAsync();
	while (!(q_respawn.empty() || (svT<q_respawn.begin()->timestamp)))
	{
		// get
		svs_respawn	R = *q_respawn.begin();
		q_respawn.erase(q_respawn.begin());

		// 
		CSE_Abstract* E	= ID_to_entity(R.phantom);
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(Packet, *E);
		} else
		{
			E->Spawn_Write(Packet,false);
		}
		u16 ID;
		Packet.r_begin(ID);
		R_ASSERT(M_SPAWN==ID);
		ClientID clientID; 
		clientID.set(ALife::INVALID_OBJECT_ID);
		Process_spawn(Packet,clientID);
	}

	SendUpdatesToAll();

	if (game->sv_force_sync)
	{
		Perform_game_export();
	}

#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
	VERIFY(verify_entities());
#	endif
#endif
	//-----------------------------------------------------

	PerformCheckClientsForMaxPing();
	Flush_Clients_Buffers();

	if (0 == (Device.dwFrame % 100)) // once per 100 frames
	{
		UpdateBannedList();
	}
}

void xrServer::SendGameUpdateTo(IClient* client)
{
	xrClientData* xr_client = static_cast<xrClientData*>(client);
	VERIFY(xr_client);
	if (!xr_client->net_Ready)
	{
		return;
	}

	if (!HasBandwidth(client)
#ifdef DEBUG
		&& !g_sv_SendUpdate
#endif
	)
	{
		return;
	}

	NET_Packet Packet;
	u16 PacketType = M_UPDATE;
	Packet.w_begin(PacketType);
	game->net_Export_Update(Packet, xr_client->ID, xr_client->ID);
	SendTo(xr_client->ID, Packet, net_flags(false, true));
}

void xrServer::MakeUpdatePackets()
{
	NET_Packet tmpPacket;
	u32 position;

	m_update_packets.clear();
	m_updator.begin_updates();

	xrS_entities::iterator I = entities.begin();
	xrS_entities::iterator E = entities.end();
	for (; I != E; ++I)
	{
		CSE_Abstract& Test = *(I->second);

		if (0 == Test.owner || !Test.net_Ready)
		{
			continue;
		}

		if (Test.s_flags.is(M_SPAWN_OBJECT_PHANTOM) || !Test.Net_Relevant())
		{
			continue;
		}

		tmpPacket.B.data.clear();

		// write specific data
		{
			tmpPacket.w_u16(Test.ID);
			tmpPacket.w_chunk_open8(position);
			Test.UPDATE_Write(tmpPacket);
			if (g_pGamePersistent->GameType() == eGameIDFreeMP)
			{
				Test.SyncWrite(tmpPacket);
			}

			u32 ObjectSize = u32(tmpPacket.w_tell() - position) - sizeof(u8);
			tmpPacket.w_chunk_close8(position);

			if (ObjectSize == 0)
			{
				continue;
			}
#ifdef DEBUG
			if (g_Dump_Update_Write)
			{
				Msg("* %s : %d", Test.name(), ObjectSize);
			}
#endif
			UpdatePacket* NewPacket = &m_update_packets.emplace_back(UpdatePacket());
			NewPacket->Entity = I->second;
			std::memcpy(&(NewPacket->Packet), &tmpPacket, sizeof(NET_Packet));
		}
	}

	m_updator.end_updates(m_update_begin, m_update_end);
}

void xrServer::SendUpdatePacketsToAll()
{
	struct ClientExcluderPredicate
	{
		ClientID id_to_exclude;
		ClientExcluderPredicate(ClientID exclude)
			: id_to_exclude(exclude)
		{
		}
		bool operator()(IClient* client)
		{
			xrClientData* TempClient = static_cast<xrClientData*>(client);
			if (client->ID == id_to_exclude)
			{
				return false;
			}
			if (!client->flags.bConnected)
			{
				return false;
			}
			if (!TempClient->net_Accepted)
			{
				return false;
			}
			return true;
		}
	};

	struct SenderFunctor
	{
		xrServer* m_owner;
		u32 m_dwFlags;
		xr_vector<UpdatePacket>& m_packets;

		server_updates_compressor* m_updator;
		update_iterator_t m_update_begin;
		update_iterator_t m_update_end;

		SenderFunctor(xrServer* owner, xr_vector<UpdatePacket>& packets, server_updates_compressor* updator, u32 dwFlags)
			: m_owner(owner), m_packets(packets), m_updator(updator), m_dwFlags(dwFlags)
		{
		}
		void operator()(IClient* client)
		{
			auto I = m_packets.begin();
			auto E = m_packets.end();

			xrClientData* CL = static_cast<xrClientData*>(client);

			bool need_to_update_15 = Device.dwTimeGlobal - CL->m_last_update_time_15 >= u32(1000 / 15); // 15 per sec
			bool need_to_update_10 = Device.dwTimeGlobal - CL->m_last_update_time_10 >= u32(1000 / 10); // 10 per sec
			bool need_to_update_5 = Device.dwTimeGlobal - CL->m_last_update_time_5 >= u32(1000 / 5);	// 5 per sec
			bool need_to_update_1 = Device.dwTimeGlobal - CL->m_last_update_time_1 >= u32(1000);		// 1 per sec
			bool need_to_update_05 = Device.dwTimeGlobal - CL->m_last_update_time_05 >= u32(2000);		// 1 per 2 sec

			constexpr float distance_30 = 30.f * 30.f;
			constexpr float distance_50 = 50.f * 50.f;
			constexpr float distance_60 = 60.f * 60.f;
			constexpr float distance_100 = 100.f * 100.f;
			constexpr float distance_200 = 200.f * 200.f;
			constexpr float distance_300 = 300.f * 300.f;

			// create big net packets & compress (if enabled)
			m_updator->begin_updates();
			for (; I != E; ++I)
			{
				CSE_Abstract* owner = CL->owner;
				if (!owner)
				{
					continue;
				}

				CSE_Abstract* entity = I->Entity;
				NET_Packet& packet = I->Packet;

				float distance = 0.f;

				CSE_Abstract* parent = m_owner->ID_to_entity(entity->ID_Parent);

				bool has_parent = !!parent;
				if (!has_parent)
				{
					distance = owner->Position().distance_to_sqr(entity->Position());
				}
				else
				{
					distance = owner->Position().distance_to_sqr(parent->Position());
				}

				if (entity->cast_human_abstract() || entity->cast_monster_abstract())
				{
					// MONSTERS AND HUMANS
					// 0 - 50 : 30 per sec
					// 50 - 100 : 15 per sec
					// 100 - 200 : 10 per sec
					// 200 - 300 : 5 per sec
					// 300 and more : 1 per 2 sec

					bool NeedUpdate = distance <= distance_50;
					NeedUpdate = NeedUpdate || (need_to_update_15 && distance <= distance_100);
					NeedUpdate = NeedUpdate || (need_to_update_10 && distance <= distance_200);
					NeedUpdate = NeedUpdate || (need_to_update_5 && distance <= distance_300);
					NeedUpdate = NeedUpdate || need_to_update_05;

					if (NeedUpdate)
					{
						m_updator->write_update_for(entity->ID, packet);
					}
				}
				else if (smart_cast<CSE_ActorMP*>(entity))
				{
					// ACTORS
					// 0 - 200 : 30 per second
					// 200 - 300 : 10 per second
					// 300 and more : 1 per sec

					bool NeedUpdate = distance <= distance_200;
					NeedUpdate = NeedUpdate || (need_to_update_10 && distance <= distance_300);
					NeedUpdate = NeedUpdate || need_to_update_1;

					if (NeedUpdate)
					{
						m_updator->write_update_for(entity->ID, packet);
					}
				}
				else if (smart_cast<CSE_ALifeItemArtefact*>(entity))
				{
					// ARTEFACTS
					// 0 - 30 : 10 per second
					// 30 - 60 : 5 per second
					// 60 and more : 1 per 2 sec
					bool NeedUpdate = need_to_update_10 && distance <= distance_30;
					NeedUpdate = NeedUpdate || (need_to_update_5 && distance <= distance_60);
					NeedUpdate = NeedUpdate || need_to_update_05;

					if (NeedUpdate)
					{
						m_updator->write_update_for(entity->ID, packet);
					}
				}
				else if (entity->cast_inventory_item())
				{
					if (has_parent)
					{
						// Inventory items with parent
						// 0 - 200 : 30 per second
						// 200 - 300 : 10 per second
						// 300 and more : 1 per sec

						bool NeedUpdate = distance <= distance_200;
						NeedUpdate = NeedUpdate || (need_to_update_10 && distance <= distance_300);
						NeedUpdate = NeedUpdate || need_to_update_1;

						if (NeedUpdate)
						{
							m_updator->write_update_for(entity->ID, packet);
						}
					}
					else
					{
						m_updator->write_update_for(entity->ID, packet);
					}
				}
				else
				{
					m_updator->write_update_for(entity->ID, packet);
				}
			}

			CL->m_last_update_time_15 = need_to_update_15 ? Device.dwTimeGlobal : CL->m_last_update_time_15;
			CL->m_last_update_time_10 = need_to_update_10 ? Device.dwTimeGlobal : CL->m_last_update_time_10;
			CL->m_last_update_time_5 = need_to_update_5 ? Device.dwTimeGlobal : CL->m_last_update_time_5;
			CL->m_last_update_time_1 = need_to_update_1 ? Device.dwTimeGlobal : CL->m_last_update_time_1;
			CL->m_last_update_time_05 = need_to_update_05 ? Device.dwTimeGlobal : CL->m_last_update_time_05;

			m_updator->end_updates(m_update_begin, m_update_end);

			// send packets to client
			for (update_iterator_t i = m_update_begin; i != m_update_end; ++i)
			{
				NET_Packet& P = **i;
				if (P.B.data.size() > 2)
				{
					m_owner->SendTo_LL(client->ID, P.B.data.data(), P.B.data.size(), m_dwFlags);
				}
			}
		}
	};

	if (GetServerClient() == nullptr)
	{
		return;
	}

	SenderFunctor temp_functor(this, m_update_packets, &m_updator, net_flags(false, true));
	net_players.ForFoundClientsDo(ClientExcluderPredicate(GetServerClient()->ID), temp_functor);
}

void xrServer::SendUpdatesToAll()
{
	if (IsGameTypeSingle())
	{
		return;
	}

	KickCheaters();


	// sending game_update
	xr_delegate<void(IClient*)> sendtofd;
	sendtofd.bind(this, &xrServer::SendGameUpdateTo);
	ForEachClientDoSender(sendtofd);

	if ((Device.dwTimeGlobal - m_last_update_time) >= u32(1000 / psNET_ServerUpdate))
	{
		MakeUpdatePackets();
		SendUpdatePacketsToAll();

#ifdef DEBUG
		g_sv_SendUpdate = 0;
#endif			
		if (game->sv_force_sync)	
		{
			Perform_game_export();
		}
#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
		VERIFY(verify_entities());
#	endif
#endif
		m_last_update_time = Device.dwTimeGlobal;
	}
	if (m_file_transfers)
	{
		m_file_transfers->update_transfer();
		m_file_transfers->stop_obsolete_receivers();
	}
}

xr_vector<shared_str> _tmp_log;
void console_log_cb(const char* text)
{
	_tmp_log.push_back(text);
}

u32 xrServer::OnDelayedMessage(NET_Packet& P, ClientID sender) // Non-Zero means broadcasting with "flags" as returned
{
	u16 type;
	P.r_begin(type);

#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
	VERIFY							(verify_entities());
#	endif
#endif
	xrClientData* CL = ID_to_client(sender);

	switch (type)
	{
		case M_CLIENT_REQUEST_CONNECTION_DATA:
		{
			IClient* TempClient = net_players.GetFoundClient(
				ClientIdSearchPredicate(sender)
			);
			VERIFY(TempClient);
			OnCL_Connected(TempClient);
		}
		break;
		case M_REMOTE_CONTROL_CMD:
		{
#ifdef DEBUG
			bool bCan = true;
#else
			bool bCan = CL->m_admin_rights.m_has_admin_rights;
#endif
			if (bCan)
			{
				string1024 buff;
				P.r_stringZ(buff);
				Msg("* Radmin [%s] is running command: %s", CL->ps->getName(), buff);
				xrLogger::AddLogCallback(console_log_cb);
				_tmp_log.clear();
				string512 result_command;
				string64 tmp_number_str;
				xr_sprintf(tmp_number_str, " raid:%u", CL->ID.value());
				xr_strconcat(result_command, buff, tmp_number_str);
				Console->Execute(result_command);
				xrLogger::RemoveLogCallback(console_log_cb);

				NET_Packet P_answ;
				for (u32 i = 0; i < _tmp_log.size(); ++i)
				{
					P_answ.w_begin(M_REMOTE_CONTROL_CMD);
					P_answ.w_stringZ(_tmp_log[i]);
					SendTo(sender, P_answ, net_flags(true, true));
				}
			}
			else
			{
				NET_Packet P_answ;
				P_answ.w_begin(M_REMOTE_CONTROL_CMD);
				P_answ.w_stringZ("you dont have admin rights");
				SendTo(sender, P_answ, net_flags(true, true));
			}
		}
		break;
		case M_FILE_TRANSFER:
		{
			m_file_transfers->on_message(&P, sender);
		}
		break;
	}
#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
	VERIFY							(verify_entities());
#	endif
#endif

	return 0;
}

u32 xrServer::OnMessageSync(NET_Packet& P, ClientID sender)
{
	csMessage.Enter();
	u32 ret = OnMessage(P, sender);
	csMessage.Leave();
	return ret;
}

extern float g_fCatchObjectTime;
u32 xrServer::OnMessage(NET_Packet& P, ClientID sender) // Non-Zero means broadcasting with "flags" as returned
{
	u16 type;
	P.r_begin(type);
#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
	VERIFY(verify_entities());
#	endif
#endif
	xrClientData* CL = ID_to_client(sender);

	switch (type)
	{
		case M_UPDATE:
			{
				Process_update(P,sender);						// No broadcast
			}break;
		case M_SPAWN:
		case M_SPAWN_LOCAL:
		{
			if (CL && CL->flags.bLocal)
			{
				Process_spawn(P, sender);
			}
		}
		break;
		case M_EVENT:
		{
			Process_event(P, sender);
		}
		break;
		case M_EVENT_PACK:
		{
			NET_Packet tmpP;
			while (!P.r_eof())
			{
				tmpP.B.data.resize(P.r_u8());
				P.r(tmpP.B.data.data(), tmpP.B.data.size());

				OnMessage(tmpP, sender);
			};
		}
		break;
		case M_CL_UPDATE:
		{
			xrClientData* CL_ = ID_to_client(sender);
			if (!CL_)
			{
				break;
			}
			CL_->net_Ready = true;

			if (!CL_->net_PassUpdates)
			{
				break;
			}
			//-------------------------------------------------------------------
			u32 ClientPing = CL_->stats.getPing();
			P.w_seek(P.r_tell() + 2, &ClientPing, 4);
			//-------------------------------------------------------------------
			if (SV_Client)
			{
				SendTo(SV_Client->ID, P, net_flags(true, true));
			}
		}
		break;
		case M_MOVE_PLAYERS_RESPOND:
		{
			xrClientData* CL_ = ID_to_client(sender);
			if (!CL_)
			{
				break;
			}
			CL_->net_Ready = true;
			CL_->net_PassUpdates = true;
		}
		break;
		//-------------------------------------------------------------------
		case M_CL_INPUT:
		{
			xrClientData* CL_ = ID_to_client(sender);
			if (CL_)
			{
				CL_->net_Ready = true;
			}
			if (SV_Client)
			{
				SendTo(SV_Client->ID, P, net_flags(true, true));
			}
		}
		break;
		case M_GAMEMESSAGE:
		{
			SendBroadcast(BroadcastCID, P, net_flags(true, true));
		}
		break;
		case M_CLIENTREADY:
		{
			game->OnPlayerConnectFinished(sender);
		}
		break;
		case M_SWITCH_DISTANCE:
		{
			game->switch_distance(P, sender);
		}
		break;
		case M_CHANGE_LEVEL:
		{
			if (game->change_level(P, sender))
			{
				SendBroadcast(BroadcastCID, P, net_flags(true, true));
			}
		}
		break;
		case M_SAVE_GAME:
		{
			game->save_game(P, sender);
		}
		break;
		case M_LOAD_GAME:
		{
			game->load_game(P, sender);
			SendBroadcast(BroadcastCID, P, net_flags(true, true));
		}
		break;
		case M_RELOAD_GAME:
		{
			SendBroadcast(BroadcastCID, P, net_flags(true, true));
		}
		break;
		case M_SAVE_PACKET:
		{
			Process_save(P, sender);
		}
		break;
		case M_CLIENT_REQUEST_CONNECTION_DATA:
		{
			AddDelayedPacket(P, sender);
		}
		break;
		case M_CHAT_MESSAGE:
		{
			xrClientData* l_pC = ID_to_client(sender);
			OnChatMessage(&P, l_pC);
		}
		break;
		case M_VOICE_MESSAGE:
		{
			OnVoiceMessage(P, sender);
		}
		break;
		case M_SV_MAP_NAME:
		{
			xrClientData* l_pC = ID_to_client(sender);
			OnProcessClientMapData(P, l_pC->ID);
		}
		break;
		case M_SV_DIGEST:
		{
			R_ASSERT(CL);
			ProcessClientDigest(CL, &P);
		}
		break;
		case M_CHANGE_LEVEL_GAME:
		{
			ClientID CID;
			CID.set(0xffffffff);
			SendBroadcast(CID, P, net_flags(true, true));
		}
		break;
		case M_CL_AUTH:
		{
			game->AddDelayedEvent(P, GAME_EVENT_PLAYER_AUTH, 0, sender);
		}
		break;
		case M_CREATE_PLAYER_STATE:
		{
			game->AddDelayedEvent(P, GAME_EVENT_CREATE_PLAYER_STATE, 0, sender);
		}
		break;
		case M_STATISTIC_UPDATE:
		{
			SendBroadcast(BroadcastCID, P, net_flags(true, true));
		}
		break;
		case M_STATISTIC_UPDATE_RESPOND:
		{
			// client method for collecting statistics are called from two places : 1 - this, 2 - game_sv_mp::WritePlayerStats
			if (!IsGameTypeSingle())
			{
				game_sv_mp* my_game = static_cast<game_sv_mp*>(game);
				if (CL)
				{
					my_game->m_async_stats.set_responded(CL->ID);
					if (static_cast<IClient*>(CL) != GetServerClient())
					{
						game_PlayerState* tmp_ps = CL->ps;
						u32 tmp_pid = tmp_ps != nullptr ? tmp_ps->m_account.profile_id() : 0;
						Game().m_WeaponUsageStatistic->OnUpdateRespond(&P, CL->m_cdkey_digest, tmp_pid);
					}
				}
				else
				{
					Msg("! ERROR: SV: update respond received from unknown sender");
				}
			}
		}
		break;
		case M_PLAYER_FIRE:
		{
			if (game)
			{
				game->OnPlayerFire(sender, P);
			}
		}
		break;
		case M_REMOTE_CONTROL_AUTH:
		{
			string512 reason;
			shared_str user;
			shared_str pass;
			P.r_stringZ(user);
			if (0 == _stricmp(user.c_str(), "logoff"))
			{
				CL->m_admin_rights.m_has_admin_rights = false;
				if (CL->ps)
				{
					CL->ps->resetFlag(GAME_PLAYER_HAS_ADMIN_RIGHTS);
				}
				xr_strcpy(reason, "logged off");
				Msg("# Remote administrator logged off.");
			}
			else
			{
				P.r_stringZ(pass);
				bool res = CheckAdminRights(user, pass, reason);
				if (res)
				{
					CL->m_admin_rights.m_has_admin_rights = true;
					CL->m_admin_rights.m_dwLoginTime = Device.dwTimeGlobal;
					if (CL->ps)
					{
						CL->ps->setFlag(GAME_PLAYER_HAS_ADMIN_RIGHTS);
					}
					Msg("# User [%s] logged as remote administrator.", user.c_str());
				}
				else
				{
					Msg("# User [%s] tried to login as remote administrator. Access denied.", user.c_str());
				}
			}
			NET_Packet P_answ;
			P_answ.w_begin(M_REMOTE_CONTROL_AUTH);
			P_answ.w_stringZ(reason);
			SendTo(CL->ID, P_answ, net_flags(true, true));
		}
		break;

		case M_REMOTE_CONTROL_CMD:
		{
			AddDelayedPacket(P, sender);
		}
		break;
		case M_BATTLEYE:
		{
		}
		break;
		case M_FILE_TRANSFER:
		{
			AddDelayedPacket(P, sender);
		}
		break;
		case M_SECURE_KEY_SYNC:
		{
			PerformSecretKeysSyncAck(CL, P);
		}
		break;
		case M_SECURE_MESSAGE:
		{
			OnSecureMessage(P, CL);
		}
		break;
		case M_SCRIPT_EVENT:
		{
			OnScriptEvent(P, sender);
		}
		break;
	}
#ifdef DEBUG
#	ifdef SLOW_VERIFY_ENTITIES
	VERIFY(verify_entities());
#	endif
#endif
	return IPureServer::OnMessage(P, sender);
}

bool xrServer::CheckAdminRights(const shared_str& user, const shared_str& pass, string512& reason)
{
	bool res = false;
	string_path fn;
	FS.update_path(fn, "$app_data_root$", "radmins.ltx");
	if (FS.exist(fn))
	{
		CInifile ini(fn);
		if (ini.line_exist("radmins", user.c_str()))
		{
			if (ini.r_string("radmins", user.c_str()) == pass)
			{
				xr_strcpy(reason, sizeof(reason), "Access permitted.");
				res = true;
			}
			else
			{
				xr_strcpy(reason, sizeof(reason), "Access denied. Wrong password.");
			}
		}
		else
		{
			xr_strcpy(reason, sizeof(reason), "Access denied. No such user.");
		}
	}
	else
	{
		xr_strcpy(reason, sizeof(reason), "Access denied.");
	}

	return res;
}

void xrServer::SendTo_LL(ClientID ID, void* data, u32 size, u32 dwFlags, u32 dwTimeout)
{
	if ((SV_Client && SV_Client->ID == ID) || (psNET_direct_connect))
	{
		// optimize local traffic
		Level().OnMessage(data, size);
	}
	else
	{
		IClient* pClient = ID_to_client(ID);
		VERIFY2(pClient && pClient->flags.bConnected, "trying to send packet to disconnected client");
		if (!pClient || !pClient->flags.bConnected)
		{
			return;
		}

		IPureServer::SendTo_Buf(ID, data, size, dwFlags, dwTimeout);
	}
}
void xrServer::SendBroadcast(ClientID exclude, NET_Packet& P, u32 dwFlags)
{
	struct ClientExcluderPredicate
	{
		ClientID id_to_exclude;
		ClientExcluderPredicate(ClientID exclude)
			: id_to_exclude(exclude)
		{
		}
		bool operator()(IClient* client)
		{
			xrClientData* TempClient = static_cast<xrClientData*>(client);
			if (client->ID == id_to_exclude)
			{
				return false;
			}
			if (!client->flags.bConnected)
			{
				return false;
			}
			if (!TempClient->net_Accepted)
			{
				return false;
			}
			return true;
		}
	};
	struct ClientSenderFunctor
	{
		xrServer* m_owner;
		void* m_data;
		u32 m_size;
		u32 m_dwFlags;
		ClientSenderFunctor(xrServer* owner, void* data, u32 size, u32 dwFlags)
			: m_owner(owner), m_data(data), m_size(size), m_dwFlags(dwFlags)
		{
		}
		void operator()(IClient* client)
		{
			m_owner->SendTo_LL(client->ID, m_data, m_size, m_dwFlags);
		}
	};
	ClientSenderFunctor temp_functor(this, P.B.data.data(), P.B.data.size(), dwFlags);
	net_players.ForFoundClientsDo(ClientExcluderPredicate(exclude), temp_functor);
}
//--------------------------------------------------------------------
CSE_Abstract* xrServer::entity_Create(const char* name)
{
	return F_entity_Create(name);
}

void xrServer::entity_Destroy(CSE_Abstract*& P)
{
#ifdef DEBUG
	if (dbg_net_Draw_Flags.test(dbg_destroy))
	{
		Msg("xrServer::entity_Destroy : [%d][%s][%s]", P->ID, P->name(), P->name_replace());
	}
#endif
	R_ASSERT(P);
	entities.erase(P->ID);
	FreeID(P->ID, Device.TimerAsync());

	if (P->owner && P->owner->owner == P)
	{
		P->owner->owner = nullptr;
	}

	P->owner = nullptr;
	if (!ai().get_alife() || !P->m_bALifeControl)
	{
		F_entity_Destroy(P);
	}
}

//--------------------------------------------------------------------
void xrServer::Server_Client_Check(IClient* CL)
{
	if (SV_Client && SV_Client->ID == CL->ID)
	{
		if (!CL->flags.bConnected)
		{
			SV_Client = nullptr;
		};
		return;
	}

	if (SV_Client && SV_Client->ID != CL->ID)
	{
		return;
	}

	if (!CL->flags.bConnected)
	{
		return;
	}

	if (CL->process_id == Platform::GetCurrentProcessId())
	{
		CL->flags.bLocal = 1;
		SV_Client = (xrClientData*)CL;
		Msg("New SV client 0x%08x", SV_Client->ID.value());
		return;
	}

	CL->flags.bLocal = 0;
}

bool xrServer::OnCL_QueryHost()
{
	if (game->Type() == eGameIDSingle)
	{
		return false;
	}

	return (GetClientsCount() != 0);
}

CSE_Abstract* xrServer::GetEntity(u32 Num)
{
	if (Num >= entities.size())
	{
		return nullptr;
	}

	auto Iter = entities.begin();
	std::advance(Iter, Num);
	return Iter->second;
}

void xrServer::OnChatMessage(NET_Packet* P, xrClientData* CL)
{
	if (!CL->net_Ready)
	{
		return;
	}

	struct MessageSenderController
	{
		xrServer* m_owner;
		s16 m_team;
		game_PlayerState* m_sender_ps;
		NET_Packet* m_packet;
		MessageSenderController(xrServer* owner)
			: m_owner(owner)
		{
		}
		void operator()(IClient* client)
		{
			xrClientData* xr_client = static_cast<xrClientData*>(client);
			game_PlayerState* ps = xr_client->ps;
			if (!ps)
			{
				return;
			}
			if (!xr_client->net_Ready)
			{
				return;
			}
			if (m_team != -1 && ps->team != m_team)
			{
				return;
			}
			if (m_sender_ps->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD) &&
				!ps->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD))
			{
				return;
			}
			m_owner->SendTo(client->ID, *m_packet);
		}
	};
	MessageSenderController mesenger(this);
	mesenger.m_team = P->r_s16();
	mesenger.m_sender_ps = CL->ps;
	mesenger.m_packet = P;
	ForEachClientDoSender(mesenger);
};

void xrServer::OnVoiceMessage(NET_Packet& P, ClientID sender)
{
	xrClientData* pClient = (xrClientData*)ID_to_client(sender);

	if (!pClient || !pClient->net_Ready)
	{
		return;
	}
	game_PlayerState* ps = pClient->ps;
	if (!ps)
	{
		return;
	}
	if (!pClient->owner)
	{
		return;
	}

	struct send_voice_message
	{
		xrServer* m_server;
		NET_Packet* m_packet;
		xrClientData* m_from;
		float m_voiceDistanceSqr;

		void operator()(IClient* client)
		{
			if (client == m_server->GetServerClient())
			{
				return;
			}

			xrClientData* CL = static_cast<xrClientData*>(client);
			if (!CL || !CL->net_Ready || !CL->owner || !m_from->owner || !m_from->ps)
			{
				return;
			}

			if (CL->ID == m_from->ID)
			{
				return;
			}

			game_PlayerState* ps = CL->ps;
			if (!ps || ps->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD))
			{
				return;
			}

			float distanceSqr = CL->owner->Position().distance_to_sqr(m_from->owner->Position());

			if (distanceSqr <= m_voiceDistanceSqr)
			{
				m_server->SendTo(CL->ID, *m_packet, net_flags(false, true, true, true));
			}
		}
	};

	u8 distance = P.r_u8(); // distance byte
	float voiceDistanceSqr = (float)distance * (float)distance;

	send_voice_message tmp_functor;
	tmp_functor.m_server = this;
	tmp_functor.m_packet = &P;
	tmp_functor.m_from = pClient;
	tmp_functor.m_voiceDistanceSqr = voiceDistanceSqr;

	ForEachClientDo(tmp_functor);
};

#ifdef DEBUG

static bool _ve_initialized = false;
static bool _ve_use = true;

bool xrServer::verify_entities() const
{
	if (!_ve_initialized)
	{
		_ve_initialized = true;
	}
	if (!_ve_use)
	{
		return true;
	}

	xrS_entities::const_iterator I = entities.begin();
	xrS_entities::const_iterator E = entities.end();
	for ( ; I != E; ++I) {
		VERIFY2(I->first != ALife::INVALID_OBJECT_ID,"SERVER : Invalid entity id as a map key - ALife::INVALID_OBJECT_ID");
		VERIFY2(I->second,"SERVER : Null entity object in the map");
		VERIFY3(I->first == I->second->ID,"SERVER : ID mismatch - map key doesn't correspond to the real entity ID", I->second ? I->second->name_replace() : "");
		verify_entity(I->second);
	}
	return (true);
}

void xrServer::verify_entity(const CSE_Abstract* entity) const
{
	if (!IsGameTypeSingle())
	{
		return;
	}

	VERIFY(entity->m_wVersion != 0);

	if (entity->ID_Parent != ALife::INVALID_OBJECT_ID)
	{
		xrS_entities::const_iterator J = entities.find(entity->ID_Parent);
		if (J != entities.end())
		{
			VERIFY3(J->second, "SERVER : Null entity object in the map", entity->name_replace());
			VERIFY3(J->first == J->second->ID, "SERVER : ID mismatch - map key doesn't correspond to the real entity ID", J->second ? J->second->name_replace() : "");
			VERIFY3(std::ranges::find(J->second->children, entity->ID) != J->second->children.end(), "SERVER : Parent/Children relationship mismatch - Object has parent, but corresponding parent doesn't have children", J->second ? J->second->name_replace() : "");
		}
	}

	for (auto ID : entity->children) {
		VERIFY3(ID != ALife::INVALID_OBJECT_ID,"SERVER : Invalid entity children id - ALife::INVALID_OBJECT_ID",entity->name_replace());
		xrS_entities::const_iterator J = entities.find(ID);
		VERIFY3(J != entities.end(),"SERVER : Cannot find children in the map",entity->name_replace());
		VERIFY3(J->second,"SERVER : Null entity object in the map",entity->name_replace());
		VERIFY3(J->first == J->second->ID,"SERVER : ID mismatch - map key doesn't correspond to the real entity ID", J->second ? J->second->name_replace() : "");
		VERIFY3(J->second->ID_Parent == entity->ID,"SERVER : Parent/Children relationship mismatch - Object has children, but children doesn't have parent", J->second ? J->second->name_replace() : "");
	}
}

#endif // DEBUG

shared_str xrServer::level_name(const shared_str& server_options) const
{
	return (game->level_name(server_options));
}
shared_str xrServer::level_version(const shared_str& server_options) const
{
	return (game_sv_GameState::parse_level_version(server_options));
}

void xrServer::create_direct_client()
{
	SClientConnectData cl_data;
	cl_data.clientID.set(1);
	xr_strcpy(cl_data.name, "single_player");
	cl_data.process_id = Platform::GetCurrentProcessId();

	new_client(&cl_data);
}


void xrServer::ProceedDelayedPackets()
{
	DelayedPackestCS.Enter();
	while (!m_aDelayedPackets.empty())
	{
		DelayedPacket& DPacket = *m_aDelayedPackets.begin();
		OnDelayedMessage(DPacket.Packet, DPacket.SenderID);
		m_aDelayedPackets.pop_front();
	}
	DelayedPackestCS.Leave();
};

void xrServer::AddDelayedPacket(NET_Packet& Packet, ClientID Sender)
{
	DelayedPackestCS.Enter();

	m_aDelayedPackets.push_back(DelayedPacket());
	DelayedPacket* NewPacket = &(m_aDelayedPackets.back());
	NewPacket->SenderID = Sender;
	NewPacket->Packet.B.data.resize(Packet.B.data.size());
	CopyMemory(NewPacket->Packet.B.data.data(), Packet.B.data.data(), Packet.B.data.size());
	NewPacket->Packet.inistream = Packet.inistream;
	NewPacket->Packet.r_pos = Packet.r_pos;
	NewPacket->Packet.w_allow = Packet.w_allow;
	NewPacket->Packet.timeReceive = Packet.timeReceive;

	DelayedPackestCS.Leave();
}

u32 g_sv_dwMaxClientPing = 2000;
u32 g_sv_time_for_ping_check = 15000; // 15 sec
u8 g_sv_maxPingWarningsCount = 5;

void xrServer::PerformCheckClientsForMaxPing()
{
	struct MaxPingClientDisconnector
	{
		xrServer* m_owner;
		MaxPingClientDisconnector(xrServer* owner)
			: m_owner(owner)
		{
		}
		void operator()(IClient* client)
		{
			xrClientData* Client = static_cast<xrClientData*>(client);
			game_PlayerState* ps = Client->ps;
			if (!ps)
			{
				return;
			}

			if (client == m_owner->GetServerClient())
			{
				return;
			}

			if (ps->ping > g_sv_dwMaxClientPing &&
				Client->m_ping_warn.m_dwLastMaxPingWarningTime + g_sv_time_for_ping_check < Device.dwTimeGlobal)
			{
				++Client->m_ping_warn.m_maxPingWarnings;
				Client->m_ping_warn.m_dwLastMaxPingWarningTime = Device.dwTimeGlobal;

				if (Client->m_ping_warn.m_maxPingWarnings >= g_sv_maxPingWarningsCount)
				{ // kick
					string256 reason;
					xr_strconcat(reason, g_pStringTable->translate("st_kicked_by_server").c_str());
					Level().Server->DisconnectClient(Client, reason);
				}
				else
				{ // send warning
					NET_Packet P;
					P.w_begin(M_CLIENT_WARN);
					P.w_u8(1); // 1 means max-ping-warning
					P.w_u16(ps->ping);
					P.w_u8(Client->m_ping_warn.m_maxPingWarnings);
					P.w_u8(g_sv_maxPingWarningsCount);
					m_owner->SendTo(Client->ID, P, net_flags(false, true));
				}
			}
		}
	};
	MaxPingClientDisconnector temp_functor(this);
	ForEachClientDoSender(temp_functor);
}

extern s32 g_sv_dm_dwFragLimit;
extern s32 g_sv_ah_dwArtefactsNum;
extern s32 g_sv_dm_dwTimeLimit;
extern int g_sv_ah_iReinforcementTime;
extern int g_sv_mp_iDumpStatsPeriod;
extern bool g_bCollectStatisticData;

// xr_token game_types[];
const char* GameTypeToString(EGameIDs gt, bool bShort);

void xrServer::GetServerInfo(CServerInfo* si)
{
	string32 tmp;
	string256 tmp256;

	si->AddItem("Server port", _itoa(GetPort(), tmp, 10), RGB(128, 128, 255));
	const char* time = InventoryUtilities::GetTimeAsString(Device.dwTimeGlobal, InventoryUtilities::etpTimeToSecondsAndDay).c_str();
	si->AddItem("Uptime", time, RGB(255, 228, 0));

	xr_strcpy(tmp256, GameTypeToString(game->Type(), true));
	if (game->Type() == eGameIDDeathmatch || game->Type() == eGameIDTeamDeathmatch)
	{
		xr_strcat(tmp256, " [");
		xr_strcat(tmp256, _itoa(g_sv_dm_dwFragLimit, tmp, 10));
		xr_strcat(tmp256, "] ");
	}
	else if (game->Type() == eGameIDArtefactHunt || game->Type() == eGameIDCaptureTheArtefact)
	{
		xr_strcat(tmp256, " [");
		xr_strcat(tmp256, _itoa(g_sv_ah_dwArtefactsNum, tmp, 10));
		xr_strcat(tmp256, "] ");
	}

	xr_strcat(tmp256, " time limit [");
	xr_strcat(tmp256, _itoa(g_sv_dm_dwTimeLimit, tmp, 10));
	xr_strcat(tmp256, "] ");

	if (game->Type() == eGameIDArtefactHunt || game->Type() == eGameIDCaptureTheArtefact)
	{
		xr_strcat(tmp256, " RT [");
		xr_strcat(tmp256, _itoa(g_sv_ah_iReinforcementTime, tmp, 10));
		xr_strcat(tmp256, "]");
	}
	si->AddItem("Game type", tmp256, RGB(128, 255, 255));

	if (g_pGameLevel)
	{
		time = InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes).c_str();
		xr_strcpy(tmp256, time);

		if (g_sv_mp_iDumpStatsPeriod > 0)
		{
			xr_strcat(tmp256, " statistic [");
			xr_strcat(tmp256, _itoa(g_sv_mp_iDumpStatsPeriod, tmp, 10));
			xr_strcat(tmp256, "]");
			if (g_bCollectStatisticData)
			{
				xr_strcat(tmp256, "[weapons]");
			}
		}
		si->AddItem("Game time", tmp256, RGB(205, 228, 178));

		// FPS
		string32 FPSText = {};
		xr_strcat(FPSText, itoa((int)(1.f / Device.fTimeDelta), tmp, 10));
		si->AddItem("FPS", FPSText, RGB(205, 228, 178));
	}
}

void xrServer::AddCheater(shared_str const& reason, ClientID const& cheaterID)
{
	CheaterToKick NewCheater;
	NewCheater.reason = reason;
	NewCheater.cheater_id = cheaterID;
	m_cheaters.push_back(NewCheater);
}

void xrServer::KickCheaters()
{
	for (const CheaterToKick& ID : m_cheaters)
	{
		IClient* TempClient = GetClientByID(ID.cheater_id);
		if (!TempClient)
		{
			Msg("! ERROR: KickCheaters: client [%u] not found", ID.cheater_id);
			continue;
		}
		ClientID TempClientID = TempClient->ID;
		DisconnectClient(TempClient, ID.reason.c_str());

		NET_Packet P;
		P.w_begin(M_GAMEMESSAGE);
		P.w_u32(GAME_EVENT_SERVER_STRING_MESSAGE);
		P.w_stringZ(ID.reason.c_str() + 2);
		Level().Server->SendBroadcast(TempClientID, P);
	}
	m_cheaters.clear();
}

void xrServer::MakeScreenshot(ClientID const& admin_id, ClientID const& cheater_id)
{
	if ((cheater_id == SV_Client->ID) && g_dedicated_server)
	{
		return;
	}
	for (int i = 0; i < sizeof(m_screenshot_proxies) / sizeof(clientdata_proxy*); ++i)
	{
		if (!m_screenshot_proxies[i]->is_active())
		{
			m_screenshot_proxies[i]->make_screenshot(admin_id, cheater_id);
			Msg("* admin [%d] is making screeshot of client [%d]", admin_id, cheater_id);
			return;
		}
	}
	Msg("! ERROR: SV: not enough file transfer proxies for downloading screenshot, please try later ...");
}
void xrServer::MakeConfigDump(ClientID const& admin_id, ClientID const& cheater_id)
{
	if ((cheater_id == SV_Client->ID) && g_dedicated_server)
	{
		return;
	}
	for (int i = 0; i < sizeof(m_screenshot_proxies) / sizeof(clientdata_proxy*); ++i)
	{
		if (!m_screenshot_proxies[i]->is_active())
		{
			m_screenshot_proxies[i]->make_config_dump(admin_id, cheater_id);
			Msg("* admin [%d] is making config dump of client [%d]", admin_id, cheater_id);
			return;
		}
	}
	Msg("! ERROR: SV: not enough file transfer proxies for downloading file, please try later ...");
}


void xrServer::initialize_screenshot_proxies()
{
	for (int i = 0; i < sizeof(m_screenshot_proxies) / sizeof(clientdata_proxy*); ++i)
	{
		m_screenshot_proxies[i] = new clientdata_proxy(m_file_transfers);
	}
}
void xrServer::deinitialize_screenshot_proxies()
{
	for (int i = 0; i < sizeof(m_screenshot_proxies) / sizeof(clientdata_proxy*); ++i)
	{
		xr_delete(m_screenshot_proxies[i]);
	}
}

struct PlayerInfoWriter
{
	NET_Packet* dest;
	void operator()(IClient* C)
	{
		xrClientData* TempClient = smart_cast<xrClientData*>(C);
		if (!TempClient)
		{
			return;
		}

		dest->w_clientID(TempClient->ID);
		dest->w_stringZ(TempClient->m_cAddress.to_string().c_str());
		dest->w_stringZ(TempClient->m_cdkey_digest);
	}
};

void xrServer::SendPlayersInfo(ClientID const& to_client)
{
	PlayerInfoWriter tmp_functor;
	NET_Packet tmp_packet;
	tmp_packet.w_begin(M_GAMEMESSAGE);
	tmp_packet.w_u32(GAME_EVENT_PLAYERS_INFO_REPLY);
	tmp_functor.dest = &tmp_packet;
	ForEachClientDo(tmp_functor);
	SendTo(to_client, tmp_packet, net_flags(true, true));
}

void xrServer::OnScriptEvent(NET_Packet& P, ClientID sender)
{
	script_server_events.push_back(ScriptEvent());
	ScriptEvent* pEvent = &(script_server_events.back());

	pEvent->SenderID = sender.value();
	CopyMemory(&(pEvent->Packet), &P, sizeof(NET_Packet));
}

ScriptEvent* xrServer::GetFrontServerScriptEvent()
{
	R_ASSERT2(script_server_events.size() > 0, "empty script server events");
	return &(script_server_events.front());
}

void xrServer::PopFrontServerScriptEvent()
{
	script_server_events.pop_front();
}

ScriptEvent* xrServer::GetLastServerScriptEvent()
{
	R_ASSERT2(script_server_events.size() > 0, "empty script server events");
	return &(script_server_events.back());
}

void xrServer::PopLastServerScriptEvent()
{
	script_server_events.pop_back();
}

u32 xrServer::GetSizeServerScriptEvent()
{
	return script_server_events.size();
}

void xrServer::OnProcessClientMapData(NET_Packet& P, ClientID const& clientID)
{
	string128 client_map_name;
	string128 client_map_version;
	u32 client_geom_crc32;

	P.r_stringZ_s(client_map_name);
	P.r_stringZ_s(client_map_version);
	P.r_u32(client_geom_crc32);

	const char* server_map_name = Level().get_net_DescriptionData().map_name;
	const char* server_map_version = Level().get_net_DescriptionData().map_version;

	NET_Packet responseP;
	responseP.w_begin(M_SV_MAP_NAME);

	if ((xr_strcmp(server_map_name, client_map_name)) || (xr_strcmp(server_map_version, client_map_version)))
	{
		responseP.w_u8(static_cast<u8>(YouHaveOtherMap));
		Msg("--- Client [0x%08x] has incorrect map [%s] or version [%s]", client_map_name, client_map_version);
		// here we can make hard disconnect of this client...
	}
	else if (!Level().IsChecksumsEqual(client_geom_crc32))
	{
		responseP.w_u8(static_cast<u8>(InvalidChecksum));
	}
	else
	{
		responseP.w_u8(static_cast<u8>(SuccessSync));
	}

	SendTo(clientID, responseP, net_flags(true, true));
}

void xrServer::Process_event_activate(NET_Packet& P, const ClientID sender, const u32 time, const ALife::_OBJECT_ID id_parent, const ALife::_OBJECT_ID id_entity, bool send_message)
{
	// Parse message
	CSE_Abstract* e_parent = game->get_entity_from_eid(id_parent);
	CSE_Abstract* e_entity = game->get_entity_from_eid(id_entity);

#ifndef MASTER_GOLD
	Msg("---Artefact activate (parent = %d) (item = %d)", id_parent, id_entity);
#endif // #ifndef MASTER_GOLD

	if (g_dedicated_server)
	{
		if (e_parent == nullptr)
		{
			Msg("parent not found. id_parent=%d id_entity=%d frame=%d", id_parent, id_entity, Device.dwFrame);
			return;
		}

		if (e_entity == nullptr)
		{
			Msg("entity not found. id_parent=%d id_entity=%d frame=%d", id_parent, id_entity, Device.dwFrame);
			return;
		}
	}
	else
	{
		R_ASSERT2(e_parent, make_string<const char*>("parent not found. id_parent=%d id_entity=%d frame=%d", id_parent, id_entity, Device.dwFrame));
		R_ASSERT2(e_entity, make_string<const char*>("entity not found. id_parent=%d id_entity=%d frame=%d", id_parent, id_entity, Device.dwFrame));
	}

	if (!game->OnActivate(id_parent, id_entity))
	{
		return;
	}


	if (ALife::INVALID_OBJECT_ID == e_entity->ID_Parent)
	{
#ifndef MASTER_GOLD
		Msg("~ ERROR: can't activate independant object. entity[%s:%d], parent[%s:%d], section[%s]",
			e_entity->name_replace(),
			id_entity,
			e_parent->name_replace(),
			id_parent,
			*e_entity->s_name);
#endif // #ifndef MASTER_GOLD
		return;
	}

	// Signal to everyone (including sender)
	if (send_message)
	{
		DWORD MODE = net_flags(true, true, false, true);
		SendBroadcast(BroadcastCID, P, MODE);
	}

	return;
}

void xrServer::Perform_destroy(CSE_Abstract* object, u32 mode)
{
	R_ASSERT(object);
	R_ASSERT(object->ID_Parent == ALife::INVALID_OBJECT_ID);

	while (!object->children.empty())
	{
		CSE_Abstract* child = game->get_entity_from_eid(object->children.back());
		R_ASSERT2(child, make_string<const char*>("child registered but not found [%d]", object->children.back()));
		Perform_reject(child, object, 2 * NET_Latency);
		Perform_destroy(child, mode);
	}

	auto object_id = object->ID;
	entity_Destroy(object);

	NET_Packet P;
	P.w_begin(M_EVENT);
	P.w_u32(Device.dwTimeGlobal - 2 * NET_Latency);
	P.w_u16(GE_DESTROY);
	P << object_id;
	SendBroadcast(BroadcastCID, P, mode);
}

void xrServer::SLS_Clear()
{
	u32 mode = net_flags(true, true);
	while (!entities.empty())
	{
		bool found = false;
		xrS_entities::const_iterator I = entities.begin();
		xrS_entities::const_iterator E = entities.end();
		for (; I != E; ++I)
		{
			if ((*I).second->ID_Parent != ALife::INVALID_OBJECT_ID)
			{
				continue;
			}
			found = true;
			Perform_destroy((*I).second, mode);
			break;
		}
		if (!found) // R_ASSERT(found);
		{
			I = entities.begin();
			E = entities.end();
			for (; I != E; ++I)
			{
				if (I->second)
				{
					Msg("! ERROR: can't destroy object [%d][%s] with parent [%d]",
						I->second->ID,
						I->second->s_name.size() ? I->second->s_name.c_str() : "unknown",
						I->second->ID_Parent
					);
				}
				else
				{
					Msg("! ERROR: can't destroy entity [%d][?] with parent[?]", I->first);
				}
			}
			Msg("! ERROR: FATAL: can't delete all entities !");
			entities.clear();
		}
	}
}

void xrServer::PerformSecretKeysSync(xrClientData* xrCL)
{
	VERIFY(xrCL);
	xrCL->m_last_key_sync_request_seed = m_seed_generator.genrate();

	NET_Packet key_sync_command;
	key_sync_command.w_begin(M_SECURE_KEY_SYNC);
	key_sync_command.w_s32(xrCL->m_last_key_sync_request_seed);
	SendTo(xrCL->ID, key_sync_command);
}

void xrServer::PerformSecretKeysSyncAck(xrClientData* xrCL, NET_Packet& P)
{
	VERIFY(xrCL);
	s32 new_seed;
	P.r_s32(new_seed); // only for DEBUG
	VERIFY2(new_seed == xrCL->m_last_key_sync_request_seed, "cracker detected !");
	secure_messaging::generate_key(xrCL->m_last_key_sync_request_seed, xrCL->m_secret_key);
}

void xrServer::SecureSendTo(xrClientData* xrCL, NET_Packet& P, u32 dwFlags, u32 dwTimeout)
{
	VERIFY(xrCL);

	NET_Packet enc_packet;

	enc_packet.w_begin(M_SECURE_MESSAGE);
	enc_packet.w(P.B.data.data(), P.B.data.size());
	u32 checksum = secure_messaging::encrypt(
		enc_packet.B.data.data() + sizeof(u16),
		enc_packet.B.data.size() - sizeof(u16),
		xrCL->m_secret_key
	);
	enc_packet.w_u32(checksum);
	SendTo(xrCL->ID, enc_packet, dwFlags, dwTimeout);
}

void xrServer::OnSecureMessage(NET_Packet& P, xrClientData* xrClSender)
{
#ifdef DEBUG
	char dbg_tmp_buff[33];
	ZeroMemory(dbg_tmp_buff, sizeof(dbg_tmp_buff));
	xr_strcpy(dbg_tmp_buff, "xray crypt check");
	u32 dbg_encrypt_checksum = secure_messaging::encrypt(dbg_tmp_buff, sizeof(dbg_tmp_buff), xrClSender->m_secret_key);
	u32 dbg_decrypt_checksum = secure_messaging::decrypt(dbg_tmp_buff, sizeof(dbg_tmp_buff), xrClSender->m_secret_key);
	VERIFY(dbg_encrypt_checksum == dbg_decrypt_checksum);
#endif
	NET_Packet dec_packet;
	dec_packet.B.data.resize(P.B.data.size() - sizeof(u16) - sizeof(u32)); // - r_begin - crypt_check_sum
	P.r(dec_packet.B.data.data(), dec_packet.B.data.size());
	u32 checksum = secure_messaging::decrypt(dec_packet.B.data.data(), dec_packet.B.data.size(), xrClSender->m_secret_key);
	u32 real_checksum = 0;
	P.r_u32(real_checksum);
	VERIFY2(checksum == real_checksum, "caught cheater");
	if (checksum != real_checksum)
	{
		return; // WARNING!: do not add any log messages - security treat!
	}

	OnMessage(dec_packet, xrClSender->ID);
}

void xrServer::Perform_game_export()
{
	struct NetExportToClientFunctor
	{
		xrServer* server_ptr;
		NetExportToClientFunctor(xrServer* server)
			: server_ptr(server)
		{
		}
		void operator()(IClient* client)
		{
			R_ASSERT(server_ptr);
			NET_Packet P;
			u32 mode = net_flags(true, true);

			xrClientData* CL = (xrClientData*)client;
			if (!CL->net_Accepted)
			{
				return;
			}
			P.w_begin(M_SV_CONFIG_GAME);
			server_ptr->game->net_Export_State(P, client->ID);
			server_ptr->SendTo(client->ID, P, mode);
		}
	};
	NetExportToClientFunctor temp_functor(this);
	ForEachClientDoSender(temp_functor);
	game->sv_force_sync = false;
}

void xrServer::Export_game_type(IClient* CL)
{
	NET_Packet P;
	u32 mode = net_flags(true, true);
	P.w_begin(M_SV_CONFIG_NEW_CLIENT);
	P.w_stringZ(game->type_name());
	SendTo(CL->ID, P, mode);
}

void xrServer::Perform_connect_spawn(CSE_Abstract* E, xrClientData* CL, NET_Packet& P)
{
	P.B.data.clear();
	xr_vector<ALife::_OBJECT_ID>::iterator it = std::find(conn_spawned_ids.begin(), conn_spawned_ids.end(), E->ID);
	if (it != conn_spawned_ids.end())
	{
		return;
	}

	conn_spawned_ids.push_back(E->ID);

	if (E->net_Processed)
	{
		return;
	}
	if (E->s_flags.is(M_SPAWN_OBJECT_PHANTOM))
	{
		return;
	}

	// Connectivity order
	CSE_Abstract* Parent = ID_to_entity(E->ID_Parent);
	if (Parent)
	{
		Perform_connect_spawn(Parent, CL, P);
	}

	// Process
	Flags16 save = E->s_flags;
	//-------------------------------------------------
	E->s_flags.set(M_SPAWN_UPDATE, true);
	if (0 == E->owner)
	{
		// PROCESS NAME; Name this entity
		if (E->s_flags.is(M_SPAWN_OBJECT_ASPLAYER))
		{
			CL->owner = E;
			VERIFY(CL->ps);
			E->set_name_replace(CL->ps->getName());
		}

		// Associate
		E->owner = CL;
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacketFull(P, *E);
		}
		else
		{
			E->Spawn_Write(P, true);
			E->UPDATE_Write(P);

			if (g_pGamePersistent->GameType() == eGameIDFreeMP)
			{
				E->SyncWrite(P);
			}
		}

		CSE_ALifeObject* object = smart_cast<CSE_ALifeObject*>(E);
		VERIFY(object);
		if (!object->keep_saved_data_anyway())
		{
			object->client_data.clear();
		}
	}
	else
	{
		VERIFY(!EngineExternal()[EEngineExternalSystem::AdvancedSerialization]);
		E->Spawn_Write(P, false);
		E->UPDATE_Write(P);

		if (g_pGamePersistent->GameType() == eGameIDFreeMP)
		{
			E->SyncWrite(P);
		}
	}
	//-----------------------------------------------------
	E->s_flags = save;
	SendTo(CL->ID, P, net_flags(true, true));
	E->net_Processed = true;
}

void xrServer::SendConfigFinished(ClientID const& clientId)
{
	NET_Packet P;
	P.w_begin(M_SV_CONFIG_FINISHED);
	SendTo(clientId, P, net_flags(true, true));
}

void xrServer::SendConnectionData(IClient* _CL)
{
	conn_spawned_ids.clear();
	xrClientData* CL = (xrClientData*)_CL;
	NET_Packet P;
	// Replicate current entities on to this client
	xrS_entities::iterator I = entities.begin(), E = entities.end();
	for (; I != E; ++I)
	{
		I->second->net_Processed = false;
	}
	for (I = entities.begin(); I != E; ++I)
	{
		Perform_connect_spawn(I->second, CL, P);
	}

	// Start to send server logo and rules
	SendServerInfoToClient(CL->ID);
}

void xrServer::OnCL_Connected(IClient* _CL)
{
	xrClientData* CL = (xrClientData*)_CL;
	CL->net_Accepted = true;

	Export_game_type(CL);
	Perform_game_export();
	SendConnectionData(CL);

	VERIFY2(CL->ps, "Player state not created");
	if (!CL->ps)
	{
		Msg("! ERROR: Player state not created - incorect message sequence!");
		return;
	}

	game->OnPlayerConnect(CL->ID);
}

void xrServer::SendConnectResult(IClient* CL, u8 res, u8 res1, char* ResultStr)
{
	NET_Packet P;
	P.w_begin(M_CLIENT_CONNECT_RESULT);
	P.w_u8(res);
	P.w_u8(res1);
	P.w_stringZ(ResultStr);
	P.w_clientID(CL->ID);

	if (SV_Client && SV_Client == CL)
	{
		P.w_u8(1);
	}
	else
	{
		P.w_u8(0);
	}
	P.w_stringZ(Level().m_caServerOptions);

	SendTo(CL->ID, P);

	if (!res) // need disconnect
	{
		Flush_Clients_Buffers();
		DisconnectClient(CL, ResultStr);
	}

	if (Level().IsDemoPlay())
	{
		Level().StartPlayDemo();

		return;
	}
};

void xrServer::SendProfileCreationError(IClient* CL, char const* reason)
{
	VERIFY(CL);

	NET_Packet P;
	P.w_begin(M_CLIENT_CONNECT_RESULT);
	P.w_u8(0);
	P.w_u8(ecr_profile_error);
	P.w_stringZ(reason);
	P.w_clientID(CL->ID);
	SendTo(CL->ID, P);
	if (CL != GetServerClient())
	{
		Flush_Clients_Buffers();
		DisconnectClient(CL, reason);
	}
}

// this method response for client validation on connect state (CLevel::net_start_client2)
// the first validation is CDKEY, then gamedata checksum (NeedToCheckClient_BuildVersion), then
// banned or not...
// WARNING ! if you will change this method see M_AUTH_CHALLENGE event handler
void xrServer::Check_GameSpy_CDKey_Success(IClient* CL)
{
	if (NeedToCheckClient_BuildVersion(CL))
	{
		return;
	}
	//-------------------------------------------------------------
	RequestClientDigest(CL);
};

bool g_SV_Disable_Auth_Check = false;

bool xrServer::NeedToCheckClient_BuildVersion(IClient* CL)
{
	/*#ifdef DEBUG

		return false;

	#endif*/
	xrClientData* TempClient = smart_cast<xrClientData*>(CL);
	VERIFY(TempClient);
	PerformSecretKeysSync(TempClient);


	if (g_SV_Disable_Auth_Check)
	{
		return false;
	}
	CL->flags.bVerified = false;
	NET_Packet P;
	P.w_begin(M_AUTH_CHALLENGE);
	SendTo(CL->ID, P);
	return true;
}

void xrServer::OnBuildVersionRespond(IClient* CL, NET_Packet& P)
{
	u16 Type;
	P.r_begin(Type);
	[[maybe_unused]] u64 _our = FS.auth_get();
	[[maybe_unused]] u64 _him = P.r_u64();

#ifndef DEBUG
	if (_our != _him)
	{
		SendConnectResult(CL, 0, ecr_data_verification_failed, (char*)"Data verification failed. Cheater?");
	}
	else
#endif
	{
		bool bAccessUser = false;
		string512 res_check;

		if (!CL->flags.bLocal)
		{
			bAccessUser = Check_ServerAccess(CL, res_check);
		}

		if (CL->flags.bLocal || bAccessUser)
		{
			RequestClientDigest(CL);
		}
		else
		{
			Msg("* Client 0x%08x has an incorrect password", CL->ID.value());
			xr_strcat(res_check, "Invalid password.");
			SendConnectResult(CL, 0, ecr_password_verification_failed, res_check);
		}
	}
}

void xrServer::Check_BuildVersion_Success(IClient* CL)
{
	CL->flags.bVerified = true;
	SendConnectResult(CL, 1, 0, (char*)"All Ok");
}

void xrServer::OnCL_Disconnected(IClient* CL)
{
	// Game config (all, info includes deleted player now, excludes at the next cl-update)
	NET_Packet P;
	P.B.data.clear();
	P.w_clientID(CL->ID);
	xrClientData* xrCData = (xrClientData*)(CL);
	VERIFY(xrCData);

	if (!xrCData->ps)
	{
		return;
	}

	P.w_stringZ(xrCData->ps->getName());
	P << xrCData->ps->GameID;
	P.r_pos = 0;

	ClientID clientID;
	clientID.set(0);

	game->AddDelayedEvent(P, GAME_EVENT_PLAYER_DISCONNECTED, 0, clientID);

	//
	xrS_entities::iterator I = entities.begin(), E = entities.end();
	const bool NotDestory = GetClientsCount() > 1 && !CL->flags.bLocal;
	if (!NotDestory)
	{
		// Destroy entities
		while (!entities.empty())
		{
			CSE_Abstract* entity = entities.begin()->second;
			entity_Destroy(entity);
		}
	}

	Server_Client_Check(CL);
}

const char* xrServer::get_map_download_url(const char* level_name, const char* level_version)
{
	R_ASSERT(level_name && level_version);
	const char* ret_url = "";
	CInifile* level_ini = pApp->GetArchiveHeader(level_name, level_version);
	if (!level_ini)
	{
		if (!IsGameTypeSingle())
		{
			Msg("! Warning: level [%s][%s] has not header ltx", level_name, level_version);
		}

		return ret_url;
	}

	ret_url = level_ini->r_string_wb("header", "link").c_str();
	if (!ret_url)
	{
		ret_url = "";
	}

	return ret_url;
}

xrServer::EConnect xrServer::Connect(shared_str& session_name, GameDescriptionData& game_descr)
{
#ifdef DEBUG
	Msg("* sv_Connect: %s", *session_name);
#endif
	PROF_EVENT("xrServer::Connect");
	// Parse options and create game
	if (0 == strchr(*session_name, '/'))
	{
		return ErrConnect;
	}

	string1024 options;
	R_ASSERT2(xr_strlen(session_name) <= sizeof(options), "session_name too BIIIGGG!!!");
	xr_strcpy(options, strchr(*session_name, '/') + 1);

	// Parse game type
	string1024 type;
	R_ASSERT2(xr_strlen(options) <= sizeof(type), "session_name too BIIIGGG!!!");
	xr_strcpy(type, options);
	if (strchr(type, '/'))
	{
		*strchr(type, '/') = 0;
	}
	game = nullptr;

	CLASS_ID clsid = game_GameState::getCLASS_ID(type, true);
	game = smart_cast<game_sv_GameState*>(NEW_INSTANCE(clsid));

	// Options
	if (0 == game)
	{
		return ErrConnect;
	}

	if (game->Type() != eGameIDSingle)
	{
		m_file_transfers = new file_transfer::server_site();
		initialize_screenshot_proxies();
		LoadServerInfo();
		xr_auth_strings_t tmp_ignore;
		xr_auth_strings_t tmp_check;
		fill_auth_check_params(tmp_ignore, tmp_check);
		FS.auth_generate(tmp_ignore, tmp_check);
	}
#ifdef DEBUG
	Msg("* Created server_game %s", game->type_name());
#endif

	ZeroMemory(&game_descr, sizeof(game_descr));
	xr_strcpy(game_descr.map_name, game->level_name(session_name.c_str()).c_str());
	xr_strcpy(game_descr.map_version, game_sv_GameState::parse_level_version(session_name.c_str()).c_str());
	xr_strcpy(game_descr.download_url, get_map_download_url(game_descr.map_name, game_descr.map_version));

	game->Create(session_name);

	return IPureServer::Connect(*session_name, game_descr);
}


IClient* xrServer::new_client(SClientConnectData* cl_data)
{
	IClient* CL = client_Find_Get(cl_data->clientID);
	VERIFY(CL);

	// copy entity
	CL->ID = cl_data->clientID;
	CL->process_id = cl_data->process_id;
	CL->name = cl_data->name; // only for offline mode
	CL->pass._set(cl_data->pass);

	NET_Packet P;
	P.B.data.clear();
	P.r_pos = 0;

	game->AddDelayedEvent(P, GAME_EVENT_CREATE_CLIENT, 0, CL->ID);

	return CL;
}

void xrServer::AttachNewClient(IClient* CL)
{
	MSYS_CONFIG msgConfig;
	msgConfig.sign1 = 0x12071980;
	msgConfig.sign2 = 0x26111975;

	if (psNET_direct_connect) // single_game
	{
		SV_Client = CL;
		CL->flags.bLocal = 1;
		SendTo_LL(SV_Client->ID, &msgConfig, sizeof(msgConfig), net_flags(true, true, true, true));
	}
	else
	{
		SendTo_LL(CL->ID, &msgConfig, sizeof(msgConfig), net_flags(true, true, true, true));
		Server_Client_Check(CL);
	}

	// gen message
	if (!NeedToCheckClient_GameSpy_CDKey(CL))
	{
		//-------------------------------------------------------------
		Check_GameSpy_CDKey_Success(CL);
	}

	CL->m_guid[0] = 0;
}

void xrServer::RequestClientDigest(IClient* CL)
{
	if (IsGameTypeSingle() || (CL == GetServerClient()))
	{
		Check_BuildVersion_Success(CL);
		return;
	}
	xrClientData* TempClient = smart_cast<xrClientData*>(CL);
	VERIFY(TempClient);
	PerformSecretKeysSync(TempClient);

	NET_Packet P;
	P.w_begin(M_SV_DIGEST);
	SendTo(CL->ID, P);
}

#define NET_BANNED_STR "Player banned by server!"
void xrServer::ProcessClientDigest(xrClientData* xrCL, NET_Packet* P)
{
	R_ASSERT(xrCL);
	IClient* TempClient = static_cast<IClient*>(xrCL);
	game_sv_mp* server_game = game->cast_game_sv_mp();
	P->r_stringZ(xrCL->m_cdkey_digest);
	shared_str admin_name;
	if (server_game->IsPlayerBanned(xrCL->m_cdkey_digest.c_str(), admin_name))
	{
		R_ASSERT2(TempClient != GetServerClient(), "can't disconnect server client");
		Msg("--- Client [%s] tried to connect - rejecting connection (he is banned by %s) ...",
			TempClient->m_cAddress.to_string().c_str(),
			admin_name.size() ? admin_name.c_str() : "Server");
		string256 message_to_user;
		if (admin_name.size())
		{
			xr_strconcat(message_to_user, "mp_you_have_been_banned_by ", admin_name.c_str());
		}
		else
		{
			xr_strcat(message_to_user, "");
		}
		SendConnectResult(TempClient, 0, ecr_have_been_banned, message_to_user);
		return;
	}
	GetPooledState(xrCL);
	PerformSecretKeysSync(xrCL);
	Check_BuildVersion_Success(TempClient);
}

void xrServer::Disconnect()
{
	if (m_file_transfers)
	{
		deinitialize_screenshot_proxies();
		xr_delete(m_file_transfers);
	}

	script_server_events.clear();

	IPureServer::Disconnect();
	SLS_Clear();
	xr_delete(game);
}

void xrServer::SLS_Default()
{
	if (game->custom_sls_default())
	{
		game->sls_default();
		return;
	}

#ifdef USE_DESIGNER_KEY
	bool _designer = !!strstr(Core.Params, "-designer");
	CSE_ALifeCreatureActor* _actor = 0;
#endif

	string_path fn_spawn;
	if (FS.exist(fn_spawn, "$level$", "level.spawn"))
	{
		IReader* SP = FS.r_open(fn_spawn);
		NET_Packet P;
		u32 S_id;
		for (IReader* S = SP->open_chunk_iterator(S_id); S; S = SP->open_chunk_iterator(S_id, S))
		{
			P.B.data.resize(S->length());
			S->r(P.B.data.data(),P.B.data.size());

			u16 ID;
			P.r_begin(ID);
			R_ASSERT(M_SPAWN == ID);
			ClientID clientID;
			clientID.set(0);

#ifdef USE_DESIGNER_KEY
			CSE_Abstract* entity =
#endif
				Process_spawn(P, clientID);
#ifdef USE_DESIGNER_KEY
			if (_designer)
			{
				CSE_ALifeCreatureActor* actor = smart_cast<CSE_ALifeCreatureActor*>(entity);
				if (actor)
				{
					_actor = actor;
				}
			}
#endif
		}
		FS.r_close(SP);
	}

#ifdef USE_DESIGNER_KEY
	if (!_designer)
	{
		return;
	}

	if (_actor)
	{
		return;
	}

	_actor = smart_cast<CSE_ALifeCreatureActor*>(entity_Create("actor"));
	_actor->o_Position = Fvector().set(0.f, 0.f, 0.f);
	_actor->set_name_replace("designer");
	_actor->s_flags.flags |= M_SPAWN_OBJECT_ASPLAYER;
	NET_Packet packet;
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(packet, *_actor);
	} else
	{
		packet.w_begin(M_SPAWN);
		_actor->Spawn_Write(packet, true);
	}

	u16 id;
	packet.r_begin(id);
	R_ASSERT(id == M_SPAWN);
	ClientID clientID;
	clientID.set(0);
	Process_spawn(packet, clientID);
#endif
}

void xrServer::SLS_Load(IReader& fs)
{
	VERIFY(!EngineExternal()[EEngineExternalSystem::AdvancedSerialization]);
	// Generate spawn+update
	NET_Packet P;
	u16 u_id = 0xffff;
	u32 C;
	for (IReader* F = fs.open_chunk_iterator(C); F; F = fs.open_chunk_iterator(C, F))
	{
		// Spawn
		P.B.data.resize(F->r_u16());
		F->r(P.B.data.data(),P.B.data.size());
		P.r_begin(u_id);
		R_ASSERT(M_SPAWN == u_id);
		ClientID clientID;
		clientID.set(0);
		Process_spawn(P, clientID);

		// Update
		P.B.data.resize(F->r_u16());
		F->r(P.B.data.data(),P.B.data.size());
		P.r_begin(u_id);
		R_ASSERT(M_UPDATE == u_id);

		clientID.set(0);
		Process_update(P, clientID);
	}
}

void xrServer::SLS_Save(IWriter& fs)
{
	VERIFY(!EngineExternal()[EEngineExternalSystem::AdvancedSerialization]);
	// Generate spawn+update
	NET_Packet P;
	u32 position;
	xrS_entities::iterator I = entities.begin(), E = entities.end();
	for (u32 C = 0; I != E; ++I, ++C)
	{
		CSE_Abstract* E_ = I->second;

		fs.open_chunk(C);

		// Spawn
		E_->Spawn_Write(P, true);
		I_ASSERT_M(P.B.data.size() <= u16(-1), "(Spawn_Write) Object [%s] contains more data than save data limit, current size [%d], max [%d]", E_->name(), P.B.data.size(), u16(-1));
		fs.w_u16(u16(P.B.data.size()));
		fs.w(P.B.data.data(),P.B.data.size());

		// Update
		P.w_begin(M_UPDATE);
		P << E_->ID;
		P.w_chunk_open8(position);
		E_->UPDATE_Write(P);
		P.w_chunk_close8(position);

		I_ASSERT_M(P.B.data.size() <= u16(-1), "(Spawn_Write) Object [%s] contains more data than save data limit, current size [%d], max [%d]", E_->name(), P.B.data.size(), u16(-1));
		fs.w_u16(u16(P.B.data.size()));
		fs.w(P.B.data.data(),P.B.data.size());

		fs.close_chunk();
	}
}

void xrServer::Perform_transfer(NET_Packet& PR, NET_Packet& PT, CSE_Abstract* what, CSE_Abstract* from, CSE_Abstract* to)
{
	// Sanity check
	R_ASSERT(what && from && to);
	R_ASSERT(from != to);
	R_ASSERT(what->ID_Parent == from->ID);
	u32 time = Device.dwTimeGlobal;

	// 2. Detach "FROM"
	auto& C = from->children;
	auto c = std::find(C.begin(), C.end(), what->ID);
	R_ASSERT(C.end() != c);
	C.erase(c);
	PR.w_begin(M_EVENT);
	PR.w_u32(time);
	PR.w_u16(GE_OWNERSHIP_REJECT);
	PR << from->ID;
	PR << what->ID;

	// 3. Attach "TO"
	what->ID_Parent = to->ID;
	to->children.push_back(what->ID);
	PT.w_begin(M_EVENT);
	PT.w_u32(time + 1);
	PT.w_u16(GE_OWNERSHIP_TAKE);
	PT << to->ID;
	PT << what->ID;
}

void xrServer::Perform_reject(CSE_Abstract* what, CSE_Abstract* from, int delta)
{
	R_ASSERT(what && from);
	R_ASSERT(what->ID_Parent == from->ID);

	NET_Packet P;
	u32 time = Device.dwTimeGlobal - delta;

	P.w_begin(M_EVENT);
	P.w_u32(time);
	P.w_u16(GE_OWNERSHIP_REJECT);
	P << from->ID;
	P << what->ID;
	P.w_u8(1);

	Process_event_reject(P, BroadcastCID, time, from->ID, what->ID);
}

void xrServer::Process_event(NET_Packet& P, ClientID sender)
{
#ifdef SLOW_VERIFY_ENTITIES
	VERIFY(verify_entities());
#endif

	u32 timestamp;
	u16 type;
	ALife::_OBJECT_ID destination;
	u32 MODE = net_flags(true, true);

	// correct timestamp with server-unique-time (note: direct message correction)
	P.r_u32(timestamp);

	// read generic info
	P.r_u16(type);
	P >> destination;

	CSE_Abstract* receiver = game->get_entity_from_eid(destination);
	if (receiver)
	{
		R_ASSERT(receiver->owner);
		receiver->OnEvent(P, type, timestamp, sender);
	};

	switch (type)
	{
		case GE_CLEAR_SAVED_BONES:
		{
			auto po = smart_cast<CSE_PHSkeleton*>(receiver);
			if (po != nullptr)
			{
				po->saved_bones.bones.clear();
			}
			break;
		}
		case GE_GAME_EVENT:
		{
			u16 game_event_type;
			P.r_u16(game_event_type);
			game->AddDelayedEvent(P, game_event_type, timestamp, sender);
		}
		break;
		case GE_REPAIR_ITEM:
		{
			CSE_ALifeInventoryItem* iitem = smart_cast<CSE_ALifeInventoryItem*>(receiver);
			if (!iitem)
			{
				break;
			}
			iitem->m_fCondition = 1.0f;
			SendBroadcast(BroadcastCID, P, net_flags(true, true));
		}
		break;
		case GE_INFO_TRANSFER:
		case GE_WPN_STATE_CHANGE:
		case GE_ZONE_STATE_CHANGE:
		case GE_ACTOR_JUMPING:
		case GEG_PLAYER_PLAY_HEADSHOT_PARTICLE:
		case GEG_PLAYER_ATTACH_HOLDER:
		case GEG_PLAYER_DETACH_HOLDER:
		case GEG_PLAYER_ITEM2SLOT:
		case GEG_PLAYER_ITEM2BELT:
		case GEG_PLAYER_ITEM2RUCK:
		case GE_GRENADE_EXPLODE:
		case GE_WPN_UNLOAD_AMMO:
		case GE_WPN_UPDATE_AMMO:
		case GEG_PLAYER_START_HUD_ANIMATOR:
		{
			SendBroadcast(BroadcastCID, P, MODE);
		}
		break;
		case GEG_PLAYER_ACTIVATEARTEFACT:
		{
			Process_event_activate(P, sender, timestamp, destination, P.r_u16(), true);
			break;
		};
		case GE_INV_ACTION:
		{
			xrClientData* CL = ID_to_client(sender);
			if (CL)
			{
				CL->net_Ready = true;
			}
			if (SV_Client)
			{
				SendTo(SV_Client->ID, P, net_flags(true, true));
			}
		}
		break;
		case GE_RESPAWN:
		{
			CSE_Abstract* E = receiver;
			if (E)
			{
				R_ASSERT(E->s_flags.is(M_SPAWN_OBJECT_PHANTOM));

				svs_respawn R;
				R.timestamp = timestamp + E->RespawnTime * 1000;
				R.phantom = destination;
				q_respawn.insert(R);
			}
		}
		break;
		case GE_TRADE_BUY:
		case GE_OWNERSHIP_TAKE:
		{
			Process_event_ownership(P, sender, timestamp, destination);
		}
		break;
		case GE_OWNERSHIP_TAKE_MP_FORCED:
		{
			Process_event_ownership(P, sender, timestamp, destination, true);
		}
		break;
		case GE_TRADE_SELL:
		case GE_OWNERSHIP_REJECT:
		case GE_LAUNCH_ROCKET:
		{
			Process_event_reject(P, sender, timestamp, destination, P.r_u16());
		}
		break;
		case GE_DESTROY:
		{
			Process_event_destroy(P, sender, timestamp, destination, nullptr);
		}
		break;
		case GE_TRANSFER_AMMO:
		{
			ALife::_OBJECT_ID id_entity;
			P >> id_entity;
			CSE_Abstract* e_parent = receiver;							   // ��� �������� (��� ����� ����)
			CSE_Abstract* e_entity = game->get_entity_from_eid(id_entity); // ��� ������
			if (!e_entity)
			{
				break;
			}
			if (ALife::INVALID_OBJECT_ID != e_entity->ID_Parent)
			{
				break; // this item already taken
			}
			xrClientData* c_parent = e_parent->owner;
			xrClientData* c_from = ID_to_client(sender);
			R_ASSERT(c_from == c_parent); // assure client ownership of event

			// Signal to everyone (including sender)
			SendBroadcast(BroadcastCID, P, MODE);

			// Perfrom real destroy
			entity_Destroy(e_entity);
			VERIFY(verify_entities());
		}
		break;
		case GE_HIT:
		case GE_HIT_STATISTIC:
		{
			P.r_pos -= sizeof(ALife::_OBJECT_ID);
			if (type == GE_HIT_STATISTIC)
			{
				P.B.data.resize(P.B.data.size()-4);
				P.w_u32(sender.value());
			};
			game->AddDelayedEvent(P, GAME_EVENT_ON_HIT, 0, ClientID());
		}
		break;
		case GE_ASSIGN_KILLER:
		{
			ALife::_OBJECT_ID id_src;
			P >> id_src;

			CSE_Abstract* e_dest = receiver; // ��� ����
			// this is possible when hit event is sent before destroy event
			if (!e_dest)
			{
				break;
			}

			CSE_ALifeCreatureAbstract* creature = smart_cast<CSE_ALifeCreatureAbstract*>(e_dest);
			if (creature)
			{
				creature->set_killer_id(id_src);
			}

			break;
		}
		case GE_CHANGE_VISUAL:
		{
			CSE_Visual* visual = smart_cast<CSE_Visual*>(receiver);
			VERIFY(visual);
			string256 tmp;
			P.r_stringZ(tmp);
			visual->set_visual(tmp);
		}
		break;
		case GE_DIE:
		{
			// Parse message
			ALife::_OBJECT_ID id_dest = destination, id_src;
			P >> id_src;


			xrClientData* l_pC = ID_to_client(sender);
			VERIFY(game && l_pC);
#ifndef MASTER_GOLD
			if ((game->Type() != eGameIDSingle) && l_pC && l_pC->owner)
			{
				Msg("* [%2d] killed by [%2d] - sended by [0x%08x]", id_dest, id_src, l_pC->ID.value());
			}
#endif // #ifndef MASTER_GOLD

			CSE_Abstract* e_dest = receiver; // ��� ����
			// this is possible when hit event is sent before destroy event
			if (!e_dest)
			{
				break;
			}

#ifndef MASTER_GOLD
			if (game->Type() != eGameIDSingle)
			{
				Msg("* [%2d] is [%s:%s]", id_dest, *e_dest->s_name, e_dest->name_replace());
			}
#endif // #ifndef MASTER_GOLD

			CSE_Abstract* e_src = game->get_entity_from_eid(id_src); // ��� ����
			if (!e_src)
			{
				xrClientData* C = (xrClientData*)game->get_client(id_src);
				if (C)
				{
					e_src = C->owner;
				}
			};
			VERIFY(e_src);
			if (!e_src)
			{
				Msg("! ERROR: SV: src killer not exist.");
				return;
			}

#ifndef MASTER_GOLD
			if (game->Type() != eGameIDSingle)
			{
				Msg("* [%2d] is [%s:%s]", id_src, *e_src->s_name, e_src->name_replace());
			}
#endif // #ifndef MASTER_GOLD

			game->on_death(e_dest, e_src);

			xrClientData* c_src = e_src->owner; // ������, ��� ���� ����

			if (c_src->owner->ID == id_src)
			{
				// Main unit
				P.w_begin(M_EVENT);
				P.w_u32(timestamp);
				P.w_u16(type);
				P << destination;
				P << id_src;
				P.w_clientID(c_src->ID);
			}

			SendBroadcast(BroadcastCID, P, MODE);

			//////////////////////////////////////////////////////////////////////////
			//
			if (game->Type() == eGameIDSingle)
			{
				P.w_begin(M_EVENT);
				P.w_u32(timestamp);
				P.w_u16(GE_KILL_SOMEONE);
				P << id_src;
				P << destination;
				SendTo(c_src->ID, P, net_flags(true, true));
			}
			//////////////////////////////////////////////////////////////////////////

			VERIFY(verify_entities());
		}
		break;
		case GE_ADDON_ATTACH:
		case GE_ADDON_DETACH:
		{
			SendBroadcast(BroadcastCID, P, net_flags(true, true));
		}
		break;
		case GE_PSEUDO_GIGANT_KICK:
		case GE_BURER_GRAVI_PARTICLES:
		case GE_BURER_GRAVI_WAVE:
		case GE_BURER_SHIELD:
		case GE_BURER_SHIELD_HIT:
		case GE_BLOODSUCKER_VAMPIRE_START:
		case GE_BLOODSUCKER_VAMPIRE_STOP:
		case GE_CONTROLLER_PSY_FIRE:
		{
			SendTo(SV_Client->ID, P, net_flags(true, true));
		}
		break;
		case GE_CHANGE_POS:
		{
			SendTo(SV_Client->ID, P, net_flags(true, true));
		}
		break;
		case GE_INSTALL_UPGRADE:
		{
			shared_str upgrade_id;
			P.r_stringZ(upgrade_id);
			CSE_ALifeInventoryItem* iitem = smart_cast<CSE_ALifeInventoryItem*>(receiver);
			if (!iitem)
			{
				break;
			}
			iitem->add_upgrade(upgrade_id);
		}
		break;
		case GE_INV_BOX_STATUS:
		{
			u8 can_take, closed;
			P.r_u8(can_take);
			P.r_u8(closed);
			shared_str tip_text;
			P.r_stringZ(tip_text);

			CSE_ALifeInventoryBox* box = smart_cast<CSE_ALifeInventoryBox*>(receiver);
			if (!box)
			{
				break;
			}
			box->m_can_take = (can_take == 1);
			box->m_closed = (closed == 1);
			box->m_tip_text._set(tip_text);
		}
		break;
		case GE_INV_OWNER_STATUS:
		{
			u8 can_take, closed;
			P.r_u8(can_take);
			P.r_u8(closed);

			CSE_ALifeTraderAbstract* iowner = smart_cast<CSE_ALifeTraderAbstract*>(receiver);
			if (!iowner)
			{
				break;
			}
			iowner->m_deadbody_can_take = (can_take == 1);
			iowner->m_deadbody_closed = (closed == 1);
		}
		break;

		case GEG_PLAYER_DISABLE_SPRINT:
		case GEG_PLAYER_WEAPON_HIDE_STATE:
		{
			SendTo(SV_Client->ID, P, net_flags(true, true));

#ifdef SLOW_VERIFY_ENTITIES
			VERIFY(verify_entities());
#endif
		}
		break;
		case GEG_PLAYER_ACTIVATE_SLOT:
		case GEG_PLAYER_ITEM_EAT:
		{
			SendTo(SV_Client->ID, P, net_flags(true, true));
#ifdef SLOW_VERIFY_ENTITIES
			VERIFY(verify_entities());
#endif
		}
		break;
		case GEG_PLAYER_USE_BOOSTER:
		{
			if (receiver && receiver->owner && (receiver->owner != SV_Client))
			{
				NET_Packet tmp_packet;
				CGameObject::u_EventGen(tmp_packet, GEG_PLAYER_USE_BOOSTER, receiver->ID);
				SendTo(receiver->owner->ID, P, net_flags(true, true));
			}
		}
		break;
		case GEG_PLAYER_ITEM_SELL:
		{
			game->OnPlayer_Sell_Item(sender, P);
		}
		break;
		case GE_TELEPORT_OBJECT:
		{
			game->teleport_object(P, destination);
		}
		break;
		case GE_ADD_RESTRICTION:
		{
			game->add_restriction(P, destination);
		}
		break;
		case GE_REMOVE_RESTRICTION:
		{
			game->remove_restriction(P, destination);
		}
		break;
		case GE_REMOVE_ALL_RESTRICTIONS:
		{
			game->remove_all_restrictions(P, destination);
		}
		break;
		case GE_SYNC_ALIFEITEM:
		{
			CSE_ALifeItem* item = smart_cast<CSE_ALifeItem*>(receiver);
			if (item)
			{
				item->m_fCondition = P.r_float();
			}
		}
		break;
		case GE_MONEY:
		{
			CSE_Abstract* e_dest = receiver;
			CSE_ALifeTraderAbstract* pTa = smart_cast<CSE_ALifeTraderAbstract*>(e_dest);
			if (pTa != nullptr)
			{
				pTa->m_dwMoney = P.r_u32();
			}
			if (game->Type() != eGameIDSingle)
			{
				SendBroadcast(BroadcastCID, P, MODE);
			}
		}
		break;
		case GE_STALKER_ANIMATION:
		case GE_STALKER_DIALOG:
			SendBroadcast(BroadcastCID, P, MODE);
			break;
		case GE_FREEZE_OBJECT:
			break;
		case GE_REQUEST_PLAYERS_INFO:
		{
			SendPlayersInfo(sender);
		}
		break;
		default:
			Msg("! Game event [%u] is not implemented!", type);
			break;
	}
}

xr_string xrServer::ent_name_safe(ALife::_OBJECT_ID eid)
{
	string1024 buff;
	CSE_Abstract* e_dest = game->get_entity_from_eid(eid);
	if (e_dest)
	{
		xr_sprintf(buff, "[%d][%s:%s]", eid, e_dest->name(), e_dest->name_replace());
	}
	else
	{
		xr_sprintf(buff, "[%d][%s]", eid, "NOTFOUND");
	}

	return buff;
}

void xrServer::Process_event_destroy(NET_Packet& P, ClientID sender, u32 time, ALife::_OBJECT_ID ID, NET_Packet* pEPack)
{
	u32 MODE = net_flags(true, true);
	// Parse message
	auto id_dest = ID;
#ifdef DEBUG
	if (dbg_net_Draw_Flags.test(dbg_destroy))
	{
		Msg("sv destroy object %s [%d]", ent_name_safe(id_dest).c_str(), Device.dwFrame);
	}
#endif

	CSE_Abstract* e_dest = game->get_entity_from_eid(id_dest); // ��� ������ ���� ���������
	if (!e_dest)
	{
#ifndef MASTER_GOLD
		Msg("!SV:ge_destroy: [%d] not found on server", id_dest);
#endif // #ifndef MASTER_GOLD
		return;
	};

	R_ASSERT(e_dest);
	xrClientData* c_dest = e_dest->owner; // ������, ��� ����
	R_ASSERT(c_dest);
	xrClientData* c_from = ID_to_client(sender); // ������, ��� �������
	R_ASSERT(c_dest == c_from);					 // assure client ownership of event
	auto parent_id = e_dest->ID_Parent;

	//---------------------------------------------
	NET_Packet P2, *pEventPack = pEPack;
	P2.w_begin(M_EVENT_PACK);
	//---------------------------------------------
	// check if we have children
	if (!e_dest->children.empty())
	{
		if (!pEventPack)
		{
			pEventPack = &P2;
		}

		while (!e_dest->children.empty())
		{
			Process_event_destroy(P, sender, time, *e_dest->children.begin(), pEventPack);
		}
	};

	if (ALife::INVALID_OBJECT_ID == parent_id && nullptr == pEventPack)
	{
		SendBroadcast(BroadcastCID, P, MODE);
	}
	else
	{
		NET_Packet tmpP;
		if (ALife::INVALID_OBJECT_ID != parent_id && Process_event_reject(P, sender, time, parent_id, ID, false))
		{
			game->u_EventGen(tmpP, GE_OWNERSHIP_REJECT, parent_id);
			tmpP << id_dest;
			tmpP.w_u8(1);

			if (!pEventPack)
			{
				pEventPack = &P2;
			}

			pEventPack->w_u8(u8(tmpP.B.data.size()));
			pEventPack->w(tmpP.B.data.data(), tmpP.B.data.size());
		};

		game->u_EventGen(tmpP, GE_DESTROY, id_dest);

		pEventPack->w_u8(u8(tmpP.B.data.size()));
		pEventPack->w(tmpP.B.data.data(), tmpP.B.data.size());
	};

	if (nullptr == pEPack && nullptr != pEventPack)
	{
		SendBroadcast(BroadcastCID, *pEventPack, MODE);
	}

	// Everything OK, so perform entity-destroy
	if (e_dest->m_bALifeControl && ai().get_alife())
	{
		if (ai().alife().objects().object(id_dest, true))
		{
			if (IsGameTypeSingle())
			{
				game_sv_Single* _gameS = game->cast_game_sv_single();
				_gameS->alife().release(e_dest, false);
			}
			else
			{
				game_sv_freemp* _gameM = game->cast_game_sv_freemp();
				_gameM->alife().release(e_dest, false);
			}
		}
	}

	if (game)
	{
		game->OnDestroyObject(e_dest->ID);
	}

	entity_Destroy(e_dest);
}


bool TestObjectValidOnSvClient(ALife::_OBJECT_ID id_entity)
{
	CObject* tmp_obj = Level().Objects.net_Find(id_entity);
	if (!tmp_obj)
	{
		return false;
	}

	CGameObject* tmp_gobj = tmp_obj->cast_game_object();
	if (!tmp_gobj)
	{
		return false;
	}

	if (tmp_obj->getDestroy())
	{
		return false;
	}

	if (tmp_gobj->object_removed())
	{
		return false;
	}

	return true;
}

void ReplaceOwnershipHeader(NET_Packet& P)
{
	// ������ ����� ������, �� �� ������ ������ ����� ������ ���. ������� ������� ���������
	u16 NewType = GE_OWNERSHIP_TAKE;
	CopyMemory(P.B.data.data()+6,&NewType,2);
};

void xrServer::Process_event_ownership(NET_Packet& P, ClientID sender, u32 time, ALife::_OBJECT_ID ID, bool bForced)
{
	u32 MODE = net_flags(true, true, false, true);

	ALife::_OBJECT_ID id_parent = ID, id_entity;
	P >> id_entity;
	CSE_Abstract* e_parent = game->get_entity_from_eid(id_parent);
	CSE_Abstract* e_entity = game->get_entity_from_eid(id_entity);

	if (!e_parent)
	{
		Msg("! ERROR on ownership: parent not found. parent_id = [%d], entity_id = [%d], frame = [%d].", id_parent, id_entity, Device.dwFrame);
		return;
	}

	if (!e_entity)
	{
		return;
	}

	if (!TestObjectValidOnSvClient(id_parent))
	{
		Msg("! ERROR on ownership: parent object is not valid on sv client. parent_id = [%d], entity_id = [%d], frame = [%d]", id_parent, id_entity, Device.dwFrame);
		return;
	}

	if (!TestObjectValidOnSvClient(id_entity))
	{
		Msg("! ERROR on ownership: entity object is not valid on sv client. parent_id = [%d], entity_id = [%d], frame = [%d]", id_parent, id_entity, Device.dwFrame);
		return;
	}

	if (ALife::INVALID_OBJECT_ID != e_entity->ID_Parent)
	{
		return;
	}

	xrClientData* c_parent = e_parent->owner;
	xrClientData* c_entity = e_entity->owner;
	xrClientData* c_from = ID_to_client(sender);

	if (game->Type() == eGameIDSingle && (GetServerClient() != c_from) && (c_parent != c_from))
	{
		// trust only ServerClient or new_ownerClient
		return;
	}

	CSE_ALifeCreatureAbstract* alife_entity = smart_cast<CSE_ALifeCreatureAbstract*>(e_parent);
	if (alife_entity && !alife_entity->g_Alive() && !IsGameTypeSingleCompatible())
	{
		return;
	};

	// Game allows ownership of entity
	if (game->OnTouch(id_parent, id_entity, bForced))
	{
		// Rebuild parentness
		e_entity->ID_Parent = id_parent;
		e_parent->children.push_back(id_entity);

		if (bForced)
		{
			ReplaceOwnershipHeader(P);
		}
		// Signal to everyone (including sender)
		SendBroadcast(BroadcastCID, P, MODE);
	}
}

bool xrServer::Process_event_reject(NET_Packet& P, const ClientID sender, const u32 time, const ALife::_OBJECT_ID id_parent, const ALife::_OBJECT_ID id_entity, bool send_message)
{
	// Parse message
	CSE_Abstract* e_parent = game->get_entity_from_eid(id_parent);
	CSE_Abstract* e_entity = game->get_entity_from_eid(id_entity);

	VERIFY2(e_entity, make_string<const char*>("entity not found. parent_id = [%d], entity_id = [%d], frame = [%d]", id_parent, id_entity, Device.dwFrame));
	if (!e_entity)
	{
		Msg("! ERROR on rejecting: entity not found. parent_id = [%d], entity_id = [%d], frame = [%d].", id_parent, id_entity, Device.dwFrame);
		return false;
	}

	VERIFY2(e_parent, make_string<const char*>("parent not found. parent_id = [%d], entity_id = [%d], frame = [%d]", id_parent, id_entity, Device.dwFrame));
	if (!e_parent)
	{
		Msg("! ERROR on rejecting: parent not found. parent_id = [%d], entity_id = [%d], frame = [%d].", id_parent, id_entity, Device.dwFrame);
		return false;
	}

	auto& C = e_parent->children;
	auto c = std::find(C.begin(), C.end(), id_entity);
	if (c == C.end())
	{
		Msg("! ERROR: SV: can't find children [%d] of parent [%d]", id_entity, e_parent);
		return false;
	}

	if (ALife::INVALID_OBJECT_ID == e_entity->ID_Parent)
	{
#ifndef MASTER_GOLD
		Msg("! ERROR: can't detach independant object. entity[%s][%d], parent[%s][%d], section[%s]",
			e_entity->name_replace(),
			id_entity,
			e_parent->name_replace(),
			id_parent,
			e_entity->s_name.c_str());
#endif // #ifndef MASTER_GOLD
		g_pScriptEngine->print_stack();
		return (false);
	}

	// Rebuild parentness
	if (e_entity->ID_Parent != id_parent)
	{
		Msg("! ERROR: e_entity->ID_Parent = [%d]  parent = [%d][%s]  entity_id = [%d]  frame = [%d]",
			e_entity->ID_Parent,
			id_parent,
			e_parent->name_replace(),
			id_entity,
			Device.dwFrame);
		// it can't be !!!
	}

	game->OnDetach(id_parent, id_entity);

	e_entity->ID_Parent = ALife::INVALID_OBJECT_ID;

	if (auto IdToErase = std::find(C.begin(), C.end(), id_entity); IdToErase != C.end())
	{
		C.erase(IdToErase);
	}

	// Signal to everyone (including sender)
	if (send_message)
	{
		DWORD MODE = net_flags(true, true, false, true);
		SendBroadcast(BroadcastCID, P, MODE);
	}

	return (true);
}

CSE_Abstract* xrServer::Process_spawn(NET_Packet& P, ClientID sender, bool bSpawnWithClientsMainEntityAsParent, CSE_Abstract* tpExistedEntity)
{
	// create server entity
	xrClientData* CL = ID_to_client(sender);
	CSE_Abstract* E = tpExistedEntity;
	if (!E)
	{
		// read spawn information
		string64 s_name;
		P.r_stringZ(s_name);
		// create entity
		E = entity_Create(s_name);
		R_ASSERT3(E, "Can't create entity.", s_name);
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			xr_unique_ptr<CSaveObjectLoad> Obj;
			Obj.reset(SaveObjectNetPacketHelper::GetLoadObjectFromPacket(P));
			E->Spawn_Serialize(*Obj);
		}
		else
		{
			E->Spawn_Read(P);
		}
		if (
			!E->m_gameType.MatchType((u16)game->Type()) ||
			!E->match_configuration() ||
			!game->OnPreCreate(E)
		)
		{
			F_entity_Destroy(E);
			return nullptr;
		}
	}
	else
	{
		VERIFY(E->m_bALifeControl);
	}

	CSE_Abstract* e_parent = 0;
	if (E->ID_Parent != ALife::INVALID_OBJECT_ID)
	{
		e_parent = ID_to_entity(E->ID_Parent);
		if (!e_parent)
		{
			R_ASSERT(!tpExistedEntity);
			F_entity_Destroy(E);
			return nullptr;
		}
	}

	// check if we can assign entity to some client
	if (0 == CL)
	{
		CL = (xrClientData*)SV_Client;
	}

	// check for respawn-capability and create phantom as needed
	if (E->RespawnTime && (ALife::INVALID_OBJECT_ID == E->ID_Phantom))
	{
		// Create phantom
		CSE_Abstract* Phantom = entity_Create(*E->s_name);
		R_ASSERT(Phantom);
		Phantom->Spawn_Read(P);
		Phantom->ID = PerformIDgen(ALife::INVALID_OBJECT_ID);
		Phantom->ID_Phantom = Phantom->ID; // Self-linked to avoid phantom-breeding
		Phantom->owner = nullptr;
		entities.insert(std::make_pair(Phantom->ID, Phantom));

		Phantom->s_flags.set(M_SPAWN_OBJECT_PHANTOM, true);

		// Spawn entity
		E->ID = PerformIDgen(E->ID);
		E->ID_Phantom = Phantom->ID;
		E->owner = CL;
		entities.insert(std::make_pair(E->ID, E));
	}
	else
	{
		if (E->s_flags.is(M_SPAWN_OBJECT_PHANTOM))
		{
			// Clone from Phantom
			E->ID = PerformIDgen(ALife::INVALID_OBJECT_ID);
			E->owner = CL; //		= SelectBestClientToMigrateTo	(E);
			E->s_flags.set(M_SPAWN_OBJECT_PHANTOM, false);
			entities.insert(std::make_pair(E->ID, E));
		}
		else
		{
			// Simple spawn
			if (bSpawnWithClientsMainEntityAsParent)
			{
				R_ASSERT(CL);
				CSE_Abstract* P_ = CL->owner;
				R_ASSERT(P_);
				E->ID_Parent = P_->ID;
			}
			E->ID = PerformIDgen(E->ID);
			E->owner = CL;
			entities.insert(std::make_pair(E->ID, E));
		}
	}

	// PROCESS NAME; Name this entity
	if (CL && (E->s_flags.is(M_SPAWN_OBJECT_ASPLAYER)))
	{
		CL->owner = E;
	}

	E->s_RP = 0xFE; // Use supplied

	// Parent-Connect
	if (!tpExistedEntity)
	{
		game->OnCreate(E->ID);

		if (ALife::INVALID_OBJECT_ID != E->ID_Parent)
		{
			R_ASSERT(e_parent);

			game->OnTouch(E->ID_Parent, E->ID);

			e_parent->children.push_back(E->ID);
		}
	}

	// create packet and broadcast packet to everybody
	NET_Packet Packet;
	if (CL)
	{
		// For local ONLY
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacketPossibleFull(Packet, *E);
		}
		else
		{
			E->Spawn_Write(Packet, true);
			if (E->s_flags.is(M_SPAWN_UPDATE))
			{
				E->UPDATE_Write(Packet);
			}
		}
		SendTo(CL->ID, Packet, net_flags(true, true));

		// For everybody, except client, which contains authorative copy
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacketPossibleFull(Packet, *E);
		}
		else
		{
			E->Spawn_Write(Packet, false);
			if (E->s_flags.is(M_SPAWN_UPDATE))
			{
				E->UPDATE_Write(Packet);
			}
		}
		SendBroadcast(CL->ID, Packet, net_flags(true, true));
	}
	else
	{
		if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
		{
			SaveObjectNetPacketHelper::PrepareLocalSpawnPacketPossibleFull(Packet, *E);
		}
		else
		{
			E->Spawn_Write(Packet, false);
			if (E->s_flags.is(M_SPAWN_UPDATE))
			{
				E->UPDATE_Write(Packet);
			}
		}
		ClientID clientID;
		clientID.set(0);
		SendBroadcast(clientID, Packet, net_flags(true, true));
	}
	if (!tpExistedEntity)
	{
		game->OnPostCreate(E->ID);
	}

	return E;
}

void xrServer::Process_update(NET_Packet& P, ClientID sender)
{
	xrClientData* CL = ID_to_client(sender);
	R_ASSERT2(CL, "Process_update client not found");
	R_ASSERT(CL->flags.bLocal);

	// while has information
	while (!P.r_eof())
	{
		// find entity
		ALife::_OBJECT_ID ID;
		u8 size;

		P >> ID;
		P.r_u8(size);
		u32 _pos = P.r_tell();
		CSE_Abstract* E = ID_to_entity(ID);

		if (E)
		{
			E->net_Ready = true;
			E->UPDATE_Read(P);

			if (g_pGamePersistent->GameType() == eGameIDFreeMP)
			{
				E->SyncRead(P);
			}

			if ((P.r_tell() - _pos) != size)
			{
				string16 tmp;
				CLSID2TEXT(E->m_tClassID, tmp);
				Debug.fatal(DEBUG_INFO, "Beer from the creator of '%s'; initiator: 0x%08x, r_tell() = %d, pos = %d, objectID = %d", tmp, CL->ID.value(), P.r_tell(), _pos, E->ID);
			}
		}
		else
		{
			P.r_advance(size);
		}
	}
}

void xrServer::Process_save(NET_Packet& P, ClientID sender)
{
	xrClientData* CL = ID_to_client(sender);
	R_ASSERT2(CL, "Process_save client not found");
	CL->net_Ready = true;

	R_ASSERT(CL->flags.bLocal);
	// while has information
	while (!P.r_eof())
	{
		// find entity
		ALife::_OBJECT_ID ID;
		u16 size;

		P >> ID;
		P.r_u16(size);
		s32 _pos_start = P.r_tell();
		CSE_Abstract* E = ID_to_entity(ID);

		if (E)
		{
			E->net_Ready = true;
			E->load(P);
		}
		else
		{
			P.r_advance(size);
		}
		s32 _pos_end = P.r_tell();
		s32 _size = size;
		if (_size != (_pos_end - _pos_start))
		{
			Msg("! load/save mismatch, object: '%s'", E ? E->name_replace() : "unknown");
			s32 _rollback = _pos_start + _size;
			P.r_seek(_rollback);
		}
	}
}

#ifdef DEBUG
static size_t debug_num = 0;
static xr_set<ALife::_OBJECT_ID> UsedIDsDebug = {};

void xrServer::VerifyIDDebug()
{
	size_t result = 0;
	for (auto& elem1 : m_id_chunks)
	{
		for (auto& elem2 : elem1->data)
		{
			for (auto& elem3 : elem2.pack)
			{
				result += elem3.bset.n1;
				result += elem3.bset.n2;
				result += elem3.bset.n3;
				result += elem3.bset.n4;
				result += elem3.bset.n5;
				result += elem3.bset.n6;
				result += elem3.bset.n7;
				result += elem3.bset.n8;
			}
		}
	}
	VERIFY(result == debug_num);
}
#endif

void xrServer::clear_ids()
{
	xrCriticalSectionGuard g(m_id_chunksCS);
	m_id_chunks.clear();
#ifdef DEBUG
	debug_num = 0;
	UsedIDsDebug.clear();
#endif
}

ALife::_OBJECT_ID xrServer::PerformIDgen(ALife::_OBJECT_ID ID)
{
	xrCriticalSectionGuard g(m_id_chunksCS);
	// Clean-up too old pending ID
	if (m_id_chunks.empty()) // whatever the reason is, this means that all IDs (including pending) are invalidated
	{
		m_pending_delete_id_set.clear();
		while (!m_pending_delete_id_queue.empty()){
			m_pending_delete_id_queue.pop();
		}
	}
	else
	{
		IVERIFY(m_pending_delete_id_queue.size() >= m_pending_delete_id_set.size());
		while (!m_pending_delete_id_queue.empty())
		{
			auto& elem = m_pending_delete_id_queue.front();
			auto CurTime = Device.TimerAsync();
			if(elem.first + ID_delete_delay > CurTime)
			{
				//Msg("Stop ID [%u] free because timeout is not ready [current time: %u; queue time: %u; timeout: %u]", elem.second, CurTime, elem.first, ID_delete_delay);
				break;
			}
			//Msg("Free ID [%u] because timeout is ready [current time: %u; queue time: %u; timeout: %u]", elem.second, CurTime, elem.first, ID_delete_delay);
			if (m_pending_delete_id_set.contains(elem.second))
			{
				FreeIDImpl(elem.second);
				m_pending_delete_id_set.erase(elem.second);
			}
			m_pending_delete_id_queue.pop();
		}
	}
	
	// ID generation itself
	auto Result = ALife::INVALID_OBJECT_ID;
	size_t i1 = ID/i1Shift;
	size_t i2 = (ID%i1Shift)/i2Shift;
	size_t i3 = ((ID%i1Shift)%i2Shift)/i3Shift;
	const u8 Mod = ((ID%i1Shift)%i2Shift)%i3Shift;
	if (ID == ALife::INVALID_OBJECT_ID)
	{
		for (i1 = 0; i1 < m_id_chunks.size(); ++i1)
		{
			VERIFY(i1 < m_id_chunks.size());
			auto& Chunk = *m_id_chunks[i1];
			if (!Chunk.empty)
			{
				continue;
			}
			for (i2 = 0; i2 < 255; ++i2)
			{
				auto& LLChunk = Chunk.data[i2];
				if (!LLChunk.empty)
				{
					continue;
				}
				for (i3 = 0; i3 < 255; ++i3)
				{
					auto& Pack = LLChunk.pack[i3];
					if (Pack.set == 255)
					{
						continue;
					}
					auto CalcID = [&](u8 shift) -> ALife::_OBJECT_ID
					{
						return ALife::_OBJECT_ID(i1*i1Shift)
							+ ALife::_OBJECT_ID(i2*i2Shift)
							+ ALife::_OBJECT_ID(i3*i3Shift)
							+ shift;
					};
#ifdef DEBUG
#define PackAcquire(num) \
	if(!Pack.bset.n##num){ \
		Result = CalcID(num-1);\
		VERIFY(!UsedIDsDebug.contains(Result)); \
		Pack.bset.n##num = true; \
	}
#else
#define PackAcquire(num) \
	if(!Pack.bset.n##num){ \
	Result = CalcID(num-1);\
	Pack.bset.n##num = true; \
}
#endif
					PackAcquire(1)
					else PackAcquire(2)
					else PackAcquire(3)
					else PackAcquire(4)
					else PackAcquire(5)
					else PackAcquire(6)
					else PackAcquire(7)
					else PackAcquire(8)
#undef PackAcquire
					if (IVERIFY(Result != ALife::INVALID_OBJECT_ID))
					{
						if (Pack.set == 255)
						{
							IVERIFY(LLChunk.empty);
							--LLChunk.empty;
							if (!LLChunk.empty)
							{
								IVERIFY(Chunk.empty);
								--Chunk.empty;
							}
						}
						break;
					}
				}
				if (IVERIFY(Result != ALife::INVALID_OBJECT_ID))
				{
					break;
				}
			}
			if (IVERIFY(Result != ALife::INVALID_OBJECT_ID))
			{
				break;
			}
		}
		if (Result == ALife::INVALID_OBJECT_ID)
		{
			m_id_chunks.push_back(xr_make_unique<IDChunkSet>());
			auto& Chunk = *m_id_chunks.back();
			auto& LLChunk = Chunk.data[0];
			auto& Pack = LLChunk.pack[0];
			Pack.bset.n1 = true;
			Result = i1Shift*(m_id_chunks.size()-1);
			VERIFY(!UsedIDsDebug.contains(Result));
		}
		R_ASSERT(Result != ALife::INVALID_OBJECT_ID);
		if (Result > m_TopValidID || m_TopValidID == ALife::INVALID_OBJECT_ID)
		{
			m_TopValidID = Result;
		}
#ifdef DEBUG
		debug_num++;
		VerifyIDDebug();
		UsedIDsDebug.insert(Result);
#endif
		return Result;
	}
	{
		if (m_pending_delete_id_set.contains(ID))
		{
			m_pending_delete_id_set.erase(ID); // if we here, we need this ID right now, suppose it's safe to use
			return ID; // we haven't changed storage state for this ID, we can skip update and just return id
		}
		while (i1 >= m_id_chunks.size())
		{
			m_id_chunks.push_back(xr_make_unique<IDChunkSet>());
		}
		auto& Chunk = *(m_id_chunks[i1]);
		auto& LLChunk = Chunk.data[i2];
		auto& Pack = LLChunk.pack[i3];
		if (I_ASSERT_M(!(Pack.set & (1 << Mod)), "ID [%d] is already used!", ID))
		{
			Pack.set |= 1 << Mod;
			Result = ID;
		}
		if (Pack.set == 255)
		{
			IVERIFY(LLChunk.empty);
			--LLChunk.empty;
		}
		if (!LLChunk.empty)
		{
			IVERIFY(Chunk.empty);
			--Chunk.empty;
		}
	}
#ifdef DEBUG
	debug_num++;
	VerifyIDDebug();
	UsedIDsDebug.insert(Result);
#endif
	IVERIFY(ID == Result);
	if (Result > m_TopValidID || m_TopValidID == ALife::INVALID_OBJECT_ID)
	{
		m_TopValidID = Result;
	}
	return Result;
}

void xrServer::FreeID(ALife::_OBJECT_ID ID, u32 time)
{
	xrCriticalSectionGuard g(m_id_chunksCS);
	//Msg("Put ID [%u] in delay free [current time: %u]", ID, time);
	m_pending_delete_id_set.insert(ID);
	m_pending_delete_id_queue.emplace(time, ID);
}

ALife::_OBJECT_ID xrServer::TopValidID() const
{
	R_ASSERT(m_TopValidID != ALife::INVALID_OBJECT_ID);
	return m_TopValidID + 1;
}

void xrServer::FreeIDImpl(ALife::_OBJECT_ID ID)
{
	size_t i1 = ID/i1Shift;
	size_t i2 = (ID%i1Shift)/i2Shift;
	size_t i3 = ((ID%i1Shift)%i2Shift)/i3Shift;
	u8 Mod = ((ID%i1Shift)%i2Shift)%i3Shift;
	if (IVERIFY(i1 < m_id_chunks.size()))
	{
		auto& Chunk = *m_id_chunks[i1];
		auto& LLChunk = Chunk.data[i2];
		auto& Pack = LLChunk.pack[i3];
		bool PackBecameFree = Pack.set == 255;
		bool LLChunkBecameFree = !LLChunk.empty;
		if (IVERIFY((Pack.set & (1 << Mod))))
		{
			Pack.set &= ~(1 << Mod);
		}
		if (PackBecameFree)
		{
			++LLChunk.empty;
		}
		if (LLChunkBecameFree)
		{
			++Chunk.empty;
		}
	}
	
#ifdef DEBUG
	debug_num--;
	VerifyIDDebug();
	UsedIDsDebug.erase(ID);
#endif
}

#ifdef DEBUG
bool xrServer::IsIDUsed(ALife::_OBJECT_ID ID)
{
	xrCriticalSectionGuard g(m_id_chunksCS);
	size_t i1 = ID/i1Shift;
	size_t i2 = (ID%i1Shift)/i2Shift;
	size_t i3 = ((ID%i1Shift)%i2Shift)/i3Shift;
	u8 Mod = ((ID%i1Shift)%i2Shift)%i3Shift;
	if (IVERIFY(i1 < m_id_chunks.size()))
	{
		auto& Chunk = *m_id_chunks[i1];
		IVERIFY(Chunk.empty != 255);
		auto& LLChunk = Chunk.data[i2];
		IVERIFY(LLChunk.empty != 255);
		auto& Pack = LLChunk.pack[i3];
		if (IVERIFY((Pack.set & (1 << Mod))))
		{
			return Pack.set & (1 << Mod);
		}
	}
	return false;
}
#endif

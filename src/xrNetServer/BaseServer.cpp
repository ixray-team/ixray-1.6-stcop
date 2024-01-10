#include "stdafx.h"
#include "BaseServer.h"
#include "IBannedClient.h"
#include "IClient.h"
#include "NET_Log.h"


XRNETSERVER_API ClientID BroadcastCID(0xffffffff);
XRNETSERVER_API int psNET_ServerUpdate = 30; // FPS
XRNETSERVER_API int psNET_ServerPending = 3;
XRNETSERVER_API BOOL psNET_direct_connect = FALSE;

// -----------------------------------------------------------------------------

static	INetLog* pSvNetLog = NULL;

// -----------------------------------------------------------------------------

BaseServer::BaseServer(CTimer* timer, BOOL	Dedicated)
	: m_bDedicated(Dedicated)
#ifdef PROFILE_CRITICAL_SECTIONS
	, csMessage(MUTEX_PROFILE_ID(BaseServer::csMessage))
	, csMessagesQueue(MUTEX_PROFILE_ID(BaseServer::csMessagesQueue))
#endif // PROFILE_CRITICAL_SECTIONS
{
	SV_Client = NULL;
	device_timer = timer;
	stats.clear();
	stats.dwSendTime = TimeGlobal(device_timer);

	pSvNetLog = NULL;//xr_new<INetLog>("logs\\net_sv_log.log", TimeGlobal(device_timer));

#ifdef DEBUG
	sender_functor_invoked = false;
#endif
}

// -----------------------------------------------------------------------------

BaseServer::~BaseServer()
{
	for (u32 it = 0; it < BannedAddresses.size(); it++)
		xr_delete(BannedAddresses[it]);

	BannedAddresses.clear();

	SV_Client = NULL;
	psNET_direct_connect = FALSE;

	xr_delete(pSvNetLog);
}

// -----------------------------------------------------------------------------

IClient* BaseServer::ID_to_client(ClientID ID, bool ScanAll)
{
	if (0 == ID.value())			return NULL;
	IClient* ret_client = GetClientByID(ID);
	if (ret_client || !ScanAll)
		return ret_client;

	return NULL;
}

// -----------------------------------------------------------------------------

#pragma region connect / disconnect

void BaseServer::ParseConnectionOptions(LPCSTR options, ServerConnectionOptions& out)
{
	// SESSION NAME	
	xr_strcpy(out.session_name, options); //sertanly we can use game_descr structure for determinig level_name,
												 // but for backward compatibility we save next line...
	if (strchr(out.session_name, '/'))	*strchr(out.session_name, '/') = 0;

	// PASSWORD
	if (strstr(options, "psw="))
	{
		const char* PSW = strstr(options, "psw=") + 4;
		if (strchr(PSW, '/'))
			strncpy_s(out.server_pass, PSW, strchr(PSW, '/') - PSW);
		else
			strncpy_s(out.server_pass, PSW, 63);
	}

	// MAX PLAYERS
	if (strstr(options, "maxplayers="))
	{
		const char* sMaxPlayers = strstr(options, "maxplayers=") + 11;
		string64 tmpStr = "";
		if (strchr(sMaxPlayers, '/'))
			strncpy_s(tmpStr, sMaxPlayers, strchr(sMaxPlayers, '/') - sMaxPlayers);
		else
			strncpy_s(tmpStr, sMaxPlayers, 63);
		out.dwMaxPlayers = atol(tmpStr);
	}

	if (out.dwMaxPlayers > GetMaxPlayers() || out.dwMaxPlayers < 1)
	{
		out.dwMaxPlayers = GetMaxPlayers();
	}

	// SERVER PORT
	out.bPortWasSet = false;
	out.dwServerPort = START_PORT_LAN_SV;
	if (strstr(options, "portsv="))
	{
		const char* ServerPort = strstr(options, "portsv=") + 7;
		string64 tmpStr = "";
		if (strchr(ServerPort, '/'))
			strncpy_s(tmpStr, ServerPort, strchr(ServerPort, '/') - ServerPort);
		else
			strncpy_s(tmpStr, ServerPort, 63);
		out.dwServerPort = atol(tmpStr);
		clamp(out.dwServerPort, u32(START_PORT), u32(END_PORT));
		out.bPortWasSet = true; //this is not casual game
	}
}

bool BaseServer::CreateConnection(GameDescriptionData& game_descr, ServerConnectionOptions& opt)
{
	// set m_game_description
	CopyMemory(&m_game_description, &game_descr, sizeof(game_descr));

	return true;
}


BaseServer::EConnect BaseServer::Connect(LPCSTR options, GameDescriptionData & game_descr)
{
	connect_options = options;
	psNET_direct_connect = FALSE;

	if (strstr(options, "/single"))
		psNET_direct_connect = TRUE;


	// Parse options
	ServerConnectionOptions connectOpt;
	ParseConnectionOptions(options, connectOpt);

	if (!psNET_direct_connect)
	{
		bool success = CreateConnection(game_descr, connectOpt);
		if (!success)
		{
			return ErrNoError;
		}
		BannedList_Load();
		IpList_Load();
	}
#ifndef XR_MP_BUILD
	else
	{
		CopyMemory(&m_game_description, &game_descr, sizeof(game_descr));
	}
#endif

	return EConnect::ErrNoError;
}

void BaseServer::Disconnect()
{
	DestroyConnection();

	if (!psNET_direct_connect)
	{
		BannedList_Save();
		IpList_Unload();
	}
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region ban / unban

IBannedClient*	BaseServer::GetBannedClient(const ip_address& Address)
{
	for (u32 it = 0; it < BannedAddresses.size(); it++)
	{
		IBannedClient* pBClient = BannedAddresses[it];
		if (pBClient->HAddr == Address)
			return pBClient;
	}
	return NULL;
};

void BaseServer::BanClient(IClient* C, u32 BanTime)
{
	ip_address				ClAddress;
	GetClientAddress(C->ID, ClAddress);
	BanAddress(ClAddress, BanTime);
};

void BaseServer::BanAddress(const ip_address& Address, u32 BanTimeSec)
{
	if (GetBannedClient(Address))
	{
		Msg("Already banned\n");
		return;
	};

	IBannedClient* pNewClient = xr_new<IBannedClient>();
	pNewClient->HAddr = Address;
	time(&pNewClient->BanTime);
	pNewClient->BanTime += BanTimeSec;
	if (pNewClient)
	{
		BannedAddresses.push_back(pNewClient);
		BannedList_Save();
	}
};

void BaseServer::UnBanAddress(const ip_address& Address)
{
	if (!GetBannedClient(Address))
	{
		Msg("! Can't find address %s in ban list.", Address.to_string().c_str());
		return;
	};

	for (u32 it = 0; it < BannedAddresses.size(); it++)
	{
		IBannedClient* pBClient = BannedAddresses[it];
		if (pBClient->HAddr == Address)
		{
			xr_delete(BannedAddresses[it]);
			BannedAddresses.erase(BannedAddresses.begin() + it);
			Msg("Unbanning %s", Address.to_string().c_str());
			BannedList_Save();
			break;
		}
	};
}

void BaseServer::Print_Banned_Addreses()
{
	Msg("- ----banned ip list begin-------");
	for (u32 i = 0; i < BannedAddresses.size(); i++)
	{
		IBannedClient* pBClient = BannedAddresses[i];
		Msg("- %s to %s", pBClient->HAddr.to_string().c_str(), pBClient->BannedTimeTo().c_str());
	}
	Msg("- ----banned ip list end-------");
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region banned list

LPCSTR BaseServer::GetBannedListName()
{
	return "banned_list_ip.ltx";
}

void BaseServer::BannedList_Save()
{
	string_path					temp;
	FS.update_path(temp, "$app_data_root$", GetBannedListName());

	CInifile					ini(temp, FALSE, FALSE, TRUE);

	for (u32 it = 0; it < BannedAddresses.size(); it++)
	{
		IBannedClient* cl = BannedAddresses[it];
		cl->Save(ini);
	};
}

void BaseServer::BannedList_Load()
{
	string_path					temp;
	FS.update_path(temp, "$app_data_root$", GetBannedListName());

	CInifile					ini(temp);

	auto it = ini.sections().begin();
	auto it_e = ini.sections().end();

	for (; it != it_e; ++it)
	{
		const shared_str& sect_name = (it)->first;
		IBannedClient* Cl = xr_new<IBannedClient>();
		Cl->Load(ini, sect_name);
		BannedAddresses.push_back(Cl);
	}
}


bool banned_client_comparer(IBannedClient* C1, IBannedClient* C2)
{
	return C1->BanTime > C2->BanTime;
}

void BaseServer::UpdateBannedList()
{
	if (!BannedAddresses.size())		return;
	std::sort(BannedAddresses.begin(), BannedAddresses.end(), banned_client_comparer);
	time_t						T;
	time(&T);

	IBannedClient* Cl = BannedAddresses.back();
	if (Cl->BanTime < T)
	{
		ip_address				Address = Cl->HAddr;
		UnBanAddress(Address);
	}
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region ip filter

void BaseServer::IpList_Load()
{
	Msg("* Initializing IP filter.");
	m_ip_filter.load();
}
void BaseServer::IpList_Unload()
{
	Msg("* Deinitializing IP filter.");
	m_ip_filter.unload();
}

bool BaseServer::IsPlayerIPDenied(u32 ip_address)
{
	return !m_ip_filter.is_ip_present(ip_address);
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region statistic

void	BaseServer::ClearStatistic()
{
	stats.clear();
	struct StatsClearFunctor
	{
		static void Clear(IClient* client)
		{
			client->stats.Clear();
		}
	};
	net_players.ForEachClientDo(StatsClearFunctor::Clear);
};

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region disconnect

bool BaseServer::DisconnectAddress(const ip_address & Address, LPCSTR reason)
{
	u32 players_count = net_players.ClientsCount();
	buffer_vector<IClient*>	PlayersToDisconnect(
		_alloca(players_count * sizeof(IClient*)),
		players_count
	);
	struct ToDisconnectFillerFunctor
	{
		BaseServer*				m_owner;
		buffer_vector<IClient*>*	dest;
		ip_address const*			address_to_disconnect;
		ToDisconnectFillerFunctor(BaseServer* owner, buffer_vector<IClient*>* dest_disconnect, ip_address const* address) :
			m_owner(owner), dest(dest_disconnect), address_to_disconnect(address)
		{}
		void operator()(IClient* client)
		{
			ip_address			tmp_address;
			m_owner->GetClientAddress(client->ID, tmp_address);
			if (*address_to_disconnect == tmp_address)
			{
				dest->push_back(client);
			};
		}
	};
	ToDisconnectFillerFunctor tmp_functor(this, &PlayersToDisconnect, &Address);
	net_players.ForEachClientDo(tmp_functor);

	buffer_vector<IClient*>::iterator it = PlayersToDisconnect.begin();
	buffer_vector<IClient*>::iterator it_e = PlayersToDisconnect.end();

	for (; it != it_e; ++it)
	{
		DisconnectClient(*it, reason);
	}
	return true;
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region send

void BaseServer::Flush_Clients_Buffers()
{
#if NET_LOG_PACKETS
	Msg("#flush server send-buf");
#endif

	struct LocalSenderFunctor
	{
		static void FlushBuffer(IClient* client)
		{
			client->MultipacketSender::FlushSendBuffer(0);
		}
	};

	net_players.ForEachClientDo(
		LocalSenderFunctor::FlushBuffer
	);
}

void BaseServer::SendTo_Buf(ClientID id, void* data, u32 size, u32 dwFlags, u32 dwTimeout)
{
	IClient* tmp_client = net_players.GetFoundClient(
		ClientIdSearchPredicate(id));
	VERIFY(tmp_client);
	tmp_client->MultipacketSender::SendPacket(data, size, dwFlags, dwTimeout);
}


void BaseServer::SendTo_LL(ClientID ID, void* data, u32 size, u32 dwFlags, u32 dwTimeout)
{
	/*if (psNET_Flags.test(NETFLAG_LOG_SV_PACKETS))
	{
		if (!pSvNetLog) pSvNetLog = xr_new<INetLog>("logs\\net_sv_log.log", TimeGlobal(device_timer));
		if (pSvNetLog) pSvNetLog->LogData(TimeGlobal(device_timer), data, size);
	}*/

	/*
#ifdef _DEBUG
	u32 time_global = TimeGlobal(device_timer);
	if (time_global - stats.dwSendTime >= 999)
	{
		stats.dwBytesPerSec = (stats.dwBytesPerSec * 9 + stats.dwBytesSended) / 10;
		stats.dwBytesSended = 0;
		stats.dwSendTime = time_global;
	};
	if (ID.value())
		stats.dwBytesSended += size;
#endif
*/

	_SendTo_LL(ID, data, size, dwFlags, dwTimeout);
}

void BaseServer::SendTo(ClientID ID, NET_Packet& P, u32 dwFlags, u32 dwTimeout)
{
	SendTo_LL(ID, P.B.data, P.B.count, dwFlags, dwTimeout);
}

void BaseServer::SendBroadcast_LL(ClientID exclude, void* data, u32 size, u32 dwFlags)
{
	struct ClientExcluderPredicate
	{
		ClientID id_to_exclude;
		ClientExcluderPredicate(ClientID exclude) :
			id_to_exclude(exclude)
		{}
		bool operator()(IClient* client)
		{
			if (client->ID == id_to_exclude)
				return false;
			if (!client->flags.bConnected)
				return false;
			return true;
		}
	};
	struct ClientSenderFunctor
	{
		BaseServer*	m_owner;
		void*			m_data;
		u32				m_size;
		u32				m_dwFlags;
		ClientSenderFunctor(BaseServer* owner, void* data, u32 size, u32 dwFlags) :
			m_owner(owner), m_data(data), m_size(size), m_dwFlags(dwFlags)
		{}
		void operator()(IClient* client)
		{
			m_owner->SendTo_LL(client->ID, m_data, m_size, m_dwFlags);
		}
	};
	ClientSenderFunctor temp_functor(this, data, size, dwFlags);
	net_players.ForFoundClientsDo(ClientExcluderPredicate(exclude), temp_functor);
}

void BaseServer::SendBroadcast(ClientID exclude, NET_Packet& P, u32 dwFlags)
{
	// Perform broadcasting
	SendBroadcast_LL(exclude, P.B.data, P.B.count, dwFlags);
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region receive

void BaseServer::_Recieve(const void* data, u32 data_size, u32 param)
{
	if (data_size >= NET_PacketSizeLimit) {
		Msg("! too large packet size[%d] received, DoS attack?", data_size);
		return;
	}

	csMessagesQueue.Enter();

	m_messagesQueue.emplace_back(data, data_size, param);

	csMessagesQueue.Leave();

	/*if (psNET_Flags.test(NETFLAG_LOG_SV_PACKETS))
	{
		if (!pSvNetLog)
			pSvNetLog = xr_new<INetLog>("logs\\net_sv_log.log", TimeGlobal(device_timer));

		if (pSvNetLog)
			pSvNetLog->LogPacket(TimeGlobal(device_timer), &packet, TRUE);
	}*/
}

void BaseServer::ProcessMessagesQueue()
{
	// Pavel: Calls from xrServer (from game thread)
	csMessagesQueue.Enter();

	if (!m_messagesQueue.empty())
	{
		for (auto& msg : m_messagesQueue)
		{
			csMessage.Enter();
			u32	result = OnMessage(msg.P, msg.Id);
			csMessage.Leave();

			if (result)
				SendBroadcast(msg.Id, msg.P, result);
		}
		m_messagesQueue.clear();
	}

	csMessagesQueue.Leave();
}

#pragma endregion

// -----------------------------------------------------------------------------

#pragma region bandwidth

bool BaseServer::HasBandwidth(IClient * C)
{
	u32	dwTime = TimeGlobal(device_timer);
	u32	dwInterval = 0;

	if (psNET_direct_connect)
	{
		UpdateClientStatistic(C);
		C->dwTime_LastUpdate = dwTime;
		dwInterval = 1000;
		return true;
	}

	if (psNET_ServerUpdate != 0) dwInterval = 1000 / psNET_ServerUpdate;
	if (psNET_Flags.test(NETFLAG_MINIMIZEUPDATES))	dwInterval = 1000;	// approx 2 times per second

	if (psNET_ServerUpdate != 0 && (dwTime - C->dwTime_LastUpdate) > dwInterval)
	{
		//check queue for "empty" state
		DWORD dwPending;
		if (!GetClientPendingMessagesCount(C->ID.value(), dwPending))
		{
			return false;
		}

		if (dwPending > u32(psNET_ServerPending))
		{
			C->stats.dwTimesBlocked++;
			return false;
		};

		UpdateClientStatistic(C);
		// ok
		C->dwTime_LastUpdate = dwTime;
		return true;
	}

	return false;
}

#pragma endregion

// -----------------------------------------------------------------------------

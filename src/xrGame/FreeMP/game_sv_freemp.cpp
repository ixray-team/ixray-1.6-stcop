#include "stdafx.h"
#include "game_sv_freemp.h"
#include "Level.h"

#include "alife_simulator.h"
#include "alife_object_registry.h"
#include "alife_graph_registry.h"
#include "alife_time_manager.h"

#include "restriction_space.h"
#include "../xrServerEntities/clsid_game.h"

game_sv_freemp::game_sv_freemp()
	:pure_relcase(&game_sv_freemp::net_Relcase)
{
	m_type = eGameIDFreeMP;

}

game_sv_freemp::~game_sv_freemp()
{
}


void game_sv_freemp::SpawnItemToActor(u16 actorId, LPCSTR name)
{
	if (!name) return;

	CSE_Abstract* E = spawn_begin(name);
	E->ID_Parent = actorId;
	E->s_flags.assign(M_SPAWN_OBJECT_LOCAL);	// flags

	CSE_ALifeItemWeapon* pWeapon = smart_cast<CSE_ALifeItemWeapon*>(E);
	if (pWeapon)
	{
		u16 ammo_magsize = pWeapon->get_ammo_magsize();
		pWeapon->a_elapsed = ammo_magsize;
	}

	CSE_ALifeItemPDA* pPda = smart_cast<CSE_ALifeItemPDA*>(E);
	if (pPda)
	{
		pPda->m_original_owner = actorId;
	}

	spawn_end(E, m_server->GetServerClient()->ID);
}

void game_sv_freemp::AddMoneyToPlayer(game_PlayerState* ps, s32 amount)
{
	if (!ps) return;

	Msg("- Add money to player: [%u]%s, %d amount", ps->GameID, ps->getName(), amount);

	s64 total_money = ps->money_for_round;
	total_money += amount;

	if (total_money < 0)
		total_money = 0;

	if (total_money > std::numeric_limits<s32>().max())
	{
		Msg("! The limit of the maximum amount of money has been exceeded.");
		total_money = std::numeric_limits<s32>().max() - 1;
	}

	ps->money_for_round = s32(total_money);
	signal_Syncronize();
}



void game_sv_freemp::OnTransferMoney(NET_Packet& P, ClientID const& clientID)
{
	ClientID to;
	s32 money;

	P.r_clientID(to);
	P.r_s32(money);

	Msg("* Try to transfer money from %u to %u. Amount: %d", clientID.value(), to.value(), money);

	game_PlayerState* ps_from = get_id(clientID);
	if (!ps_from)
	{
		Msg("! Can't find player state with id=%u", clientID.value());
		return;
	}

	game_PlayerState* ps_to = get_id(to);
	if (!ps_to)
	{
		Msg("! Can't find player state with id=%u", to.value());
		return;
	}

	if (money <= 0 || ps_from->money_for_round < money) return;

	AddMoneyToPlayer(ps_from, -money);
	AddMoneyToPlayer(ps_to, money);
}

void game_sv_freemp::on_death(CSE_Abstract* e_dest, CSE_Abstract* e_src)
{
	inherited::on_death(e_dest, e_src);

	if (!ai().get_alife())
		return;

	alife().on_death(e_dest, e_src);
}


void game_sv_freemp::Create(shared_str& options)
{
	inherited::Create(options);
	R_ASSERT2(rpoints[0].size(), "rpoints for players not found");

	switch_Phase(GAME_PHASE_PENDING);

	::Random.seed(GetTickCount());
	m_CorpseList.clear();
}

// player connect #1
void game_sv_freemp::OnPlayerConnect(ClientID id_who)
{
	inherited::OnPlayerConnect(id_who);

	xrClientData* xrCData = m_server->ID_to_client(id_who);
	game_PlayerState* ps_who = get_id(id_who);

	if (!xrCData->flags.bReconnect)
	{
		ps_who->clear();
		ps_who->team = 0;
		ps_who->skin = -1;
	};
	ps_who->setFlag(GAME_PLAYER_FLAG_SPECTATOR);

	ps_who->resetFlag(GAME_PLAYER_FLAG_SKIP);

	if (g_dedicated_server && (xrCData == m_server->GetServerClient()))
	{
		ps_who->setFlag(GAME_PLAYER_FLAG_SKIP);
		return;
	}
}

// player connect #2
void game_sv_freemp::OnPlayerConnectFinished(ClientID id_who)
{
	xrClientData* xrCData = m_server->ID_to_client(id_who);
	SpawnPlayer(id_who, "spectator");

	if (xrCData)
	{
		R_ASSERT2(xrCData->ps, "Player state not created yet");
		NET_Packet					P;
		GenerateGameMessage(P);
		P.w_u32(GAME_EVENT_PLAYER_CONNECTED);
		P.w_clientID(id_who);
		xrCData->ps->team = 0;
		xrCData->ps->setFlag(GAME_PLAYER_FLAG_SPECTATOR);
		xrCData->ps->setFlag(GAME_PLAYER_FLAG_READY);
		xrCData->ps->net_Export(P, TRUE);
		u_EventSend(P);
		xrCData->net_Ready = TRUE;
	};
}

void game_sv_freemp::SetSkin(CSE_Abstract* E, u16 Team, u16 ID)
{
	if (!E) return;
	//-------------------------------------------
	CSE_Visual* pV = smart_cast<CSE_Visual*>(E);
	if (!pV) return;
	//-------------------------------------------
	string256 SkinName;
	xr_strcpy(SkinName, pSettings->r_string("mp_skins_path", "skin_path"));
	//загружены ли скины для этой комманды
//	if (SkinID != -1) ID = u16(SkinID);

	if (!TeamList.empty() &&
		TeamList.size() > Team &&
		!TeamList[Team].aSkins.empty())
	{
		//загружено ли достаточно скинов для этой комманды
		if (TeamList[Team].aSkins.size() > ID)
		{
			xr_strcat(SkinName, TeamList[Team].aSkins[ID].c_str());
		}
		else
			xr_strcat(SkinName, TeamList[Team].aSkins[0].c_str());
	}
	else
	{
		//скины для такой комманды не загружены
		switch (Team)
		{
		case 0:
			xr_strcat(SkinName, "stalker_hood_multiplayer");
			break;
		case 1:
			xr_strcat(SkinName, "soldat_beret");
			break;
		case 2:
			xr_strcat(SkinName, "stalker_black_mask");
			break;
		default:
			R_ASSERT2(0, "Unknown Team");
			break;
		};
	};

	xr_strcat(SkinName, ".ogf");
	Msg("* Skin - %s", SkinName);
	int len = xr_strlen(SkinName);
	R_ASSERT2(len < 64, "Skin Name is too LONG!!!");
	pV->set_visual(SkinName);
	//-------------------------------------------
};

void game_sv_freemp::OnPlayerReady(ClientID id_who)
{
	switch (Phase())
	{
	case GAME_PHASE_INPROGRESS:
	{
		xrClientData* xrCData = (xrClientData*)m_server->ID_to_client(id_who);
		game_PlayerState* ps = get_id(id_who);
		if (ps->IsSkip())					break;

		if (!(ps->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD)))	break;

		xrClientData* xrSCData = (xrClientData*)m_server->GetServerClient();

		CSE_Abstract* pOwner = xrCData->owner;

		RespawnPlayer(id_who, false);
		pOwner = xrCData->owner;

	} break;

	default:
		break;
	};
}

void game_sv_freemp::RespawnPlayer(ClientID id_who, bool NoSpectator)
{
	inherited::RespawnPlayer(id_who, NoSpectator);

	xrClientData* xrCData = (xrClientData*)m_server->ID_to_client(id_who);
	if (!xrCData) return;

	game_PlayerState* ps = xrCData->ps;
	if (!ps) return;

	CSE_ALifeCreatureActor* pA = smart_cast<CSE_ALifeCreatureActor*>(xrCData->owner);
	if (!pA) return;

	SpawnWeapon4Actor(pA->ID, "mp_players_rukzak", 0, ps->pItemList);
}

void game_sv_freemp::OnDetach(u16 eid_who, u16 eid_what)
{
	CSE_ActorMP* e_who = smart_cast<CSE_ActorMP*>(m_server->ID_to_entity(eid_who));
	if (!e_who)
		return;

	CSE_Abstract* e_entity = m_server->ID_to_entity(eid_what);
	if (!e_entity)
		return;

	// drop players bag
	if (e_entity->m_tClassID == CLSID_OBJECT_PLAYERS_BAG)
	{
		OnDetachPlayersBag(e_who, e_entity);
	}
}

// player disconnect
void game_sv_freemp::OnPlayerDisconnect(ClientID id_who, LPSTR Name, u16 GameID)
{
	inherited::OnPlayerDisconnect(id_who, Name, GameID);
}

void game_sv_freemp::OnPlayerKillPlayer(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA)
{
	if (ps_killed)
	{
		ps_killed->setFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD);
		ps_killed->DeathTime = Device.dwTimeGlobal;
	}
	signal_Syncronize();
}

void game_sv_freemp::OnEvent(NET_Packet& P, u16 type, u32 time, ClientID sender)
{
	switch (type)
	{
	case GAME_EVENT_PLAYER_KILL: // (g_kill)
	{
		u16 ID = P.r_u16();
		xrClientData* l_pC = (xrClientData*)get_client(ID);
		if (!l_pC) break;
		KillPlayer(l_pC->ID, l_pC->ps->GameID);
	}
	break;
	case GAME_EVENT_TRANSFER_MONEY:
	{
		OnTransferMoney(P, sender);
	}
	break;
	default:
		inherited::OnEvent(P, type, time, sender);
	};
}

void game_sv_freemp::Update()
{
	if (Phase() != GAME_PHASE_INPROGRESS)
	{
		OnRoundStart();
	}
}
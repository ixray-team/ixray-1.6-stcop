#include "stdafx.h"
#include "game_sv_freemp.h"
#include "ai_space.h"
#include "../../../xrScripts/script_engine.h"
#include "inventory_item.h"
#include "Level.h"
#include "gameobject.h"

#include "ai_space.h"
#include "alife_simulator.h"
#include "inventory_upgrade_manager.h"
#include "inventory_upgrade.h"

void game_sv_freemp::OnPlayerRepairItem(NET_Packet& P, ClientID const& clientID)
{
	game_PlayerState* ps = get_id(clientID);
	if (!ps) return;

	u16 itemId = P.r_u16();
	s32 cost = P.r_s32();

	PIItem item = smart_cast<CInventoryItem*>(Level().Objects.net_Find(itemId));
	if (!item) return;

	if (ps->money_for_round < cost) return;

	AddMoneyToPlayer(ps, -cost);

	NET_Packet NP;
	CGameObject::u_EventGen(NP, GE_REPAIR_ITEM, itemId);
	CGameObject::u_EventSend(NP);

	GenerateGameMessage(NP);
	NP.w_u32(GAME_EVENT_MP_REPAIR_SUCCESS);
	NP.w_u16(itemId);
	m_server->SendTo(clientID, NP);
}

void game_sv_freemp::OnPlayerInstallUpgrade(NET_Packet& P, ClientID const& clientID)
{
	game_PlayerState* ps = get_id(clientID);
	if (!ps) return;

	shared_str upgrade_id;

	u16 itemId = P.r_u16();
	P.r_stringZ(upgrade_id);

	PIItem item = smart_cast<CInventoryItem*>(Level().Objects.net_Find(itemId));
	if (!item) return;


	luabind::functor<s32> funct;
	R_ASSERT2(
		ai().script_engine().functor("inventory_upgrades.how_much_upgrade", funct),
		make_string<const char*>("Failed to get functor <inventory_upgrades.how_much_upgrade>, upgrade id = %s", pSettings->r_string(upgrade_id, "section"))
	);

	s32 cost = funct(pSettings->r_string(upgrade_id, "section"));

	if (ps->money_for_round < cost) return;

	AddMoneyToPlayer(ps, -cost);

	auto& upgrade_manager = ai().alife().inventory_upgrade_manager();

	if (!upgrade_manager.can_upgrade_install(*item, upgrade_id, false))
	{
		Msg("! Warning: cannot install upgrade \"%s\" to \"%s\" Player \"%s\".", upgrade_id.c_str(), item->m_name.c_str(), ps->getName());
		return;
	}

	if (upgrade_manager.upgrade_install_mp(*item, upgrade_id, false))
	{
		NET_Packet NP;
		GenerateGameMessage(NP);
		NP.w_u32(GAME_EVENT_MP_INSTALL_UPGRADE_SUCCESS);
		NP.w_u16(itemId);
		m_server->SendTo(clientID, NP);
	}
}

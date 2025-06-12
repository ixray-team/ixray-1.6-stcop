#include "stdafx.h"
#include "game_sv_freemp.h"
#include "Level.h"
#include "trade.h"
#include "actor_mp_client.h"
// Pavel: Отключено, так как нет нормальной синхронизации.
// #define ENABLE_DETAILED_TRADING_CHECK 1
void game_sv_freemp::OnPlayerTrade(NET_Packet& P, ClientID const& clientID)
{
	game_PlayerState* ps = get_id(clientID);
	if (!ps) return;
	bool isSelling = P.r_u8();
	u16 traderID = P.r_u16();
	u16 playerID = P.r_u16();
	s32 totalPrice = P.r_s32();
	auto pActor = smart_cast<CActor*>(Level().Objects.net_Find(playerID));
	if (!pActor)
		return;
	auto pTrader = smart_cast<CInventoryOwner*>(Level().Objects.net_Find(traderID));
	if (!pTrader)
		return;
	CTrade* pTrade = pTrader->GetTrade();
	if (!pTrade)
		return;
	if (isSelling)
	{
		u32	itemsCount = P.r_u32();
		xr_vector<CInventoryItem*> items;
		items.reserve(itemsCount);
		s32 svTotalPrice = 0;
		float itemCondition;
		u16 itemId;
#ifdef ENABLE_DETAILED_TRADING_CHECK
		pTrade->StartTradeEx(pActor);
		for (u32 i = 0; i != itemsCount; ++i)
		{
			P.r_u16(itemId);
			P.r_float(itemCondition);
			auto pItem = smart_cast<CInventoryItem*>(Level().Objects.net_Find(itemId));
			R_ASSERT(pItem);
			items.push_back(pItem);
			pItem->SetCondition(itemCondition);
			svTotalPrice += pTrade->GetItemPrice(pItem, true);
		}
		pTrade->StopTrade();
		if (totalPrice != svTotalPrice)
		{
			Msg(
				"! Cheater! Sell Items. Player name: %s; money: %d; total price: %d; sv total price: %d",
				ps->getName(),
				ps->money_for_round,
				totalPrice, svTotalPrice
			);
			// For debug. Remove in future
			for (const auto pItem : items)
			{
				Msg("Item to sell: %s", pItem->m_section_id.c_str());
			}
			return;
		}
#else
		for (u32 i = 0; i != itemsCount; ++i)
		{
			P.r_u16(itemId);
			P.r_float(itemCondition);
			auto pItem = smart_cast<CInventoryItem*>(Level().Objects.net_Find(itemId));
			if (pItem)
			{
				items.push_back(pItem);
			}
			else
			{
				Msg("! Warning: item with id=%u not found, skip for trade", itemId);
			}
		}
#endif // ENABLE_DETAILED_TRADING_CHECK
		if (pTrader->get_money() < totalPrice)
		{
			Msg("NPC %s does not have enough money", pTrader->Name());
			return;
		}
		if (!pTrader->InfinitiveMoney())
			pTrader->set_money(pTrader->get_money() - totalPrice, true);
		AddMoneyToPlayer(ps, totalPrice);
		signal_Syncronize();
		for (const auto pItem : items)
		{
			NET_Packet P;
			P.w_begin(M_EVENT);
			P.w_u32(Device.dwTimeGlobal - 2 * NET_Latency);
			P.w_u16(GE_DESTROY);
			P.w_u16(pItem->object_id());
			Level().Send(P, net_flags(TRUE, TRUE));
		}
	}
	else
	{
		if (ps->money_for_round < totalPrice)
		{
			Msg("! Cheater! Buy Items. Player name:  %s; money: %d total price: %d",
				ps->getName(),
				ps->money_for_round,
				totalPrice
			);
			return;
		}
		xr_map<CInventoryItem*, u16> sellMap;
		s32 svTotalPrice = 0;
		u32	itemsMapSize;
		u16 itemId, itemCount;
		P.r_u32(itemsMapSize);
#ifdef ENABLE_DETAILED_TRADING_CHECK
		pTrade->StartTradeEx(pActor);
		for (u32 i = 0; i != itemsMapSize; ++i)
		{
			P.r_u16(itemId);
			P.r_u16(itemCount);
			auto pItem = smart_cast<CInventoryItem*>(Level().Objects.net_Find(itemId));
			R_ASSERT(pItem);
			sellMap[pItem] = itemCount;
			svTotalPrice += (pTrade->GetItemPrice(pItem, false) * itemCount);
		}
		pTrade->StopTrade();
		if (totalPrice != svTotalPrice)
		{
			Msg(
				"! Cheater! Buy Items. Player name: %s; money: %d; total price: %d; sv total price: %d",
				ps->getName(),
				ps->money_for_round,
				totalPrice, svTotalPrice
			);
			return;
		}
#else
		for (u32 i = 0; i != itemsMapSize; ++i)
		{
			P.r_u16(itemId);
			P.r_u16(itemCount);
			auto pItem = smart_cast<CInventoryItem*>(Level().Objects.net_Find(itemId));
			if (pItem)
			{
				sellMap[pItem] = itemCount;
			}
			else
			{
				Msg("! Warning: item with id=%u not found, skip for trade", itemId);
			}
		}
#endif // ENABLE_DETAILED_TRADING_CHECK
		if (!pTrader->InfinitiveMoney())
			pTrader->set_money(pTrader->get_money() + totalPrice, true);
		AddMoneyToPlayer(ps, -totalPrice);
		signal_Syncronize();
		auto it = sellMap.cbegin(), it_e = sellMap.cend();
		for (; it != it_e; it++)
		{
			LPCSTR itemName = it->first->m_section_id.c_str();
			u16 itemsCount = it->second;
			for (u16 i = 0; i < itemsCount; ++i)
			{
				CSE_Abstract* E = spawn_begin(itemName);
				E->ID_Parent = playerID;
				E->s_flags.assign(M_SPAWN_OBJECT_LOCAL);
				spawn_end(E, m_server->GetServerClient()->ID);
			}
		}
	}
}
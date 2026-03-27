//#include "StdAfx.h"
#include "StdAfx.h"
#include "pch_script.h"
#include "UIActorMenu.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UICharacterInfo.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "UICellItem.h"
#include "UIInventoryUtilities.h"
#include "UICellItemFactory.h"
#include "../../xrEngine/xr_input.h"
#include "../InventoryOwner.h"
#include "../Inventory.h"
#include "../trade.h"
#include "../Entity.h"
#include "../Actor.h"
#include "../Weapon.h"
#include "../trade_parameters.h"
#include "../inventory_item_object.h"
#include "../MPPlayersBag.h"
#include "../../xrEngine/string_table.h"
#include "../ai/monsters/basemonster/base_monster.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"
#include "UIGameSP.h"
#include "UITalkWnd.h"
#include "Car.h"

bool is_item_in_list(CUIDragDropListEx* pList, PIItem item);
// -------------------------------------------------

void CUIActorMenu::InitTradeMode()
{
	m_pInventoryBagList->Show		(false);
	m_PartnerCharacterInfo->Show	(true);
	m_PartnerMoney->Show			(true);
	if (m_pQuickSlot)
		m_pQuickSlot->Show				(true);

	m_pTradeActorBagList->Show		(true);
	m_pTradeActorList->Show			(true);
	m_pTradePartnerBagList->Show	(true);
	m_pTradePartnerList->Show		(true);

	m_RightDelimiter->Show			(true);
	m_LeftDelimiter->Show			(true);
	m_LeftBackground->Show			(true);

	m_PartnerBottomInfo->Show		(true);
	m_PartnerWeight->Show			(true);
    if (m_trade_button)
	{
        m_trade_button->Show(true);
		m_trade_button->Enable(false);  // Disable until items added
	}
    if (m_trade_buy_button)
	{
        m_trade_buy_button->Show(true);
		m_trade_buy_button->Enable(false);  // Disable until items added
	}
    if (m_trade_sell_button)
	{
        m_trade_sell_button->Show(true);
		m_trade_sell_button->Enable(false);  // Disable until items added
	}

	VERIFY							( m_pPartnerInvOwner );
	m_pPartnerInvOwner->StartTrading();

	InitInventoryContents			( m_pTradeActorBagList );
	InitPartnerInventoryContents	();

	m_actor_trade					= m_pActorInvOwner->GetTrade();
	m_partner_trade					= m_pPartnerInvOwner->GetTrade();
	VERIFY							( m_actor_trade );
	VERIFY							( m_partner_trade );
	m_actor_trade->StartTradeEx		( m_pPartnerInvOwner );
	m_partner_trade->StartTradeEx	( m_pActorInvOwner );

	UpdatePrices();

	SetAreaSelectionTo(m_pTradeActorBagList);
}

void CUIActorMenu::UpdateTradeActorBagList()
{
	if (!m_pTradeActorBagList || !m_pActorInvOwner)
	{
		return;
	}

	m_pTradeActorBagList->ClearAll(true);

	TIItemContainer items_list = m_pActorInvOwner->inventory().m_ruck;
	std::sort(items_list.begin(), items_list.end(), InventoryUtilities::GreaterRoomInRuck);
	if (m_pInventorySorter)
	{
		m_pInventorySorter->SortItems(items_list, GetPlayerSortCategory());
	}

	for (PIItem item : items_list)
	{
		CMPPlayersBag* bag = smart_cast<CMPPlayersBag*>(&item->object());
		if (bag || is_item_in_list(m_pTradeActorList, item))
		{
			continue;
		}

		CUICellItem* itm = create_cell_item(item);
		m_pTradeActorBagList->SetItem(itm);
		ColorizeItem(itm, !CanMoveToPartner(item));
	}
}

void CUIActorMenu::UpdateTradePartnerBagList()
{
	if (!m_pTradePartnerBagList || !m_pPartnerInvOwner)
	{
		return;
	}

	InitPartnerInventoryContents();
	UpdatePrices();
}

void CUIActorMenu::DeInitTradeMode()
{
	if ( m_actor_trade )
	{
		m_actor_trade->StopTrade();
	}
	if ( m_partner_trade )
	{
		m_partner_trade->StopTrade();
	}
	if ( m_pPartnerInvOwner )
	{
		m_pPartnerInvOwner->StopTrading();
	}

	m_pInventoryBagList->Show		(true);
	m_PartnerCharacterInfo->Show	(false);
	m_PartnerMoney->Show			(false);

	m_pTradeActorBagList->Show		(false);
	m_pTradeActorList->Show			(false);
	m_pTradePartnerBagList->Show	(false);
	m_pTradePartnerList->Show		(false);
	
	m_RightDelimiter->Show			(false);
	m_LeftDelimiter->Show			(false);
	m_LeftBackground->Show			(false);

	m_PartnerBottomInfo->Show		(false);
	m_PartnerWeight->Show			(false);
	if (m_trade_button)
		m_trade_button->Show(false);
	if (m_trade_buy_button)
		m_trade_buy_button->Show(false);
	if (m_trade_sell_button)
		m_trade_sell_button->Show(false);

	if (!CurrentGameUI())
		return;

	if (CurrentGameUI()->TalkMenu && CurrentGameUI()->TalkMenu->IsShown())
	{
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();
	}
}

void CUIActorMenu::UpdateActor()
{
	if ( IsGameTypeSingle() )
	{
		string64 buf;
		xr_sprintf( buf, "%d RU", m_pActorInvOwner->get_money() );
		m_ActorMoney->SetText( buf );
	}
	else
	{
		UpdateActorMoneyMP();
	}
	
	if (CActor* actor = m_pActorInvOwner->cast_actor())
	{
		if (CWeapon* wp = actor->inventory().ActiveItem() ? actor->inventory().ActiveItem()->cast_weapon() : nullptr)
		{
			wp->ForceUpdateAmmo();
		}
	}

	InventoryUtilities::UpdateWeightStr( *m_ActorWeight, *m_ActorWeightMax, m_pActorInvOwner );
	
	m_ActorWeight->AdjustWidthToText();
	m_ActorWeightMax->AdjustWidthToText();
	m_ActorBottomInfo->AdjustWidthToText();

	Fvector2 pos = m_ActorWeight->GetWndPos();
	pos.x = m_ActorWeightMax->GetWndPos().x - m_ActorWeight->GetWndSize().x - 5.0f;
	m_ActorWeight->SetWndPos( pos );
	pos.x = pos.x - m_ActorBottomInfo->GetWndSize().x - 5.0f;
	m_ActorBottomInfo->SetWndPos( pos );
}

void CUIActorMenu::UpdatePartnerBag()
{
	string64 buf;

	CBaseMonster* monster = m_pPartnerInvOwner != nullptr ? m_pPartnerInvOwner->cast_base_monster() : nullptr;
	CCar* pCar = m_pPartnerInvOwner != nullptr ? m_pPartnerInvOwner->cast_car() : nullptr;

	if (pCar != nullptr || monster != nullptr || m_pPartnerInvOwner->use_simplified_visual())
	{
		m_PartnerWeight->SetText( "" );
	}
	else if ( m_pPartnerInvOwner->InfinitiveMoney() ) 
	{
		m_PartnerMoney->SetText( "∞ RU" );
	}
	else
	{
		xr_sprintf( buf, "%d RU", m_pPartnerInvOwner->get_money() );
		m_PartnerMoney->SetText( buf );
	}	

	LPCSTR kg_str = g_pStringTable->translate( "st_kg" ).c_str();
	float total	= CalcItemsWeight( m_pTradePartnerBagList );
	xr_sprintf( buf, "%.1f %s", total, kg_str );
	m_PartnerWeight->SetText( buf );
	m_PartnerWeight->AdjustWidthToText();

	Fvector2 pos = m_PartnerWeight->GetWndPos();
	pos.x = m_PartnerWeight_end_x - m_PartnerWeight->GetWndSize().x - 5.0f;
	m_PartnerWeight->SetWndPos( pos );
	pos.x = pos.x - m_PartnerBottomInfo->GetWndSize().x - 5.0f;
	m_PartnerBottomInfo->SetWndPos( pos );
}

void CUIActorMenu::UpdatePrices()
{
	LPCSTR kg_str = g_pStringTable->translate( "st_kg" ).c_str();

	UpdateActor();
	UpdatePartnerBag();
	u32 actor_price   = CalcItemsPrice( m_pTradeActorList,   m_partner_trade, true  );
	u32 partner_price = CalcItemsPrice( m_pTradePartnerList, m_partner_trade, false );

	string64 buf;
	xr_sprintf( buf, "%d RU", actor_price );		m_ActorTradePrice->SetText( buf );	m_ActorTradePrice->AdjustWidthToText();
	xr_sprintf( buf, "%d RU", partner_price );	m_PartnerTradePrice->SetText( buf );	m_PartnerTradePrice->AdjustWidthToText();

	float actor_weight   = CalcItemsWeight( m_pTradeActorList );
	float partner_weight = CalcItemsWeight( m_pTradePartnerList );

	xr_sprintf( buf, "(%.1f %s)", actor_weight, kg_str );	m_ActorTradeWeightMax->SetText( buf );
	xr_sprintf( buf, "(%.1f %s)", partner_weight, kg_str );	m_PartnerTradeWeightMax->SetText( buf );

	Fvector2 pos = m_ActorTradePrice->GetWndPos();
	pos.x = m_ActorTradeWeightMax->GetWndPos().x - m_ActorTradePrice->GetWndSize().x - 5.0f;
	m_ActorTradePrice->SetWndPos( pos );
//	pos.x = pos.x - m_ActorTradeCaption->GetWndSize().x - 5.0f;
//	m_ActorTradeCaption->SetWndPos( pos );

	pos = m_PartnerTradePrice->GetWndPos();
	pos.x = m_PartnerTradeWeightMax->GetWndPos().x - m_PartnerTradePrice->GetWndSize().x - 5.0f;
	m_PartnerTradePrice->SetWndPos( pos );
//	pos.x = pos.x - m_PartnerTradeCaption->GetWndSize().x - 5.0f;
//	m_PartnerTradeCaption->SetWndPos( pos );

// Update trade buttons state based on items in trade lists
	bool has_actor_items = m_pTradeActorList->ItemsCount() > 0;
	bool has_partner_items = m_pTradePartnerList->ItemsCount() > 0;

	if (m_trade_button)
		m_trade_button->Enable(has_actor_items || has_partner_items);

	if (m_trade_buy_button)
		m_trade_buy_button->Enable(has_partner_items);

	if (m_trade_sell_button)
		m_trade_sell_button->Enable(has_actor_items);	
}

void CUIActorMenu::OnBtnPerformTrade(CUIWindow* w, void* d)
{
	if (m_pTradeActorList->ItemsCount() == 0 && m_pTradePartnerList->ItemsCount() == 0)
	{
		return;
	}

	int actor_money = (int)m_pActorInvOwner->get_money();
	int partner_money = (int)m_pPartnerInvOwner->get_money();
	int actor_price = (int)CalcItemsPrice(m_pTradeActorList, m_partner_trade, true);
	int partner_price = (int)CalcItemsPrice(m_pTradePartnerList, m_partner_trade, false);

	int delta_price = actor_price - partner_price;
	actor_money += delta_price;
	partner_money -= delta_price;

	if ((actor_money >= 0) && (partner_money >= 0) && (actor_price >= 0 || partner_price > 0))
	{
		m_partner_trade->OnPerformTrade(partner_price, actor_price);

		TransferItems(m_pTradeActorList, m_pTradePartnerBagList, m_partner_trade, true);
		TransferItems(m_pTradePartnerList, m_pTradeActorBagList, m_partner_trade, false);

		if (pInput->GetControllerMode())
			SetCurrentItem(nullptr);
	}
	else
	{
		if (actor_money < 0)
		{
			CallMessageBoxOK("not_enough_money_actor");
		}
		else if (partner_money < 0)
		{
			CallMessageBoxOK("not_enough_money_partner");
		}
		else
		{
			CallMessageBoxOK("trade_dont_make");
		}
	}
	if (!pInput->GetControllerMode())
		SetCurrentItem(nullptr);

	UpdateItemsPlace();
}

void CUIActorMenu::OnBtnPerformTradeBuy(CUIWindow* w, void* d)
{
	if(m_pTradePartnerList->ItemsCount()==0) 
	{
		return;
	}

	int actor_money    = (int)m_pActorInvOwner->get_money();
	int partner_money  = (int)m_pPartnerInvOwner->get_money();
	int actor_price    = 0;//(int)CalcItemsPrice( m_pTradeActorList,   m_partner_trade, true  );
	int partner_price  = (int)CalcItemsPrice( m_pTradePartnerList, m_partner_trade, false );

	int delta_price    = actor_price - partner_price;
	actor_money        += delta_price;
	partner_money      -= delta_price;

	if ( ( actor_money >= 0 ) /*&& ( partner_money >= 0 )*/ && ( actor_price >= 0 || partner_price > 0 ) )
	{
		m_partner_trade->OnPerformTrade( partner_price, actor_price );

//		TransferItems( m_pTradeActorList,   m_pTradePartnerBagList, m_partner_trade, true );
		TransferItems( m_pTradePartnerList,	m_pTradeActorBagList,	m_partner_trade, false );

		// we clear current selection here since the item is transferred.
		// Only allow calling OnBtnPerformTradeBuy when current selection is inside the list of items to transfer
		// otherwise we can clear selection for another area 
		if (pInput->GetControllerMode())
			SetCurrentItem(nullptr);
	}
	else
	{
		if ( actor_money < 0 )
		{
			CallMessageBoxOK( "not_enough_money_actor" );
		}
		//else if ( partner_money < 0 )
		//{
		//	CallMessageBoxOK( "not_enough_money_partner" );
		//}
		else
		{
			CallMessageBoxOK( "trade_dont_make" );
		}
	}
	if (!pInput->GetControllerMode())
		SetCurrentItem				( nullptr );

	UpdateItemsPlace				();
}

void CUIActorMenu::OnBtnPerformTradeSell(CUIWindow* w, void* d)
{
	if (m_pTradeActorList->ItemsCount() == 0)
	{
		return;
	}

	int actor_money = (int)m_pActorInvOwner->get_money();
	int partner_money = (int)m_pPartnerInvOwner->get_money();
	int actor_price = (int)CalcItemsPrice(m_pTradeActorList, m_partner_trade, true);
	int partner_price = 0;
	bool partner_infinivite_money = m_pPartnerInvOwner->InfinitiveMoney();

	int delta_price = actor_price - partner_price;
	actor_money += delta_price;
	partner_money -= delta_price;

	if (actor_money >= 0 && (partner_infinivite_money || partner_money >= 0) && (actor_price >= 0 || partner_price > 0))
	{
		m_partner_trade->OnPerformTrade(partner_price, actor_price);

		TransferItems(m_pTradeActorList, m_pTradePartnerBagList, m_partner_trade, true);

		if (pInput->GetControllerMode())
			SetCurrentItem(nullptr);
	}
	else
	{
		if (!partner_infinivite_money && partner_money <= 0)
		{
			CallMessageBoxOK("not_enough_money_partner");
		}
		else
		{
			CallMessageBoxOK("trade_dont_make");
		}
	}
	if (!pInput->GetControllerMode())
		SetCurrentItem(nullptr);

	UpdateItemsPlace();
}

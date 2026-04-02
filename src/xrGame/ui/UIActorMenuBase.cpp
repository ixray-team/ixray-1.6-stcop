#include "StdAfx.h"
#include "UIActorMenuBase.h"

#include "UICellCustomItems.h"
#include "UICellItem.h"

#include "../xrUI/Widgets/UIPropertiesBox.h"
#include "../trade.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../medkit.h"
#include "../Weapon.h"
#include "../trade_parameters.h"
#include "UICellItemFactory.h"
#include "UIDragDropReferenceList.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIItemStateDisplay.h"
#include "UIInventoryUtilities.h"
#include "UIHelperGame.h"
#include "../MPPlayersBag.h"
#include "UIOutfitSlot.h"

void move_item_from_to (u16 from_id, u16 to_id, u16 what_id)
{
	NET_Packet P;
	CGameObject::u_EventGen					(P, GE_TRADE_SELL, from_id);
	P.w_u16									(what_id);
	CGameObject::u_EventSend				(P);

	//другому инвентарю - взять вещь 
	CGameObject::u_EventGen					(P, GE_TRADE_BUY, to_id);
	P.w_u16									(what_id);
	CGameObject::u_EventSend				(P);
}

bool is_item_in_list(CUIDragDropListEx* pList, PIItem item)
{
	for(u16 i=0;i<pList->ItemsCount();i++)
	{
		CUICellItem* cell_item = pList->GetItemIdx(i);
		for(u16 k=0;k<cell_item->ChildsCount();k++)
		{
			CUICellItem* inv_cell_item = cell_item->Child(k);
			if((PIItem)inv_cell_item->m_pData==item)
				return true;
		}
		if((PIItem)cell_item->m_pData==item)
			return true;
	}
	return false;
}

CUIActorMenuBase::CUIActorMenuBase()
{
	for (u8 i = 0; i <= 4; ++i)
	{
		m_QuickSlotsHighlight[i] = nullptr;
	}
	m_ArtefactSlotsHighlight.clear();
	LoadCallbackGlobals(m_isCanMoveToPartner, m_onCanMoveToPartner, "OnCanMoveToPartner");
	LoadCallbackGlobals(m_isDonateCurrentItem, m_onDonateCurrentItem, "OnDonateCurrentItem");
	LoadCallbackGlobals(m_isInvBoxCanTakeItem, m_onInvBoxCanTakeItem, "OnInvBoxCanTakeItem");		// FFx0001
	LoadCallbackGlobals(m_isInvBoxCanPlaceItem, m_onInvBoxCanPlaceItem, "OnInvBoxCanPlaceItem");	// FFx0001
	LoadCallbackGlobals(m_isCanTake, m_onCanTake, "OnCanTake");
	LoadCallbackGlobals(m_isItemDropped, m_onItemDropped, "OnItemDropped");
	LoadCallbackGlobals(m_isItemFocusLost, m_onItemFocusLost, "OnItemFocusLost");
	LoadCallbackGlobals(m_isItemFocusReceive, m_onItemFocusReceive, "OnItemFocusReceive");
}

CUIActorMenuBase::~CUIActorMenuBase()
{
	ClearAllLists();
}


void CUIActorMenuBase::InitSlots(CUIXml& uiXml)
{
	for (u8 i = 0; i <= LAST_SLOT; ++i)
	{
		m_pInvList[i] = nullptr;
		m_pInvSlotHighlight[i] = nullptr;
		m_pInvSlotProgressLegacy[i] = nullptr;
		m_pInvSlotProgressPercent[i] = nullptr;
	}

	XML_NODE* stored_root = uiXml.GetLocalRoot();
	XML_NODE* node = uiXml.NavigateToNode("inventory_slot_wnd", 0);
	uiXml.SetLocalRoot(node);

	u8 slot_count = uiXml.GetNodesNum(node, "slot");
	for (u8 i = 1; i <= slot_count; ++i)
	{
		uiXml.SetLocalRoot(node);
		XML_NODE* slot_node = uiXml.NavigateToNode("slot", i - 1);
		uiXml.SetLocalRoot(slot_node);

		if (uiXml.GetNodesNum(slot_node, "slot_dragdrop") == 0)
			continue;

		if (i == OUTFIT_SLOT && uiXml.ReadAttribBool("slot_dragdrop", 0, "use_full_scale_icon", false))
		{
			m_pInvList[i] = new CUIOutfitDragDropList();
		}
		else
		{
			m_pInvList[i] = new CUIDragDropListEx();
		}
		AttachChild(m_pInvList[i]);
		CUIXmlInitGame::InitDragDropListEx(uiXml, "slot_dragdrop", 0, m_pInvList[i]);
		m_pInvList[i]->SetAutoDelete(true);

		BindDragDropListEvents(m_pInvList[i]);

		if (uiXml.NavigateToNode("slot_highlight"))
		{
			m_pInvSlotHighlight[i] = new CUIStatic();
			AttachChild(m_pInvSlotHighlight[i]);
			CUIXmlInit::InitStatic(uiXml, "slot_highlight", 0, m_pInvSlotHighlight[i]);
			m_pInvSlotHighlight[i]->SetAutoDelete(true);
			m_pInvSlotHighlight[i]->Show(false);
		}

		const bool hasSlotProgress = uiXml.GetNodesNum(slot_node, "slot_progress") > 0;
		const bool hasSlotProgressPercent = uiXml.GetNodesNum(slot_node, "slot_progress_percent") > 0;

		if (hasSlotProgress)
		{
			m_pInvSlotProgressLegacy[i] = new CUIProgressBar();
			AttachChild(m_pInvSlotProgressLegacy[i]);
			CUIXmlInit::InitProgressBar(uiXml, "slot_progress", 0, m_pInvSlotProgressLegacy[i]);
			m_pInvSlotProgressLegacy[i]->SetAutoDelete(true);
		}

		if (hasSlotProgressPercent)
		{
			m_pInvSlotProgressPercent[i] = new CUIItemStateDisplay();
			AttachChild(m_pInvSlotProgressPercent[i]);
			CUIXmlInit::InitItemStateDisplay(uiXml, "slot_progress_percent", 0, m_pInvSlotProgressPercent[i]);
			m_pInvSlotProgressPercent[i]->SetAutoDelete(true);
		}
	}
	uiXml.SetLocalRoot(stored_root);
}

void CUIActorMenuBase::InitBase(CUIXml& uiXml)
{
	m_allowed_drops[iTrashSlot].push_back(iActorBag);
	m_allowed_drops[iTrashSlot].push_back(iActorSlot);
	m_allowed_drops[iTrashSlot].push_back(iActorBelt);
	m_allowed_drops[iTrashSlot].push_back(iQuickSlot);

	m_allowed_drops[iActorSlot].push_back(iActorBag);
	m_allowed_drops[iActorSlot].push_back(iActorSlot);
	m_allowed_drops[iActorSlot].push_back(iActorTrade);
	m_allowed_drops[iActorSlot].push_back(iDeadBodyBag);

	m_allowed_drops[iActorBag].push_back(iActorSlot);
	m_allowed_drops[iActorBag].push_back(iActorBelt);
	m_allowed_drops[iActorBag].push_back(iActorTrade);
	m_allowed_drops[iActorBag].push_back(iDeadBodyBag);
	m_allowed_drops[iActorBag].push_back(iActorBag);
	m_allowed_drops[iActorBag].push_back(iQuickSlot);
	
	m_allowed_drops[iActorBelt].push_back(iActorBag);
	m_allowed_drops[iActorBelt].push_back(iActorTrade);
	m_allowed_drops[iActorBelt].push_back(iDeadBodyBag);
	m_allowed_drops[iActorBelt].push_back(iActorBelt);

	m_allowed_drops[iActorTrade].push_back(iActorSlot);
	m_allowed_drops[iActorTrade].push_back(iActorBag);
	m_allowed_drops[iActorTrade].push_back(iActorBelt);
	m_allowed_drops[iActorTrade].push_back(iActorTrade);
	m_allowed_drops[iActorTrade].push_back(iQuickSlot);

	m_allowed_drops[iPartnerTradeBag].push_back(iPartnerTrade);
	m_allowed_drops[iPartnerTradeBag].push_back(iPartnerTradeBag);
	m_allowed_drops[iPartnerTrade].push_back(iPartnerTradeBag);
	m_allowed_drops[iPartnerTrade].push_back(iPartnerTrade);

	m_allowed_drops[iDeadBodyBag].push_back(iActorSlot);
	m_allowed_drops[iDeadBodyBag].push_back(iActorBag);
	m_allowed_drops[iDeadBodyBag].push_back(iActorBelt);
	m_allowed_drops[iDeadBodyBag].push_back(iDeadBodyBag);

	m_allowed_drops[iQuickSlot].push_back(iActorBag);
	m_allowed_drops[iQuickSlot].push_back(iActorTrade);

	XML_NODE* stored_root							= uiXml.GetLocalRoot	();
	uiXml.SetLocalRoot					(uiXml.NavigateToNode	("action_sounds",0));
	::Sound->create						(sounds[eSndOpen],		uiXml.Read("snd_open",			0,	"interface\\inv_open"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eSndClose],		uiXml.Read("snd_close",			0,	"interface\\inv_close"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemToSlot],	uiXml.Read("snd_item_to_slot",	0,	"interface\\inv_slot"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemToBelt],	uiXml.Read("snd_item_to_belt",	0,	"interface\\inv_belt"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemToRuck],	uiXml.Read("snd_item_to_ruck",	0,	"interface\\inv_ruck"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eProperties],	uiXml.Read("snd_properties",	0,	"interface\\inv_properties"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eDropItem],		uiXml.Read("snd_drop_item",		0,	"interface\\inv_drop"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eAttachAddon],	uiXml.Read("snd_attach_addon",	0,	"interface\\inv_attach_addon"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eDetachAddon],	uiXml.Read("snd_detach_addon",	0,	"interface\\inv_detach_addon"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemUse],		uiXml.Read("snd_item_use",		0,	"interface\\inv_slot"), st_Effect, sg_SourceType);
	uiXml.SetLocalRoot					(stored_root);
}

CUICellItem* CUIActorMenuBase::CurrentItem()
{
	return m_pCurrentCellItem;
}

PIItem CUIActorMenuBase::CurrentIItem()
{
	return m_pCurrentCellItem ? (PIItem)m_pCurrentCellItem->m_pData : nullptr;
}

float CUIActorMenuBase::CalcItemsWeight(CUIDragDropListEx* pList)
{
	float res = 0.0f;

	for( u32 i = 0; i < pList->ItemsCount(); ++i )
	{
		CUICellItem* itm	= pList->GetItemIdx(i);
		PIItem	iitem		= (PIItem)itm->m_pData;
		res					+= iitem->Weight();
		for( u32 j = 0; j < itm->ChildsCount(); ++j )
		{
			PIItem	jitem	= (PIItem)itm->Child(j)->m_pData;
			res				+= jitem->Weight();
		}
	}
	return res;
}

bool CUIActorMenuBase::CanMoveToPartner(PIItem pItem)
{
	if(!pItem->CanTrade())
		return false;

	if ( !GetPartner()->trade_parameters().enabled(
		CTradeParameters::action_buy(0), pItem->object().cNameSect() ) )
	{
		return false;
	}

	if(pItem->GetCondition()<GetPartner()->trade_parameters().buy_item_condition_factor)
		return false;

	float r1				= CalcItemsWeight( GetTradeActorList() );		// actor
	float r2				= CalcItemsWeight( GetTradePartnerList() );	// partner
	float itmWeight			 = pItem->Weight();
	float partner_inv_weight = GetPartner()->inventory().CalcTotalWeight();
	float partner_max_weight = GetPartner()->MaxCarryWeight();

	if ( partner_inv_weight - r2 + r1 + itmWeight > partner_max_weight )
	{
		return false;
	}

	if (m_isCanMoveToPartner)
	{
		luabind::functor<bool> funct;
		R_ASSERT2(ai().script_engine().functor(m_onCanMoveToPartner, funct), "failed to get OnCanMoveToPartner functor");
		
		if (funct(GetPartner()->cast_game_object()->lua_game_object(), pItem->object().lua_game_object(), r1, r2, itmWeight, partner_inv_weight, partner_max_weight) == false)
			return false;
	}

	return true;
}

bool CUIActorMenuBase::CanUpgradeItem( PIItem item )
{
	VERIFY( item );

	LPCSTR item_name = item->m_section_id.c_str();
	LPCSTR partner = GetPartner() ? GetPartner()->CharacterInfo().Profile().c_str() : Actor()->CharacterInfo().Profile().c_str();
	LPCSTR section = GetPartner() ? GetPartner()->cast_game_object()->cNameSect_str() : Actor()->cast_game_object()->cNameSect_str();
		
	luabind::functor<bool> funct; 
	R_ASSERT2(
		ai().script_engine().functor( "inventory_upgrades.can_upgrade_item", funct ),
		make_string<const char*>( "Failed to get functor <inventory_upgrades.can_upgrade_item>, item = %s, mechanic = %s", item_name, partner )
		);

	return funct( item_name, partner, section);
}

void CUIActorMenuBase::PlaySnd(eActorMenuSndAction a)
{
	if (sounds[a].handle())
		sounds[a].play(nullptr, sm_2D);
}

CUIDragDropListEx* CUIActorMenuBase::GetListByType(EDDListType t)
{
	switch(t)
	{
		case iActorBag:
			{
				if(m_currMenuMode==mmTrade)
					return GetTradeActorBagList();
				else
					return GetActorList();
			}break;
		case iDeadBodyBag:
			{
				return GetPartnerList();
			}break;
		case iActorBelt:
			{
				return GetBeltList();
			}break;
		default:
			{
				R_ASSERT("invalid call");
			}break;
	}
	return nullptr;
}

void CUIActorMenuBase::ColorizeItem(CUICellItem* itm, bool colorize)
{
	if( colorize )
	{
		itm->SetTextureColor( color_rgba(255,100,100,255) );
	}
	else
	{
		itm->SetTextureColor( color_rgba(255,255,255,255) );
	}
}

bool  CUIActorMenuBase::AllowItemDrops(EDDListType from, EDDListType to)
{
	xr_vector<EDDListType>& v = m_allowed_drops[to];
	xr_vector<EDDListType>::iterator it = std::find(v.begin(), v.end(), from);

	return(it!=v.end());
}

u32 CUIActorMenuBase::CalcItemsPrice(CUIDragDropListEx* pList, CTrade* pTrade, bool bBuying)
{
	u32 iPrice				= 0;
	
	for (u32 i = 0; i < pList->ItemsCount(); ++i)
	{
		CUICellItem* itm	= pList->GetItemIdx(i);
		PIItem iitem		= (PIItem)itm->m_pData;
		iPrice				+= pTrade->GetItemPrice(iitem, bBuying);

		for (u32 j = 0; j < itm->ChildsCount(); ++j)
		{
			PIItem jitem	= (PIItem)itm->Child(j)->m_pData;
			iPrice			+= pTrade->GetItemPrice(jitem, bBuying);
		}
	}

	return					iPrice;
}

bool CUIActorMenuBase::CanSetItemToList(PIItem item, CUIDragDropListEx* l, u16& ret_slot)
{
	u16 item_slot = item->BaseSlot();
	if( GetSlotList(item_slot)==l )
	{
		ret_slot	= item_slot;
		return		true;
	}

	const static bool pistolsOnly = EngineExternal()[EEngineExternalGame::EnableInventoryPistolSlot];
	if (item_slot == INV_SLOT_3 && l == m_pInvList[INV_SLOT_2] && !pistolsOnly)
	{
		ret_slot	= INV_SLOT_2;
		return		true;
	}

	if (item_slot == INV_SLOT_2&& l == m_pInvList[INV_SLOT_3] && !pistolsOnly)
	{
		ret_slot	= INV_SLOT_3;
		return		true;
	}

	return false;
}

void CUIActorMenuBase::UpdateConditionProgressBars()
{
	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		PIItem itm = GetInventoryOwner()->inventory().ItemFromSlot(i);
		const bool hasLegacy = m_pInvSlotProgressLegacy[i] != nullptr;
		const bool hasPercent = m_pInvSlotProgressPercent[i] != nullptr;

		if (!hasLegacy && !hasPercent)
		{
			continue;
		}

		if (!itm)
		{
			if (hasLegacy)
			{
				m_pInvSlotProgressLegacy[i]->Show(false);
			}
			if (hasPercent)
			{
				m_pInvSlotProgressPercent[i]->Show(false);
			}
			continue;
		}

		const InventoryUtilities::ConditionDisplayParams display =
			InventoryUtilities::GetConditionDisplayParams(itm);

		if (hasLegacy)
		{
			m_pInvSlotProgressLegacy[i]->SetProgressPos(display.state);
			m_pInvSlotProgressLegacy[i]->m_bUseGradient = !display.disableGradient;
			m_pInvSlotProgressLegacy[i]->ShowBackground(!display.hideBackground);
			m_pInvSlotProgressLegacy[i]->Show(true);
		}

		if (hasPercent)
		{
			CUIItemStateDisplay* stateDisplay = m_pInvSlotProgressPercent[i];
			stateDisplay->Show(true);

			CEatableItem* eatableItem = itm->cast_eatable_item();
			if (stateDisplay->GetPercentFormat() == CUIItemStateDisplay::EPercentFormat::Portion &&
				eatableItem != nullptr &&
				display.usePortion &&
				display.portionMax > 1)
			{
				stateDisplay->SetPortion(display.portionCurrent, display.portionMax);
			}
			else
			{
				stateDisplay->SetState(display.state);
			}
		}
	}

	//Highlight 'equipped' items in actor bag
	CUIDragDropListEx* slot_list = GetActorList();
	if (slot_list)
	{
		u32 const cnt = slot_list->ItemsCount();
		for (u32 i = 0; i < cnt; ++i)
		{
			CUICellItem* ci = slot_list->GetItemIdx(i);
			PIItem item = (PIItem)ci->m_pData;
			if (!item)
				continue;

			if ((item->m_highlight_equipped || ForceHighlightForSlots()) && item->m_pInventory && item->m_pInventory->ItemFromSlot(item->BaseSlot()) == item)
				ci->m_select_equipped = true;
			else
				ci->m_select_equipped = false;
		}
	}
}

EDDListType CUIActorMenuBase::GetListType(CUIDragDropListEx* l)
{
	if(l==GetActorList() && GetActorList())			return iActorBag;
	if(l==GetBeltList() && GetBeltList())			return iActorBelt;

	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		if (m_pInvList[i] && m_pInvList[i] == l)
			return iActorSlot;
	}

	if(l==GetTradeActorBagList() && GetTradeActorBagList())			return iActorBag;
	if(l==GetTradeActorList() && GetTradeActorList())			return iActorTrade;
	if(l==GetTradePartnerBagList() && GetTradePartnerBagList())		return iPartnerTradeBag;
	if(l==GetTradePartnerList() && GetTradePartnerList())			return iPartnerTrade;
	if(l==GetPartnerList() && GetPartnerList())			return iDeadBodyBag;

	if(l==m_pQuickSlot && m_pQuickSlot)					return iQuickSlot;
	if(l==m_pTrashList && m_pTrashList)					return iTrashSlot;

	R_ASSERT(false, "Invalid call in function", __FUNCTION__);
	
	return iInvalid;
}

CUIDragDropListEx* CUIActorMenuBase::GetSlotList(u16 slot_idx)
{
	if (slot_idx == NO_ACTIVE_SLOT)
	{
		return nullptr;
	}

	if (slot_idx == GRENADE_SLOT)
	{
		//fake
		if (m_currMenuMode == mmTrade)
		{
			return GetTradeActorBagList();
		}
		return GetActorList();
	}

	if (m_pInvList[slot_idx])
		return m_pInvList[slot_idx];

	if (m_currMenuMode == mmTrade)
		return GetTradeActorBagList();

	return GetActorList();
}


#define CLEAR_LIST(list) if (list) list->ClearAll(true);
void CUIActorMenuBase::ClearAllLists()
{
	CLEAR_LIST(GetActorList())
	CLEAR_LIST(GetBeltList())

	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		CLEAR_LIST(m_pInvList[i])
	}

	CLEAR_LIST(m_pQuickSlot)

	CLEAR_LIST(GetTradeActorBagList())
	CLEAR_LIST(GetTradeActorList())
	CLEAR_LIST(GetTradePartnerBagList())
	CLEAR_LIST(GetTradePartnerList())
	CLEAR_LIST(GetPartnerList())
}

void CUIActorMenuBase::InitInventoryContents(CUIDragDropListEx* pBagList)
{
	ClearAllLists				();
	m_pMouseCapturer			= nullptr;
	m_UIPropertiesBox->Hide		();
	SetCurrentItem				(nullptr);

	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		if (m_pInvList[i])
			InitCellForSlot(i);
		else
		{
			if (i != BOLT_SLOT && i != PDA_SLOT && !GetInventoryOwner()->inventory().SlotIsPersistent(i))
				InitCellForSlot(i);
		}
	}


	CUIDragDropListEx* curr_list = GetBeltList();
	TIItemContainer::iterator itb = GetInventoryOwner()->inventory().m_belt.begin();
	TIItemContainer::iterator ite = GetInventoryOwner()->inventory().m_belt.end();
	if (curr_list)
	{
		for (; itb != ite; ++itb)
		{
			CUICellItem* itm = create_cell_item(*itb);
			curr_list->SetItem(itm);
			if (m_currMenuMode == mmTrade && GetPartner())
				ColorizeItem(itm, !CanMoveToPartner(*itb));
		}
	}

	TIItemContainer				ruck_list;
	ruck_list					= GetInventoryOwner()->inventory().m_ruck;
	std::sort					( ruck_list.begin(), ruck_list.end(), InventoryUtilities::GreaterRoomInRuck );

	curr_list					= pBagList;

	if ((m_currMenuMode == mmInventory || m_currMenuMode == mmUpgrade) && m_pInventorySorter)
	{
		m_pInventorySorter->SortItems(ruck_list, GetPlayerSortCategory());
	}

	itb = ruck_list.begin();
	ite = ruck_list.end();
	for ( ; itb != ite; ++itb )
	{
		CMPPlayersBag* bag = smart_cast<CMPPlayersBag*>( &(*itb)->object() );
		if ( bag )
			continue;

		CUICellItem* itm = create_cell_item( *itb );
		curr_list->SetItem(itm);
		if ( m_currMenuMode == mmTrade && GetInventoryOwner() )
			ColorizeItem( itm, !CanMoveToPartner( *itb ) );
	}

	if (m_pQuickSlot)
		m_pQuickSlot->ReloadReferences(GetInventoryOwner());
}

void CUIActorMenuBase::InitCellForSlot( u16 slot_idx )
{
	VERIFY( KNIFE_SLOT <= slot_idx && slot_idx <= LAST_SLOT );
	PIItem item	= GetInventoryOwner()->inventory().ItemFromSlot(slot_idx);
	if ( !item )
	{
		return;
	}

	if (slot_idx == GRENADE_SLOT && m_currMenuMode == mmInventory && m_pInventorySorter &&
		GetPlayerSortCategory() != EInventorySortCategory::All &&
		GetPlayerSortCategory() != EInventorySortCategory::Ammo)
	{
		return;
	}

	CUIDragDropListEx* curr_list	= GetSlotList( slot_idx );
	CUICellItem* cell_item			= create_cell_item( item );
	curr_list->SetItem( cell_item );
	if ( m_currMenuMode == mmTrade && GetPartner() )
		ColorizeItem( cell_item, !CanMoveToPartner( item ) );
}

EInventorySortCategory CUIActorMenuBase::GetPlayerSortCategory() const
{
	if (m_currMenuMode == mmUpgrade)
	{
		return m_sortCategory[eSortTabsUpgrade];
	}

	if (m_currMenuMode == mmTrade)
	{
		return m_sortCategory[eSortTabsTradeActor];
	}

	return m_sortCategory[eSortTabsInventory];
}

void CUIActorMenuBase::UpdateDeadBodyBagList()
{
	if (!GetPartnerList())
	{
		return;
	}

	GetPartnerList()->ClearAll(true);

	TIItemContainer items_list;
	if (GetPartner())
	{
		GetPartner()->inventory().AddAvailableItems(items_list, false);
	}
	else
	{
		VERIFY(GetInvBox());
		GetInvBox()->set_in_use(true);
		GetInvBox()->AddAvailableItems(items_list);
	}

	std::sort(items_list.begin(), items_list.end(), InventoryUtilities::GreaterRoomInRuck);
	if (m_pInventorySorter)
	{
		m_pInventorySorter->SortItems(items_list, m_sortCategory[eSortTabsDeadBody]);
	}

	for (PIItem item : items_list)
	{
		CUICellItem* itm = create_cell_item(item);
		GetPartnerList()->SetItem(itm);
	}
}

void CUIActorMenuBase::UpdateItemsPlace()
{
	switch ( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		
		break;
	case mmTrade:
		UpdatePrices();
		break;
	case mmUpgrade:
		SetupUpgradeItem();
		break;
	case mmDeadBodySearch:
		UpdateDeadBodyBag();
		break;
	default:
		R_ASSERT(0);
		break;
	}

	if ( GetInventoryOwner() )
	{
		UpdateOutfit();
		UpdateActor();
	}
}

void CUIActorMenuBase::InitPartnerInventoryContents()
{
	GetTradePartnerBagList()->ClearAll(true);

	TIItemContainer					items_list;
	GetPartner()->inventory().AddAvailableItems(items_list, true);
	std::sort						(items_list.begin(), items_list.end(),InventoryUtilities::GreaterRoomInRuck);
	if (m_pInventorySorter)
	{
		m_pInventorySorter->SortItems(items_list, m_sortCategory[eSortTabsTradePartner]);
	}

	TIItemContainer::iterator itb = items_list.begin();
	TIItemContainer::iterator ite = items_list.end();
	for( ; itb != ite; ++itb ) 
	{
		if(!is_item_in_list(GetTradePartnerList(), *itb))
		{
			CUICellItem* itm			= create_cell_item( *itb );
			GetTradePartnerBagList()->SetItem(itm);
		}
	}
	m_trade_partner_inventory_state = GetPartner()->inventory().ModifyFrame();
}

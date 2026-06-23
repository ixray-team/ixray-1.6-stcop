#include "StdAfx.h"
#include "UIActorMenuBase.h"

#include "UICellCustomItems.h"
#include "UICellItem.h"

#include "../xrUI/Widgets/UIPropertiesBox.h"
#include "../trade.h"
#include "../Inventory.h"
#include "../InventoryWeaponSlotLayout.h"
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
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"
#include "UIGameCustom.h"
#include "UITalkWnd.h"
#include "UITalkDialogWnd.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UIInventoryUpgradeWnd.h"

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

	EGameActions repeatActions[] = {
		kUI_LEFT, kUI_RIGHT, kUI_UP, kUI_DOWN,
		kUI_SECONDARY_LEFT, kUI_SECONDARY_RIGHT, kUI_SECONDARY_UP, kUI_SECONDARY_DOWN,
		kUI_TAB_SECONDARY_LEFT, kUI_TAB_SECONDARY_RIGHT
	};

	for (int i = 0; i < sizeof(repeatActions) / sizeof(repeatActions[0]); ++i)
		ActionRepeaters()->Register(this, repeatActions[i]);
}

CUIActorMenuBase::~CUIActorMenuBase()
{
	ActionRepeaters()->UnregisterOwner(this);

	xr_delete(m_pInventorySorter);

	xr_delete(m_ui_navigation_selector);
	xr_delete(m_ui_aux_selector);

	ClearAllLists();

	for (size_t i = 0; i < m_ArtefactSlotsHighlight.size(); i++)
	{
		m_ArtefactSlotsHighlight[i] = nullptr;
		m_belt_list_over[i] = nullptr;
	}
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

void CUIActorMenuBase::InitGamepadSelectors()
{
	m_ui_navigation_selector = new CUIFrameWindow();
	if (m_ui_navigation_selector->InitTexture("ui_inv_item_selector", false))
	{
		m_ui_navigation_selector->SetWidth(0);
		m_ui_navigation_selector->SetHeight(0);
		m_ui_navigation_selector->SetVisible(false);
		AttachChild(m_ui_navigation_selector);
	}
	else
	{
		xr_delete(m_ui_navigation_selector);
	}

	m_ui_aux_selector = new CUIFrameWindow();
	if (m_ui_aux_selector->InitTexture("ui_inv_item_selector_tri", false))
	{
		m_ui_aux_selector->SetWidth(0);
		m_ui_aux_selector->SetHeight(0);
		m_ui_aux_selector->SetVisible(false);
		AttachChild(m_ui_aux_selector);
	}
	else
	{
		xr_delete(m_ui_aux_selector);
	}
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
		res					+= iitem->m_pInventory ? iitem->m_pInventory->CalcItemWeight(iitem) : iitem->Weight();
		for( u32 j = 0; j < itm->ChildsCount(); ++j )
		{
			PIItem	jitem	= (PIItem)itm->Child(j)->m_pData;
			res += jitem->m_pInventory ? jitem->m_pInventory->CalcItemWeight(jitem) : jitem->Weight();
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
	float itmWeight = pItem->m_pInventory ? pItem->m_pInventory->CalcItemWeight(pItem) : pItem->Weight();
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

	const char* item_name = item->m_section_id.c_str();
	const char* partner = GetPartner() ? GetPartner()->CharacterInfo().Profile().c_str() : Actor()->CharacterInfo().Profile().c_str();
	const char* section = GetPartner() ? GetPartner()->cast_game_object()->cNameSect_str() : Actor()->cast_game_object()->cNameSect_str();
		
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
	if (GetSlotList(item_slot) == l)
	{
		ret_slot	= item_slot;
		return		true;
	}

	// Sidearm items keep BaseSlot INV_SLOT_2; map drops onto the dedicated holster list to PISTOL_SLOT_NEW.
	if (m_pInvList[PISTOL_SLOT_NEW] && l == m_pInvList[PISTOL_SLOT_NEW] && item_slot == INV_SLOT_2)
	{
		if (GetInventoryOwner()->inventory().CanPutInSlot(item, PISTOL_SLOT_NEW, true))
		{
			ret_slot	= PISTOL_SLOT_NEW;
			return		true;
		}
		return false;
	}

	if (InventoryHolsterPistolSlotActiveInSettings() &&
		m_pInvList[PISTOL_SLOT_NEW] &&
		l == m_pInvList[INV_SLOT_2] &&
		InventoryHolsterExclusivePistolFootprint(item))
	{
		if (GetInventoryOwner()->inventory().CanPutInSlot(item, PISTOL_SLOT_NEW, true))
		{
			ret_slot	= PISTOL_SLOT_NEW;
			return		true;
		}
	}

	if (item_slot == INV_SLOT_3 && l == m_pInvList[INV_SLOT_2] && !InventorySecondarySlotPairingStrict())
	{
		ret_slot	= INV_SLOT_2;
		return		true;
	}

	if (item_slot == INV_SLOT_2 && l == m_pInvList[INV_SLOT_3] && !InventorySecondarySlotPairingStrict())
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

	if (m_pInvList[slot_idx])
		return m_pInvList[slot_idx];

	if (m_currMenuMode == mmTrade)
		return GetTradeActorBagList();

	return GetActorList();
}

CUIDragDropListEx* CUIActorMenuBase::GetDisplayListForItem(PIItem item, const SInvItemPlace& place)
{
	if (item == nullptr)
	{
		return nullptr;
	}

	if (place.type == eItemPlaceSlot)
	{
		const u16 slotId = place.slot_id;
		if (IsSlotHiddenInUi(slotId))
		{
			if (slotId == GRENADE_SLOT)
			{
				if (!ShouldDisplayGrenadeInBag())
				{
					return nullptr;
				}
			}

			if (m_currMenuMode == mmTrade)
			{
				return GetTradeActorBagList();
			}
			return GetActorList();
		}

		return GetSlotList(slotId);
	}

	if (place.type == eItemPlaceBelt)
	{
		return GetListByType(iActorBelt);
	}

	if (item->parent_id() == GetInventoryOwner()->object_id())
	{
		return GetListByType(iActorBag);
	}

	return GetListByType(iDeadBodyBag);
}

bool CUIActorMenuBase::IsSlotHiddenInUi(u16 slot_idx) const
{
	if (slot_idx == NO_ACTIVE_SLOT || slot_idx > LAST_SLOT)
	{
		return false;
	}

	return m_pInvList[slot_idx] == nullptr;
}

bool CUIActorMenuBase::ShouldDisplayGrenadeInBag() const
{
	if (m_currMenuMode != mmInventory || m_pInventorySorter == nullptr)
	{
		return true;
	}

	const EInventorySortCategory category = GetPlayerSortCategory();
	return category == EInventorySortCategory::All || category == EInventorySortCategory::Ammo;
}

CUIDragDropListEx* CUIActorMenuBase::GetSidearmDragDropList() const
{
	if (m_pInvList[PISTOL_SLOT_NEW] != nullptr)
	{
		return m_pInvList[PISTOL_SLOT_NEW];
	}
	return m_pInvList[INV_SLOT_2];
}

CUIDragDropListEx* CUIActorMenuBase::GetPrimaryDragDropList() const
{
	return m_pInvList[INV_SLOT_3];
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

	if (GetInventoryOwner())
	{
		GetInventoryOwner()->inventory().RepairItemPlacements();
	}

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

	if (slot_idx == GRENADE_SLOT && !ShouldDisplayGrenadeInBag() && IsSlotHiddenInUi(slot_idx))
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


// Controller UI

bool CUIActorMenuBase::MoveAreaSelector(eUIDirection4 dir)
{
	xr_vector<WND_SELECTOR_INFO>& uiNaviList = m_ui_navigation_lists[m_currMenuMode];
	for (xr_vector<WND_SELECTOR_INFO>::iterator it = uiNaviList.begin(); it != uiNaviList.end(); ++it)
	{
		const WND_SELECTOR_INFO& wsinfo = *it;
		if (wsinfo.pWnd == m_ui_navigation_selection)
		{
			switch (dir)
			{
				case eUIDirection4_Left:
				{
					if (wsinfo.pWndLeft)
					{
						SetAreaSelectionTo(wsinfo.pWndLeft);
						return true;
					}
					break;
				}
				case eUIDirection4_Right:
				{
					if (wsinfo.pWndRight)
					{
						SetAreaSelectionTo(wsinfo.pWndRight);
						return true;
					}
					break;
				}
				case eUIDirection4_Up:
				{
					if (wsinfo.pWndTop)
					{
						SetAreaSelectionTo(wsinfo.pWndTop);
						return true;
					}
					break;
				}
				case eUIDirection4_Down:
				{
					if (wsinfo.pWndBottom)
					{
						SetAreaSelectionTo(wsinfo.pWndBottom);
						return true;
					}
					break;
				}
			}
			return false;
		}
	}
	return false;
}

eUIDirection4 CUIActorMenuBase::GetNaviDirection(CUIWindow* pWndFrom, CUIWindow* pWndTo)
{
	xr_vector<WND_SELECTOR_INFO>& naviList = m_ui_navigation_lists[m_currMenuMode];
	for (xr_vector<WND_SELECTOR_INFO>::iterator it = naviList.begin(); it != naviList.end(); ++it)
	{
		if (it->pWnd == pWndFrom)
		{
			if (it->pWndLeft == pWndTo) return eUIDirection4::eUIDirection4_Left;
			else if (it->pWndRight == pWndTo) return eUIDirection4::eUIDirection4_Right;
			else if (it->pWndTop == pWndTo) return eUIDirection4::eUIDirection4_Up;
			else if (it->pWndBottom == pWndTo) return eUIDirection4::eUIDirection4_Down;
		}
	}

	return eUIDirection4::eUIDirection4_None;
}


void CUIActorMenuBase::SetAreaSelectionTo(CUIWindow* pSelection)
{
	if (pSelection == m_ui_navigation_selection || !pInput->GetControllerMode() || !m_ui_navigation_selector)
		return;

	InfoCurItem(nullptr);

	// Deselect old wnd
	CUIWindow* pOldNavSelection = m_ui_navigation_selection;
	if (m_ui_navigation_selection)
	{
		CUIDragDropListEx* pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
		if (pDragDropList)
			pDragDropList->SetControllerFocusOut();
	}

	m_ui_navigation_selection = pSelection;

	// Update selector frame
	if (m_ui_navigation_selection != nullptr)
	{
		Fvector2 frmSize = { 0, 0 };
		Fvector2 frmPos = { 0, 0 };

		CUIDragDropListEx* pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
		if (pDragDropList)
		{
			frmSize = pSelection->GetWndSize();
			frmSize.add(m_selectorPadding * 2);

			pSelection->GetAbsolutePos(frmPos);
			frmPos.sub(m_selectorPadding);
			frmPos.x -= m_wndPos.x;
			frmPos.y -= m_wndPos.y;

			// If previous selection was a CUIDragDropListEx too
			// try placing an internal selector close to the internal selector of the old list
			// for slots move selector to the only 1 item that is in the list
			bool bSetDefaultSelectorPos = true;
			if (GetListType(pDragDropList) == EDDListType::iActorSlot)
			{
				if (pDragDropList->ItemsCount() > 0)
				{
					CUICellItem* pItem = pDragDropList->GetItemIdx(0);
					R_ASSERT(pItem);
					if (pItem && pDragDropList->MoveSelectorToItem(pItem))
					{
						bSetDefaultSelectorPos = false;
					}
				}
			}
			else
			{
				CUIDragDropListEx* pOldList = dynamic_cast<CUIDragDropListEx*>(pOldNavSelection);
				if (pOldList && pOldList->HasCells() && pDragDropList->HasCells())
				{
					CUICellContainer* pOldContainer = pOldList->GetContainer();
					CUICellContainer* pNewContainer = pDragDropList->GetContainer();

					Irect oldSelectorArea = pOldContainer->GetSelectorArea();
					Ivector2 selectorPos = { 0,0 };

					// direction they moved to reach us
					eUIDirection4 dir = GetNaviDirection(pOldList, pDragDropList);
					switch (dir)
					{
						case eUIDirection4::eUIDirection4_Left:
						{
							selectorPos.y = oldSelectorArea.y1;
							selectorPos.x = pNewContainer->CellsCapacity().x - 1;
							bSetDefaultSelectorPos = false;
							break;
						}
						case eUIDirection4::eUIDirection4_Right:
						{
							selectorPos.y = oldSelectorArea.y1;
							selectorPos.x = 0;
							bSetDefaultSelectorPos = false;
							break;
						}
						case eUIDirection4::eUIDirection4_Down:
						{
							selectorPos.x = oldSelectorArea.x1;
							selectorPos.y = 0;
							bSetDefaultSelectorPos = false;
							break;
						}
						case eUIDirection4::eUIDirection4_Up:
						{
							selectorPos.x = oldSelectorArea.x1;
							selectorPos.y = pNewContainer->CellsCapacity().y - 1;
							bSetDefaultSelectorPos = false;
							break;
						}
					}

					if (!bSetDefaultSelectorPos)
					{
						Irect selector;
						selector.lt = selectorPos;
						selector.x2 = selector.x1 + 1;
						selector.y2 = selector.y1 + 1;
						pDragDropList->SetControllerFocusIn(selector);
					}
				}
			}
			
			if (bSetDefaultSelectorPos)
			{
				pDragDropList->SetControllerFocusIn({ 0,0,1,1 });
			}
		}

		if (frmSize.x > 0 && frmSize.y > 0)
		{
			m_ui_navigation_selector->SetWndSize(frmSize);
			m_ui_navigation_selector->SetWndPos(frmPos);
			m_ui_navigation_selector_shown = true;
		}
		else
		{
			m_ui_navigation_selector_shown = false;
		}
	}
	else
	{
		m_ui_navigation_selector_shown = false;
	}

	UpdateSortTabsLayout();
	ShowSortTabsForCurrentMode();
}

void CUIActorMenuBase::UpdateSortTabsLayout()
{
	for (u8 i = 0; i < eSortTabsLayoutCount; ++i)
	{
		if (!m_sortTabControl[i] || !m_sortTabsLayoutDefined[i])
		{
			continue;
		}

		m_sortTabControl[i]->SetWndPos(m_sortTabsLayoutPos[i]);
		m_sortTabControl[i]->SetWndSize(m_sortTabsLayoutSize[i]);
	}
}

void CUIActorMenuBase::ShowSortTabsForCurrentMode()
{
	for (u8 i = 0; i < eSortTabsLayoutCount; ++i)
	{
		if (!m_sortTabControl[i])
		{
			continue;
		}

		m_sortTabControl[i]->Show(false);
		m_sortTabControl[i]->Enable(false);
	}

	switch (m_currMenuMode)
	{
	case mmInventory:
		if (m_sortTabControl[eSortTabsInventory])
		{
			m_sortTabControl[eSortTabsInventory]->Show(true);
			m_sortTabControl[eSortTabsInventory]->Enable(true);
		}
		break;
	case mmUpgrade:
		if (m_sortTabControl[eSortTabsUpgrade])
		{
			m_sortTabControl[eSortTabsUpgrade]->Show(true);
			m_sortTabControl[eSortTabsUpgrade]->Enable(true);
		}
		break;
	case mmTrade:
		if (m_sortTabControl[eSortTabsTradeActor])
		{
			m_sortTabControl[eSortTabsTradeActor]->Show(true);
			m_sortTabControl[eSortTabsTradeActor]->Enable(true);
		}
		if (m_sortTabControl[eSortTabsTradePartner])
		{
			m_sortTabControl[eSortTabsTradePartner]->Show(true);
			m_sortTabControl[eSortTabsTradePartner]->Enable(true);
		}
		break;
	case mmDeadBodySearch:
		if (m_sortTabControl[eSortTabsInventory])
		{
			m_sortTabControl[eSortTabsInventory]->Show(true);
			m_sortTabControl[eSortTabsInventory]->Enable(true);
		}
		if (m_sortTabControl[eSortTabsDeadBody])
		{
			m_sortTabControl[eSortTabsDeadBody]->Show(true);
			m_sortTabControl[eSortTabsDeadBody]->Enable(true);
		}
		break;
	default:
		break;
	}
}

CUITabControl* CUIActorMenuBase::GetActiveSortTabControl() const
{
	if (m_currMenuMode != mmInventory)
	{
		return nullptr;
	}

	CUITabControl* sortTabControl = m_sortTabControl[eSortTabsInventory];
	if (!sortTabControl || !sortTabControl->IsShown() || !sortTabControl->IsEnabled())
	{
		return nullptr;
	}

	return sortTabControl;
}

bool CUIActorMenuBase::ProcessSortTabKeyboardSwitch(int dik, EUIMessages keyboard_action)
{
	if (keyboard_action != WINDOW_KEY_PRESSED)
	{
		return false;
	}

	CUITabControl* sortTabControl = GetActiveSortTabControl();
	if (!sortTabControl)
	{
		return false;
	}

	const bool hasSortPrevBinding = get_action_dik(kINV_SORT_PREV, 0) != 0 || get_action_dik(kINV_SORT_PREV, 1) != 0;
	const bool hasSortNextBinding = get_action_dik(kINV_SORT_NEXT, 0) != 0 || get_action_dik(kINV_SORT_NEXT, 1) != 0;

	const bool isSortPrevPressed = is_binded(kINV_SORT_PREV, dik) || (!hasSortPrevBinding && dik == SDL_SCANCODE_Q);
	if (isSortPrevPressed)
	{
		sortTabControl->PrevTab(true);
		return true;
	}

	const bool isSortNextPressed = is_binded(kINV_SORT_NEXT, dik) || (!hasSortNextBinding && dik == SDL_SCANCODE_E);
	if (isSortNextPressed)
	{
		sortTabControl->NextTab(true);
		return true;
	}

	return false;
}

void CUIActorMenuBase::Update()
{
	if (pInput->GetControllerMode())
	{
		CheckSelectors();
	}

	inherited::Update();
	if (m_ui_navigation_selector)
	{
		m_ui_navigation_selector->SetVisible(m_ui_navigation_selection && m_ui_navigation_selector_shown && pInput->GetControllerMode());
	}
	if (m_ui_aux_selector)
	{
		m_ui_aux_selector->SetVisible(m_ui_aux_selector_shown && pInput->GetControllerMode());
	}
	UpdateGamepadLegend();
}

void CUIActorMenuBase::CheckSelectors()
{
	CUIDragDropListEx* pDragDrop = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
	if (pDragDrop && pDragDrop->HasCells())
	{
		pDragDrop->UpdateSelector();

		CUICellItem* pDDSelected = pDragDrop->GetSelectedItem();
		if (pDDSelected != m_pCurrentCellItem)
		{
			SetCurrentItem(pDDSelected);
		}
	}
	else
	{
		SetCurrentItem(nullptr);
	}
}

void CUIActorMenuBase::UpdateGamepadLegend()
{
	if (!m_gamepad_legend)
		return;

	m_gamepad_legend->Show(!(m_pItemDropAmountWnd && m_pItemDropAmountWnd->IsShown()));
	CUIWindow* amAction = m_gamepad_legend->FindChild("am_action");
	if (amAction)
	{
		amAction->Show(!m_UIPropertiesBox->IsShown() && m_currMenuMode != mmInventory);
		CUIStatic* amActionS = amAction->ui_cast_static();
		if (amActionS)
		{
			switch (m_currMenuMode)
			{
			case mmDeadBodySearch:
			{
				CUIDragDropListEx* pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
				if (pDragDropList && pDragDropList == GetActorList())
				{
					amActionS->SetTextST("ui_am_put_all");
				}
				else
				{
					amActionS->SetTextST("ui_am_take_all");
				}
				break;
			}
			case mmUpgrade:
			{
				amActionS->SetTextST("ui_am_repair");
				break;
			}
			case mmTrade:
			{
				amActionS->SetTextST("ui_am_trade");
				break;
			}
			}
		}
	}

	CUIWindow* amInfo = m_gamepad_legend->FindChild("am_info");
	if (amInfo)
	{
		amInfo->Show(!m_UIPropertiesBox->IsShown());
	}

	CUIWindow* amActions = m_gamepad_legend->FindChild("am_actions");
	if (amActions)
	{
		amActions->Show(!m_UIPropertiesBox->IsShown());
	}

	CUIWindow* amUse = m_gamepad_legend->FindChild("am_use");
	if (amUse)
	{
		CUIStatic* amUseS = amUse->ui_cast_static();
		if (amUseS)
		{
			if (m_UIPropertiesBox->IsShown())
			{
				amUseS->SetTextST("ui_am_propbox_select");
			}
			else if (m_currMenuMode == mmUpgrade)
			{
				amUseS->SetTextST("ui_am_upgrade");
			}
			else
			{
				amUseS->SetTextST("ui_am_use");
			}
		}
	}
}

bool CUIActorMenuBase::OnMouseAction( float x, float y, EUIMessages mouse_action )
{
	inherited::OnMouseAction( x, y, mouse_action );
	return true; // no click`s
}

bool CUIActorMenuBase::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	InfoCurItem( nullptr );
	if ( is_binded(kDROP, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action && CurrentIItem() && !CurrentIItem()->IsQuestItem()
			&& CurrentIItem()->parent_id()==GetInventoryOwner()->object_id())
		{

			SendEvent_Item_Drop		(CurrentIItem(), GetInventoryOwner()->object_id());
			SetCurrentItem			(nullptr);
		}
		return true;
	}

	if ( is_binded(kSPRINT_TOGGLE, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			OnPressUserKey();
		}
		return true;
	}	

	if (ProcessSortTabKeyboardSwitch(dik, keyboard_action))
	{
		return true;
	}

	if ( is_binded(kUSE, dik) || is_binded(kINVENTORY, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			g_btnHint->Discard();
			HideDialog();

			if (GetInventoryOwner()->IsTalking())
				CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
		}
		return true;
	}	

	if ( is_binded(kQUIT, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			g_btnHint->Discard();
			HideDialog();

			if (GetInventoryOwner()->IsTalking())
				CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
		}
		return true;
	}

#ifdef DEBUG
	if (WINDOW_KEY_PRESSED == keyboard_action)
	{
		{
			if (SDL_SCANCODE_KP_7 == dik && CurrentIItem() && CurrentIItem()->IsUsingCondition())
			{
				CurrentIItem()->ChangeCondition(-0.05f);
				UpdateConditionProgressBars();
				m_pCurrentCellItem->UpdateConditionProgressBar();
			}
			else if (SDL_SCANCODE_KP_8 == dik && CurrentIItem() && CurrentIItem()->IsUsingCondition())
			{
				CurrentIItem()->ChangeCondition(0.05f);
				UpdateConditionProgressBars();
				m_pCurrentCellItem->UpdateConditionProgressBar();
			}
		}
	}
#endif
	if( inherited::OnKeyboardAction(dik,keyboard_action) )return true;

	return false;
}

bool CUIActorMenuBase::OnKeyboardHold(int dik)
{
	if (!GetActiveSortTabControl())
	{
		return false;
	}

	const bool hasSortPrevBinding = get_action_dik(kINV_SORT_PREV, 0) != 0 || get_action_dik(kINV_SORT_PREV, 1) != 0;
	const bool hasSortNextBinding = get_action_dik(kINV_SORT_NEXT, 0) != 0 || get_action_dik(kINV_SORT_NEXT, 1) != 0;

	const bool isSortPrevPressed = is_binded(kINV_SORT_PREV, dik) || (!hasSortPrevBinding && dik == SDL_SCANCODE_Q);
	const bool isSortNextPressed = is_binded(kINV_SORT_NEXT, dik) || (!hasSortNextBinding && dik == SDL_SCANCODE_E);
	return isSortPrevPressed || isSortNextPressed;
}

bool CUIActorMenuBase::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	// PropertyBox processes input here for example
	if (inherited::OnGamepadKeyAction(id, gamepad_action))
		return true;

	if (WINDOW_KEY_PRESSED == gamepad_action)
	{
		if (m_ui_navigation_selection)
		{
			if (is_binded(kUI_BACK, id) || is_binded(kQUIT, id))
			{
				if (m_bShowInfoWnds)
				{
					m_bShowInfoWnds = false;
					if (AnyInfoWindowOpen())
						return true;
					// Else let others process it
				}

				if (m_AuxMode != eAuxMode_None)
				{
					SetAuxMode(eAuxMode_None);
					return true;
				}
			}

			if (m_AuxMode == eAuxMode_None)
			{
				// Move UI primary(group) selector
				if (is_binded(kUI_SECONDARY_LEFT, id))
				{
					if (!any_binded_key_for_action_pressed_c(kUI_SECONDARY_RIGHT))
						MoveAreaSelector(eUIDirection4_Left);
					ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_LEFT);
					return true;
				}
				else if (is_binded(kUI_SECONDARY_RIGHT, id))
				{
					if (!any_binded_key_for_action_pressed_c(kUI_SECONDARY_LEFT))
						MoveAreaSelector(eUIDirection4_Right);
					ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_RIGHT);
					return true;
				}
				else if (is_binded(kUI_SECONDARY_UP, id))
				{
					if (!any_binded_key_for_action_pressed_c(kUI_SECONDARY_DOWN))
						MoveAreaSelector(eUIDirection4_Up);
					ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_UP);
					return true;
				}
				else if (is_binded(kUI_SECONDARY_DOWN, id))
				{
					if (!any_binded_key_for_action_pressed_c(kUI_SECONDARY_UP))
						MoveAreaSelector(eUIDirection4_Down);
					ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_DOWN);
					return true;
				}
			}

			if (is_binded(kUI_HINT, id))
			{
				VERIFY(m_ItemInfo);
				m_bShowInfoWnds = !m_bShowInfoWnds;
				return true;
			}
			else if (is_binded(kUI_ACCEPT, id))
			{
				if (m_AuxMode == eAuxMode_Upgrade && m_pUpgradeWnd->CanApplySelectedUpgrade())
				{
					m_bShowInfoWnds = false;
					m_pUpgradeWnd->ApplySelectedUpgrade();
				}
				else if (m_AuxMode == eAuxMode_None)
				{
					if (m_pCurrentCellItem)
					{
						OnItemDbClick(m_pCurrentCellItem);
					}
				}
				return true;
			}
			else if (is_binded(kUI_LEFT, id))
			{
				ActionRepeaters()->SetActionStarted(this, kUI_LEFT);
				if (!any_binded_key_for_action_pressed_c(kUI_RIGHT))
					MoveSelector(eUIDirection4_Left, true);
				return true;
			}
			else if (is_binded(kUI_RIGHT, id))
			{
				ActionRepeaters()->SetActionStarted(this, kUI_RIGHT);
				if (!any_binded_key_for_action_pressed_c(kUI_LEFT))
					MoveSelector(eUIDirection4_Right, true);
				return true;
			}
			else if (is_binded(kUI_UP, id))
			{
				ActionRepeaters()->SetActionStarted(this, kUI_UP);
				if (!any_binded_key_for_action_pressed_c(kUI_DOWN))
					MoveSelector(eUIDirection4_Up, true);
				return true;
			}
			else if (is_binded(kUI_DOWN, id))
			{
				ActionRepeaters()->SetActionStarted(this, kUI_DOWN);
				if (!any_binded_key_for_action_pressed_c(kUI_UP))
					MoveSelector(eUIDirection4_Down, true);
				return true;
			}


			CUIDragDropListEx* pDragDropList = nullptr;
			pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
			if (pDragDropList && m_AuxMode == eAuxMode_None)
			{
				if (is_binded(kUI_ACTION_1, id))
				{
					CUICellItem* pItem = CurrentItem();
					if (pItem)
					{
						m_bShowInfoWnds = false;
						InfoCurItem(nullptr);
						ActivatePropertiesBox();
					}
					return true;
				}
			}
		}

		if (is_binded(kACTORMENU_ACTION, id))
		{
			OnPressUserKey();
			return true;
		}
		else if (is_binded(kUI_BACK, id) || is_binded(kQUIT, id))
		{
			g_btnHint->Discard();
			HideDialog();

			if (GetInventoryOwner()->IsTalking())
				CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
			return true;
		}
		else if (is_binded(kUI_TAB_SECONDARY_LEFT, id))
		{
			ActionRepeaters()->SetActionStarted(this, kUI_TAB_SECONDARY_LEFT);
			if (!any_binded_key_for_action_pressed_c(kUI_TAB_SECONDARY_RIGHT))
			{
				m_ItemInfo->ScrollUp();
			}
			return true;
		}
		else if (is_binded(kUI_TAB_SECONDARY_RIGHT, id))
		{
			ActionRepeaters()->SetActionStarted(this, kUI_TAB_SECONDARY_RIGHT);
			if (!any_binded_key_for_action_pressed_c(kUI_TAB_SECONDARY_LEFT))
			{
				m_ItemInfo->ScrollDown();
			}
			return true;
		}
	}
	return false;
}

bool CUIActorMenuBase::OnGamepadKeyHold(int id)
{
	if (inherited::OnGamepadKeyHold(id))
		return true;

	if (m_AuxMode == eAuxMode_None)
	{
		// Move UI primary(group) selector
		switch (get_binded_action(id, agUIGeneral))
		{
			case kUI_SECONDARY_LEFT:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_LEFT) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_RIGHT))
					MoveAreaSelector(eUIDirection4_Left);
				return true;
			}
			case kUI_SECONDARY_RIGHT:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_RIGHT) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_LEFT))
					MoveAreaSelector(eUIDirection4_Right);
				return true;
			}
			case kUI_SECONDARY_UP:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_UP) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_DOWN))
					MoveAreaSelector(eUIDirection4_Up);
				return true;
			}
			case kUI_SECONDARY_DOWN:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_DOWN) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_UP))
					MoveAreaSelector(eUIDirection4_Down);
				return true;
			}
		}
	}

	if (m_ui_navigation_selection)
	{
		switch (get_binded_action(id, agUIGeneral))
		{
			case kUI_LEFT:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_LEFT) && !any_binded_key_for_action_pressed_c(kUI_RIGHT))
					MoveSelector(eUIDirection4_Left, false);
				return true;
			}
			case kUI_RIGHT:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_RIGHT) && !any_binded_key_for_action_pressed_c(kUI_LEFT))
					MoveSelector(eUIDirection4_Right, false);
				return true;
			}
			case kUI_UP:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_UP) && !any_binded_key_for_action_pressed_c(kUI_DOWN))
					MoveSelector(eUIDirection4_Up, false);
				return true;
			}
			case kUI_DOWN:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_DOWN) && !any_binded_key_for_action_pressed_c(kUI_UP))
					MoveSelector(eUIDirection4_Down, false);
				return true;
			}
			case kUI_TAB_SECONDARY_LEFT:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_TAB_SECONDARY_LEFT) && !any_binded_key_for_action_pressed_c(kUI_TAB_SECONDARY_RIGHT))
				{
					m_ItemInfo->ScrollUp();
				}
				return true;
			}
			case kUI_TAB_SECONDARY_RIGHT:
			{
				if (ActionRepeaters()->CanRepeatActionNow(this, kUI_TAB_SECONDARY_RIGHT) && !any_binded_key_for_action_pressed_c(kUI_TAB_SECONDARY_LEFT))
				{
					m_ItemInfo->ScrollDown();
				}
				return true;
			}
		}
	}

	return false;
}

void CUIActorMenuBase::MoveSelector(eUIDirection4 dir, bool bAllowAreaExit)
{
	if (m_AuxMode == eAuxMode_None)
	{
		CUIDragDropListEx* pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
		if (pDragDropList)
		{
			if (GetListType(pDragDropList) == iActorSlot)
			{
				if (MoveAreaSelector(dir))
				{
//					PlaySnd(eItemSwitch);
				}
			}
			else
			{
				if (pDragDropList->HasCells() && pDragDropList->MoveSelector(dir))
				{
//					PlaySnd(eItemSwitch);
				}
				else if (bAllowAreaExit)
				{
					if (MoveAreaSelector(dir))
					{
//						PlaySnd(eItemSwitch);
					}
				}
			}
		}
	}
	else if (m_AuxMode == eAuxMode_Upgrade)
	{
		if (m_pUpgradeWnd->SelectorMove(dir))
		{
//			PlaySnd(eItemSwitch);
		}
	}

	CheckSelectors();
}

void CUIActorMenuBase::OnPressUserKey()
{
	switch ( m_currMenuMode )
	{
	case mmUndefined:		break;
	case mmInventory:		break;
	case mmTrade:
		OnBtnPerformTrade(this, nullptr);
		break;
	case mmUpgrade:		
		if (pInput->GetControllerMode())
		{
			TryRepairItem(this, nullptr);
		}
		else
			TrySetCurUpgrade();
		break;
	case mmDeadBodySearch:
	{
		CUIDragDropListEx* pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
		if (pDragDropList && pDragDropList == GetActorList() && pInput->GetControllerMode())
		{
			PutAllToPartner(this, nullptr);
		}
		else
		{
			TakeAllFromPartner(this, nullptr);
		}
		if (pInput->GetControllerMode())
			InfoCurItem(nullptr);
		break;
	}
	default:
		R_ASSERT(0);
		break;
	}
}

void CUIActorMenuBase::OnBtnPerformTrade(CUIWindow* w, void* d)
{
	if (GetTradeActorList()->ItemsCount() == 0 && GetTradePartnerList()->ItemsCount() == 0)
	{
		return;
	}

	int actor_money = (int)GetInventoryOwner()->get_money();
	int partner_money = (int)GetPartner()->get_money();
	int actor_price = (int)CalcItemsPrice(GetTradeActorList(), GetPartnerTrade(), true);
	int partner_price = (int)CalcItemsPrice(GetTradePartnerList(), GetPartnerTrade(), false);

	int delta_price = actor_price - partner_price;
	actor_money += delta_price;
	partner_money -= delta_price;

	if ((actor_money >= 0) && (partner_money >= 0) && (actor_price >= 0 || partner_price > 0))
	{
		GetPartnerTrade()->OnPerformTrade(partner_price, actor_price);

		TransferItems(GetTradeActorList(), GetTradePartnerBagList(), GetPartnerTrade(), true);
		TransferItems(GetTradePartnerList(), GetTradeActorBagList(), GetPartnerTrade(), false);

		if (pInput->GetControllerMode())
			SetCurrentItem(nullptr);
	}
	else
	{
		TradeShowMessage(actor_money, partner_money);
	}
	if (!pInput->GetControllerMode())
		SetCurrentItem(nullptr);

	UpdateItemsPlace();
}

bool CUIActorMenuBase::StopAnyMove()  // true = актёр не идёт при открытом меню
{
	switch ( m_currMenuMode )
	{
	case mmInventory:
		return pInput->GetControllerMode();
	case mmUndefined:
	case mmTrade:
	case mmUpgrade:
	case mmDeadBodySearch:
		return true;
	}
	return true;
}

void CUIActorMenuBase::ReloadGamepadLegend() 
{
	if (m_gamepad_legend)
	{
		m_gamepad_legend->ReloadLegend();
	}
	if (m_pItemDropAmountWnd)
	{
		if (CUIGamepadLegend* l = m_pItemDropAmountWnd->_gamepadLegend)
		{
			l->ReloadLegend();
		}
	}
}

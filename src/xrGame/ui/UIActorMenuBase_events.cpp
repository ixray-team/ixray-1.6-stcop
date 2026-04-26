#include "stdafx.h"
#include "UIActorMenuBase.h"
#include "../InventoryOwner.h"
#include "../Level.h"
#include "UIDragDropReferenceList.h"
#include "UICellCustomItems.h"
#include "UICellItemFactory.h"
#include "../WeaponMagazined.h"
#include "../trade.h"
#include "../Inventory.h"
#include "../InventoryWeaponSlotLayout.h"
#include "../eatable_item_object.h"
#include "../../xrUI/UICursor.h"

void move_item_from_to(u16 from_id, u16 to_id, u16 what_id);

bool move_item_check( PIItem itm, CInventoryOwner* from, CInventoryOwner* to, bool weight_check )
{
	if ( weight_check )
	{
		float invWeight		= to->inventory().CalcTotalWeight();
		float maxWeight		= to->MaxCarryWeight();
		float itmWeight = itm->m_pInventory ? itm->m_pInventory->CalcItemWeight(itm) : itm->Weight();
		if ( invWeight + itmWeight >= maxWeight )
		{
			return false;
		}
	}
	move_item_from_to( from->object_id(), to->object_id(), itm->object_id() );
	return true;
}

bool FindItemInList(CUIDragDropListEx* lst, PIItem pItem, CUICellItem*& ci_res)
{
	u32 count = lst->ItemsCount();
	for (u32 i=0; i<count; ++i)
	{
		CUICellItem* ci				= lst->GetItemIdx(i);
		for(u32 j=0; j<ci->ChildsCount(); ++j)
		{
			CUIInventoryCellItem* ici = smart_cast<CUIInventoryCellItem*>(ci->Child(j));
			if(ici->object()==pItem)
			{
				ci_res = ici;
				return true;
			}
		}

		CUIInventoryCellItem* ici = smart_cast<CUIInventoryCellItem*>(ci);
		if(ici->object()==pItem)
		{
			ci_res = ci;
			return true;
		}
	}
	return false;
}

bool RemoveItemFromList(CUIDragDropListEx* lst, PIItem pItem)
{// fixme
	CUICellItem*	ci	= nullptr;
	if(FindItemInList(lst, pItem, ci))
	{
		R_ASSERT		(ci);

		CUICellItem* dying_cell = lst->RemoveItem	(ci, false);
		xr_delete(dying_cell);

		return			true;
	}else
		return			false;
}

void CUIActorMenuBase::SendEvent_ActivateSlot(u16 slot, u16 recipient)
{
	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ACTIVATE_SLOT, recipient);
	P.w_u16							(slot);
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();
}

void CUIActorMenuBase::SendEvent_Item2Slot(PIItem pItem, u16 recipient, u16 slot_id)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2SLOT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	P.w_u16							(slot_id);
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eItemToSlot);
};

void CUIActorMenuBase::SendEvent_Item2Belt(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2BELT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eItemToBelt);
};

void CUIActorMenuBase::SendEvent_Item2Ruck(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2RUCK, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eItemToRuck);
};

void CUIActorMenuBase::SendEvent_Item_Eat(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM_EAT, recipient);
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();
};

void CUIActorMenuBase::SendEvent_Item_Drop(PIItem pItem, u16 recipient)
{
	R_ASSERT(pItem->parent_id()==recipient);
	if (!IsGameTypeSingle())
		pItem->DenyTrade();
	//pItem->SetDropManual			(true);
	NET_Packet					P;
	pItem->object().u_EventGen	(P,GE_OWNERSHIP_REJECT,pItem->parent_id());
	P.w_u16						(pItem->object().ID());
	pItem->object().u_EventSend	(P);
	PlaySnd						(eDropItem);
	clear_highlight_lists			();
}

void CUIActorMenuBase::OnInventoryAction(PIItem pItem, u16 action_type)
{
	CUIDragDropListEx* all_lists[] =
	{
		GetBeltList(),
		GetActorList(),
		GetTradeActorBagList(),
		GetTradeActorList(),
		nullptr
	};

	switch (action_type)
	{
		case GE_TRADE_BUY :
		case GE_OWNERSHIP_TAKE :
			{
				bool b_already	= false;

				CUIDragDropListEx* lst_to_add		= nullptr;
				SInvItemPlace pl						= pItem->m_ItemCurrPlace;
				if ( pItem->BaseSlot() == GRENADE_SLOT )
				{
					pl.type		= eItemPlaceRuck;
					pl.slot_id	= GRENADE_SLOT;
				}

				if(pl.type==eItemPlaceSlot)
					lst_to_add						= GetSlotList(pl.slot_id);
				else if(pl.type==eItemPlaceBelt)
					lst_to_add						= GetListByType(iActorBelt);
				else
				{
					if(pItem->parent_id()==GetInventoryOwner()->object_id())
						lst_to_add						= GetListByType(iActorBag);
					else
						lst_to_add						= GetListByType(iDeadBodyBag);
				}


				for (int i = 0; i < 4; i++)
				{
					CUIDragDropListEx*	curr = all_lists[i];
					if (!curr)
						continue;

					CUICellItem*		ci   = nullptr;

					if ( FindItemInList(curr, pItem, ci) )
					{
						if ( lst_to_add != curr )
						{
							RemoveItemFromList(curr, pItem);
						}
						else
						{
							b_already = true;
						}
						//break;
					}
				}
				
				for (u8 i = 1; i <= LAST_SLOT; ++i)
				{
					CUIDragDropListEx* curr = m_pInvList[i];
					if (curr)
					{
						CUICellItem* ci = nullptr;
						if (FindItemInList(curr, pItem, ci))
						{
							if (lst_to_add != curr)
								RemoveItemFromList(curr, pItem);
							else
								b_already = true;
						}
					}
				}

				CUICellItem* ci = nullptr;
				if (GetMenuMode() == mmDeadBodySearch && FindItemInList(GetPartnerList(), pItem, ci))
					RemoveItemFromList(GetPartnerList(), pItem);

				if ( !b_already )
				{
					if ( lst_to_add )
					{
						CUICellItem* itm	= create_cell_item(pItem);
						lst_to_add->SetItem	(itm);
					}
				}
				if(GetInventoryOwner() && m_pQuickSlot)
					m_pQuickSlot->ReloadReferences(GetInventoryOwner());
			}break;
		case GE_TRADE_SELL :
		case GE_OWNERSHIP_REJECT :
			{
				if(CUIDragDropListEx::m_drag_item)
				{
					CUIInventoryCellItem* ici = smart_cast<CUIInventoryCellItem*>(CUIDragDropListEx::m_drag_item->ParentItem());
					R_ASSERT(ici);
					if(ici->object()==pItem)
					{
						CUIDragDropListEx*	_drag_owner		= ici->OwnerList();
						_drag_owner->DestroyDragItem		();
					}
				}

				if (GetMenuMode() == mmDeadBodySearch && RemoveItemFromList(GetPartnerList(), pItem))
					;
				else
				{
					for (int i = 0; i < 4; i++)
					{
						CUIDragDropListEx* curr = all_lists[i];
						if (!curr)
							continue;

						if (RemoveItemFromList(curr, pItem))
							break;
					}
					if (!IsGameTypeSingle() && pItem->parent_id() != GetInventoryOwner()->object_id())
						RemoveItemFromList(GetListByType(iDeadBodyBag), pItem);
				}
				
				for (u8 i = 1; i <= LAST_SLOT; ++i)
				{
					CUIDragDropListEx* curr = m_pInvList[i];
					if (curr)
					{
						if (RemoveItemFromList(curr, pItem))
							break;
					}
				}


				if(GetInventoryOwner() && m_pQuickSlot)
					m_pQuickSlot->ReloadReferences(GetInventoryOwner());
			}break;
	}
	UpdateItemsPlace();
	UpdateConditionProgressBars();
}

void CUIActorMenuBase::UnloadWeapon(CWeaponMagazined* pWpn)
{
	if (!pWpn) return;

	if (IsGameTypeSingle())
	{
		pWpn->UnloadMagazine();
	}
	else
	{
		NET_Packet	P;
		CGameObject::u_EventGen(P, GE_WPN_UNLOAD_AMMO, pWpn->ID());
		P.w_u8(0);
		CGameObject::u_EventSend(P);
	}
}

void CUIActorMenuBase::TransferItemsMp(CUIDragDropListEx* pSellList, CUIDragDropListEx* pBuyList, CTrade* pTrade, bool bBuying)
{
	if (pSellList->ItemsCount() == 0)
		return;

	CGameObject* pPlayer = pTrade->pPartner.inv_owner ? pTrade->pPartner.inv_owner->cast_game_object() : nullptr;
	R_ASSERT(!!pPlayer->cast_actor());

	NET_Packet P;
	pPlayer->u_EventGen(P, GE_GAME_EVENT, pPlayer->ID());
	P.w_u16(GAME_EVENT_MP_TRADE);
	P.w_u8(bBuying);								// Set as buying
	P.w_u16(pTrade->pThis.inv_owner->object_id());	// NPC ID
	P.w_u16(pPlayer->ID());							// Actor ID

	u32 totalPrice = 0;
	if (bBuying)
	{
		// Sell to NPC
		xr_vector<PIItem> items_to_destroy;
		while (pSellList->ItemsCount())
		{
			CUICellItem* cell_item = pSellList->GetItemIdx(0);
			PIItem item = (PIItem)cell_item->m_pData;
			items_to_destroy.push_back(item);
			totalPrice += pTrade->GetItemPrice(item, bBuying);
			cell_item = pSellList->RemoveItem(cell_item, false);
			delete_data(cell_item);
			cell_item = nullptr;
		}

		// Check to max for signed value
		R_ASSERT(totalPrice < INT32_MAX);

		P.w_s32(static_cast<s32>(totalPrice));	// Total price
		P.w_u32(items_to_destroy.size());		// Items count

		for (PIItem Itm : items_to_destroy)
		{
			P.w_u16(Itm->object_id());		// Item ID
			P.w_float(Itm->GetCondition());	// Item condition (for correct price calculation)
		}
	}
	else
	{
		// Buy from NPC
		xr_map<u16, u16> sellMap;
		while (pSellList->ItemsCount())
		{
			CUICellItem* cell_item = pSellList->GetItemIdx(0);
			PIItem item = (PIItem)cell_item->m_pData;
			sellMap[item->object_id()] += 1;
			totalPrice += pTrade->GetItemPrice(item, bBuying);
			cell_item = pSellList->RemoveItem(cell_item, false);
			delete_data(cell_item);
			cell_item = nullptr;
		}
		// Check to max for signed value
		R_ASSERT(totalPrice < INT32_MAX);
		P.w_s32(static_cast<s32>(totalPrice)); // Total price
		P.w_u32(sellMap.size());				// Map Size

		for (auto&[ID, Count] : sellMap)
		{
			P.w_u16(ID);  // Item ID
			P.w_u16(Count); // Count
		}
	}

	pPlayer->u_EventSend(P);
}

void CUIActorMenuBase::AttachAddon(PIItem item_to_upgrade)
{
	PlaySnd										(eAttachAddon);
	R_ASSERT									(item_to_upgrade);
	if (OnClient())
	{
		NET_Packet								P;
		CGameObject::u_EventGen					(P, GE_ADDON_ATTACH, item_to_upgrade->object().ID());
		P.w_u16									(CurrentIItem()->object().ID());
		CGameObject::u_EventSend				(P);
	};

	item_to_upgrade->Attach						(CurrentIItem(), true);

	SetCurrentItem								(nullptr);
}

void CUIActorMenuBase::DetachAddon(const char* addon_name, PIItem itm)
{
	PlaySnd										(eDetachAddon);
	if (OnClient())
	{
		NET_Packet								P;
		if(itm==nullptr)
			CGameObject::u_EventGen				(P, GE_ADDON_DETACH, CurrentIItem()->object().ID());
		else
			CGameObject::u_EventGen				(P, GE_ADDON_DETACH, itm->object().ID());

		P.w_stringZ								(addon_name);
		CGameObject::u_EventSend				(P);
		return;
	}
	if(itm==nullptr)
		CurrentIItem()->Detach					(addon_name, true);
	else
		itm->Detach								(addon_name, true);
}

bool CUIActorMenuBase::TryActiveSlot(CUICellItem* itm)
{
	PIItem	iitem	= (PIItem)itm->m_pData;
	u16 slot		= iitem->BaseSlot();

	if ( slot == GRENADE_SLOT )
	{
		PIItem	prev_iitem = GetInventoryOwner()->inventory().ItemFromSlot(slot);
		if ( prev_iitem && (prev_iitem->object().cNameSect() != iitem->object().cNameSect()) )
		{
			SendEvent_Item2Ruck( prev_iitem, GetInventoryOwner()->object_id());
			SendEvent_Item2Slot( iitem, GetInventoryOwner()->object_id(), slot);
		}
		SendEvent_ActivateSlot( slot, GetInventoryOwner()->object_id());
		return true;
	}
	return false;
}

bool CUIActorMenuBase::TryHolsterPistolBagDbClick(CUICellItem* itm)
{
	if (m_currMenuMode != mmInventory)
	{
		return false;
	}
	if (!InventoryHolsterPistolSlotActiveInSettings())
	{
		return false;
	}
	if (m_pInvList[PISTOL_SLOT_NEW] == nullptr)
	{
		return false;
	}

	PIItem	iitem	= (PIItem)itm->m_pData;
	if (iitem == nullptr || iitem->CurrPlace() != eItemPlaceRuck)
	{
		return false;
	}
	if (!InventoryHolsterExclusivePistolFootprint(iitem))
	{
		return false;
	}

	if (ToSlot(itm, false, PISTOL_SLOT_NEW))
	{
		return true;
	}
	return ToSlot(itm, true, PISTOL_SLOT_NEW);
}

bool CUIActorMenuBase::TryHolsterPistolHolsterSlotDbClick(CUICellItem* itm)
{
	if (m_currMenuMode != mmInventory)
	{
		return false;
	}
	if (!InventoryHolsterPistolSlotActiveInSettings())
	{
		return false;
	}
	if (m_pInvList[PISTOL_SLOT_NEW] == nullptr)
	{
		return false;
	}
	if (itm->OwnerList() != m_pInvList[PISTOL_SLOT_NEW])
	{
		return false;
	}

	PIItem	current	= (PIItem)itm->m_pData;
	if (!InventoryHolsterExclusivePistolFootprint(current))
	{
		return false;
	}

	CUIDragDropListEx*	bagList	= GetActorList();
	if (bagList == nullptr)
	{
		return false;
	}

	const u32	count	= bagList->ItemsCount();
	for (u32 i = 0; i < count; ++i)
	{
		CUICellItem*	cell		= bagList->GetItemIdx(i);
		PIItem		candidate	= (PIItem)cell->m_pData;
		if (candidate == nullptr || candidate == current)
		{
			continue;
		}
		if (candidate->CurrPlace() != eItemPlaceRuck)
		{
			continue;
		}
		if (!InventoryHolsterExclusivePistolFootprint(candidate))
		{
			continue;
		}
		if (candidate->object().cNameSect() != current->object().cNameSect())
		{
			continue;
		}
		if (ToSlot(cell, true, PISTOL_SLOT_NEW))
		{
			return true;
		}
	}
	for (u32 i = 0; i < count; ++i)
	{
		CUICellItem*	cell		= bagList->GetItemIdx(i);
		PIItem		candidate	= (PIItem)cell->m_pData;
		if (candidate == nullptr || candidate == current)
		{
			continue;
		}
		if (candidate->CurrPlace() != eItemPlaceRuck)
		{
			continue;
		}
		if (!InventoryHolsterExclusivePistolFootprint(candidate))
		{
			continue;
		}
		if (ToSlot(cell, true, PISTOL_SLOT_NEW))
		{
			return true;
		}
	}
	return false;
}

void CUIActorMenuBase::PutAllToPartner(CUIWindow* w, void* d) 
{
	u32 Iter = 0;
	while (Iter < GetActorList()->ItemsCount())
	{
		CUICellItem* ci = GetActorList()->GetItemIdx(Iter);
		if (!ToDeadBodyBag(ci, false))
		{
			++Iter;
		}
	}
}

void CUIActorMenuBase::TakeAllFromPartner(CUIWindow* w, void* d)
{
	VERIFY( GetInventoryOwner() );
	if ( !GetPartner() )
	{
		if ( GetInvBox() )
		{
			TakeAllFromInventoryBox();
		}
		return;
	}

	u32 const cnt = GetPartnerList()->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = GetPartnerList()->GetItemIdx(i);
		for ( u32 j = 0; j < ci->ChildsCount(); ++j )
		{
			PIItem j_item = (PIItem)(ci->Child(j)->m_pData);
			move_item_check( j_item, GetPartner(), GetInventoryOwner(), false);
		}
		PIItem item = (PIItem)(ci->m_pData);
		move_item_check( item, GetPartner(), GetInventoryOwner(), false);
	}

	GetPartnerList()->ClearAll(true); // false
}

void CUIActorMenuBase::TakeAllFromInventoryBox()
{
	u16 actor_id = GetInventoryOwner()->object_id();
	xr_vector<u16> IgnoredItemsIds = {};

	u32 const cnt = GetPartnerList()->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = GetPartnerList()->GetItemIdx(i);
		PIItem item = (PIItem)(ci->m_pData);

		// FFx0001
		if (!IsAllowTakeFromInvBox(ci)) 
		{ 
			IgnoredItemsIds.push_back(item->object_id());

			continue;
		}

		for ( u32 j = 0; j < ci->ChildsCount(); ++j )
		{
			PIItem j_item = (PIItem)(ci->Child(j)->m_pData);
			move_item_from_to( GetInvBox()->ID(), actor_id, j_item->object_id());
		}

		
		move_item_from_to( GetInvBox()->ID(), actor_id, item->object_id());
	}

	GetPartnerList()->ClearAll(true, IgnoredItemsIds); // FFx0001
	IgnoredItemsIds.clear();
}

bool CUIActorMenuBase::TryUseItem( CUICellItem* cell_itm )
{
	if ( !cell_itm )
	{
		return false;
	}
	PIItem item	= (PIItem)cell_itm->m_pData;

	CBottleItem*	pBottleItem		= smart_cast<CBottleItem*>	(item);
	CMedkit*		pMedkit			= smart_cast<CMedkit*>		(item);
	CAntirad*		pAntirad		= smart_cast<CAntirad*>		(item);
	CEatableItem*	pEatableItem	= smart_cast<CEatableItem*>	(item);

	if ( !(pMedkit || pAntirad || pEatableItem || pBottleItem) )
	{
		return false;
	}
	if ( !item->Useful() )
	{
		return false;
	}

	u16 recipient = GetInventoryOwner()->object_id();
	if ( item->parent_id() != recipient )
	{
		cell_itm->OwnerList()->RemoveItem( cell_itm, false );
	}

	SendEvent_Item_Eat		( item, recipient );
	PlaySnd					( eItemUse );
	return true;
}

//Alundaio: Donate current item while in trade menu
void CUIActorMenuBase::DonateCurrentItem(CUICellItem* cell_item)
{
	if (!GetPartnerTrade() || !GetTradePartnerList())
		return;

	CUIDragDropListEx* invlist = GetListByType(iActorBag);
	if (!invlist->IsOwner(cell_item))
		return;

	PIItem item = (PIItem)cell_item->m_pData;
	if (!item)
		return;

	//Alundaio: 
	luabind::functor<bool> funct;

	R_ASSERT2(
		ai().script_engine().functor(m_onDonateCurrentItem, funct),
		make_string<const char*>("Failed to get functor <onDonateCurrentItem>, item = %s", item->m_section_id.c_str())
	);

	funct(GetPartner()->cast_game_object()->lua_game_object(), item->object().lua_game_object());
	//-Alundaio

	CUICellItem* itm = invlist->RemoveItem(cell_item, false);

	GetPartnerTrade()->TransferItem(item, true, true);

	GetTradePartnerList()->SetItem(itm);

	SetCurrentItem(nullptr);
	UpdateItemsPlace();
}
//-Alundaio

bool CUIActorMenuBase::ToSlot(CUICellItem* itm, bool force_place, u16 slot_id)
{
	CUIDragDropListEx*	old_owner			= itm->OwnerList();
	PIItem	iitem							= (PIItem)itm->m_pData;

	bool b_own_item							= (iitem->parent_id()==GetInventoryOwner()->object_id());
	if (slot_id==HELMET_SLOT)
	{
		CCustomOutfit* pOutfit = GetInventoryOwner()->GetOutfit();
		if(pOutfit && !pOutfit->bIsHelmetAvaliable)
			return false;
	}

	if (GetInventoryOwner()->inventory().CanPutInSlot(iitem, slot_id))
	{
		CUIDragDropListEx* new_owner = GetSlotList(slot_id);

		if ( slot_id == GRENADE_SLOT || !new_owner )
		{
			return true; //fake, sorry (((
		}
		if(slot_id==OUTFIT_SLOT)
		{
			CCustomOutfit* pOutfit = iitem->cast_outfit();
			if (pOutfit && !pOutfit->bIsHelmetAvaliable)
			{
				CUIDragDropListEx* helmet_list = GetSlotList(HELMET_SLOT);
				if (helmet_list->ItemsCount() == 1)
				{
					CUICellItem* helmet_cell = helmet_list->GetItemIdx(0);
					ToBag(helmet_cell, false);
				}
			}
		}


		bool result							= (!b_own_item) || GetInventoryOwner()->inventory().Slot(slot_id, iitem);
		VERIFY								(result);

		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );

		new_owner->SetItem					(i);

		SendEvent_Item2Slot					(iitem, GetInventoryOwner()->object_id(), slot_id);

		SendEvent_ActivateSlot				(slot_id, GetInventoryOwner()->object_id());

		if ( slot_id == OUTFIT_SLOT && ShouldPutArtefactsToBag() )
		{
			MoveArtefactsToBag();
		}
		return								true;
	}
	else
	{ // in case slot is busy
		if ( !force_place || slot_id == NO_ACTIVE_SLOT ) 
			return false;

		// Same idea as DEVICE_SLOT: allow double-click swap out of a busy slot even when marked persistent.
		const bool allowPersistentForceSwap =
			(slot_id == DEVICE_SLOT) ||
			(slot_id == PISTOL_SLOT_NEW && InventoryHolsterPistolSlotActiveInSettings());
		if (GetInventoryOwner()->inventory().SlotIsPersistent(slot_id) && !allowPersistentForceSwap)
			return false;

		if ( slot_id == INV_SLOT_2 && GetInventoryOwner()->inventory().CanPutInSlot(iitem, INV_SLOT_3) && !InventorySecondarySlotPairingStrict())
			return ToSlot(itm, force_place, INV_SLOT_3);

		if ( slot_id == INV_SLOT_3 && GetInventoryOwner()->inventory().CanPutInSlot(iitem, INV_SLOT_2) && !InventorySecondarySlotPairingStrict())
			return ToSlot(itm, force_place, INV_SLOT_2);

		PIItem	_iitem						= GetInventoryOwner()->inventory().ItemFromSlot(slot_id);
		CUIDragDropListEx* slot_list		= GetSlotList(slot_id);
		if (slot_list == nullptr || _iitem == nullptr)
		{
			return false;
		}

		CUICellItem* slot_cell = nullptr;
		for (u32 slot_cell_idx = 0; slot_cell_idx < slot_list->ItemsCount(); ++slot_cell_idx)
		{
			CUICellItem* const candidate = slot_list->GetItemIdx(slot_cell_idx);
			if (candidate != nullptr && (PIItem)candidate->m_pData == _iitem)
			{
				slot_cell = candidate;
				break;
			}
		}

		if (slot_cell == nullptr)
		{
			// Occupant in inventory but no matching UI cell (out of sync or list fallback). Same outcome as ToBag, without a cell pointer.
			bool const occupant_is_own = (_iitem->parent_id() == GetInventoryOwner()->object_id());
			bool const cleared = (!occupant_is_own) || GetInventoryOwner()->inventory().Ruck(_iitem);
			if (!cleared)
			{
				return false;
			}
			if (occupant_is_own)
			{
				SendEvent_Item2Ruck(_iitem, GetInventoryOwner()->object_id());
			}
			UpdateItemsPlace();
			return ToSlot(itm, false, slot_id);
		}

		bool result							= ToBag(slot_cell, false);
		VERIFY								(result);

		result								= ToSlot(itm, false, slot_id);

		if (b_own_item && result && slot_id == DEVICE_SLOT)
		{
			CCustomDevice* dev = iitem->cast_custom_device();
			dev->switch_device();
		}

		return result;
	}
}

bool CUIActorMenuBase::ToBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	// FFx0001
	if (!IsAllowTakeFromInvBox(itm)) 
	{
		return false;
	}

	PIItem	iitem						= (PIItem)itm->m_pData;

	bool b_own_item						= (iitem->parent_id()==GetInventoryOwner()->object_id());

	bool b_already						= GetInventoryOwner()->inventory().InRuck(iitem);

	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= nullptr;
	if(b_use_cursor_pos)
	{
			new_owner					= CUIDragDropListEx::m_drag_item->BackList();
			VERIFY						(GetListType(new_owner)==iActorBag);
	}else
			new_owner					= GetListByType(iActorBag);

	if(GetInventoryOwner()->inventory().CanPutInRuck(iitem) || (b_already && (new_owner != old_owner)))
	{
		// Pavel: если предмет в iActorTrade, то он уже должен находиться в рюкзаке
		// Проверка нужна для того, чтобы не сбрасывалась граната в МП,
		// при перекладывании из iActorTrade
		if (GetListType(old_owner) != iActorTrade)
		{
			bool result = b_already || (!b_own_item || GetInventoryOwner()->inventory().Ruck(iitem));
			R_ASSERT(result);
		}

		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		if(!i)
			return false;

		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);

		if(!b_already || !b_own_item)
			SendEvent_Item2Ruck					(iitem, GetInventoryOwner()->object_id());

		if ( m_currMenuMode == mmTrade && GetPartner() )
		{
			ColorizeItem( itm, !CanMoveToPartner( iitem ) );
		}
		return true;
	}
	return false;
}

// FFx0001 
bool CUIActorMenuBase::IsAllowTakeFromInvBox(CUICellItem* itm)
{
	if (!m_isInvBoxCanTakeItem) 
	{
		return true;
	}

	// inv box
	if (GetInvBox()) 
	{
		luabind::functor<bool> funct;
		R_ASSERT2(ai().script_engine().functor(m_onInvBoxCanTakeItem, funct), make_string<const char*>("failed to get %s functor", m_onInvBoxCanTakeItem));

		if (funct(GetInvBox()->cast_game_object()->lua_game_object(), ((PIItem)itm->m_pData)->cast_game_object()->lua_game_object()) == false)
		{
			return false;
		}
	}
	else 
	{
		// npc
	}

	return true;
}

bool CUIActorMenuBase::ToBelt(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem	iitem						= (PIItem)itm->m_pData;
	bool b_own_item						= (iitem->parent_id()==GetInventoryOwner()->object_id());

	if(GetInventoryOwner()->inventory().CanPutInBelt(iitem))
	{
		CUIDragDropListEx*	old_owner		= itm->OwnerList();
		CUIDragDropListEx*	new_owner		= nullptr;
		if(b_use_cursor_pos){
				new_owner					= CUIDragDropListEx::m_drag_item->BackList();
				VERIFY						(new_owner==GetBeltList());
		}else
				new_owner					= GetBeltList();

		bool result							= (!b_own_item) || GetInventoryOwner()->inventory().Belt(iitem);
		VERIFY								(result);
		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );

		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);

		if(!b_own_item)
			SendEvent_Item2Belt				(iitem, GetInventoryOwner()->object_id());

		return								true;
	}
	else
	{ // in case belt slot is busy
		if(!iitem->Belt() || GetInventoryOwner()->inventory().BeltWidth() == 0)
			return false;

		CUIDragDropListEx* belt_list		= nullptr;
		if(b_use_cursor_pos)
			belt_list						= CUIDragDropListEx::m_drag_item->BackList();
		else
			return false;

		Ivector2 belt_cell_pos				= belt_list->PickCell(GetUICursor().GetCursorPosition());
		if(belt_cell_pos.x==-1 && belt_cell_pos.y==-1)
			return false;

		CUICellItem* slot_cell				= belt_list->GetCellAt(belt_cell_pos).m_item;
		if (!slot_cell)
			return false;

		bool result							= ToBag(slot_cell, false);
		VERIFY								(result);

		result								= ToBelt(itm, b_use_cursor_pos);
		return result;
	}
}

void CUIActorMenuBase::MoveArtefactsToBag()
{
	while ( GetBeltList()->ItemsCount())
	{
		CUICellItem* ci = GetBeltList()->GetItemIdx(0);
		VERIFY( ci && ci->m_pData );
		ToBag( ci, false );
	}//for i
	GetBeltList()->ClearAll(true);
}

void CUIActorMenuBase::ToBagAll(u32 item_amount)
{
	CUICellItem* itm = CurrentItem();
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toMove = (item_amount > totalCount) ? totalCount : item_amount;
	// Move children first: min(toMove, childCount)
	u32 const childrenToMove = (toMove < childCount) ? toMove : childCount;
	for (int i = 0; i < childrenToMove; ++i)
	{
		CUICellItem* childItem = itm->Child(0);
		if (!ToBag(childItem, false))
			return;
	}

	// Move parent only when moving entire stack (toMove > childCount)
	if (toMove > childCount)
		ToBag(itm, false);
}

void CUIActorMenuBase::DropAllCurrentItem(u32 item_amount)
{
	if ( CurrentIItem() && !CurrentIItem()->IsQuestItem() )
	{
		u32 childCount = CurrentItem()->ChildsCount();
		u32 toPop = (item_amount < childCount) ? item_amount : childCount;
		for ( u32 i = 0; i < toPop; ++i )
		{
			CUICellItem*	itm  = CurrentItem()->PopChild(nullptr);
			PIItem			iitm = (PIItem)itm->m_pData;
			SendEvent_Item_Drop( iitm, GetInventoryOwner()->object_id());
		}

		if ( item_amount > childCount )
			SendEvent_Item_Drop( CurrentIItem(), GetInventoryOwner()->object_id());
	}
	SetCurrentItem								(nullptr);
}

void CUIActorMenuBase::TakeAllCurrentItem(u32 item_amount)
{
	CUIDragDropListEx* deadBodyList = GetListByType(iDeadBodyBag);
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toTake = (item_amount > totalCount) ? totalCount : item_amount;
	u32 const childrenToTake = (toTake < childCount) ? toTake : childCount;

	for (u32 i = 0; i < childrenToTake; ++i)
	{
		CUICellItem* child_itm = CurrentItem()->PopChild(nullptr);
		PIItem child_iitm = (PIItem)child_itm->m_pData;
		move_item_from_to(child_iitm->parent_id(), GetInventoryOwner()->object_id(), child_iitm->object_id());
		GetActorList()->SetItem(child_itm);
	}

	if (toTake > childCount)
	{
		CUICellItem* parent_itm = CurrentItem();
		PIItem parent_iitm = CurrentIItem();
		move_item_from_to(parent_iitm->parent_id(), GetInventoryOwner()->object_id(), parent_iitm->object_id());
		parent_itm = deadBodyList->RemoveItem(parent_itm, true);
		if (parent_itm)
			GetActorList()->SetItem(parent_itm);
	}

	UpdateDeadBodyBag();
}

void CUIActorMenuBase::MoveAllCurrentItem(u32 item_amount)
{
	auto ownerID = GetPartner() ? GetPartner()->object_id() : GetInvBox()->ID();
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toMove = (item_amount > totalCount) ? totalCount : item_amount;
	// Move children first: min(toMove, childCount)
	u32 const childrenToMove = (toMove < childCount) ? toMove : childCount;
	for (u32 i = 0; i < childrenToMove; ++i)
	{
		CUICellItem* child_itm = CurrentItem()->Child(i);
		PIItem child_iitm = (PIItem)child_itm->m_pData;
		move_item_from_to(CurrentIItem()->parent_id(), ownerID, child_iitm->object_id());
	}
	// Move parent only when moving entire stack (toMove > childCount)
	if (toMove > childCount)
		move_item_from_to(CurrentIItem()->parent_id(), ownerID, CurrentIItem()->object_id());
}

// FFx0001
bool CUIActorMenuBase::IsAllowPlaceToInvBox(CUICellItem* itm)
{
	if (!m_isInvBoxCanPlaceItem) 
	{
		return true;
	}

	// inv box
	if (GetInvBox()) 
	{
		luabind::functor<bool> funct;
		R_ASSERT2(ai().script_engine().functor(m_onInvBoxCanPlaceItem, funct), make_string<const char*>("failed to get %s functor", m_onInvBoxCanPlaceItem));

		if (funct(GetInvBox()->cast_game_object()->lua_game_object(), ((PIItem)itm->m_pData)->cast_game_object()->lua_game_object()) == false)
		{
			return false;
		}
	}
	return true;
}

bool CUIActorMenuBase::ToActorTrade(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem	iitem						= (PIItem)itm->m_pData;
	if ( !CanMoveToPartner( iitem ) )
	{
		return false;
	}

//	if(m_pActorInvOwner->inventory().CanPutInRuck(iitem))
	{
		CUIDragDropListEx*	old_owner		= itm->OwnerList();
		CUIDragDropListEx*	new_owner		= nullptr;
		EDDListType			old_owner_type	= GetListType(old_owner);
		if(old_owner_type==iQuickSlot)
			return false;

		if(b_use_cursor_pos)
		{
			new_owner						= CUIDragDropListEx::m_drag_item->BackList();
			VERIFY							(new_owner==GetTradeActorList());
		}else
			new_owner						= GetTradeActorList();

		if (IsGameTypeSingle())
		{
			bool result = (old_owner_type != iActorBag) ? GetInventoryOwner()->inventory().Ruck(iitem) : true;
            VERIFY(result);
		}

		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		
		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);
		
		// Pavel: Если мы переносим предмет из слота в окно торговли
        // то необходимо переместить его по факту в рюкзак
		if ( old_owner_type != iActorBag )
		{
			SendEvent_Item2Ruck				(iitem, GetInventoryOwner()->object_id());
		}
		return true;
	}
}

void CUIActorMenuBase::ToActorTradeAll(u32 item_amount)
{
	CUICellItem* itm = CurrentItem();
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toMove = (item_amount > totalCount) ? totalCount : item_amount;
	// Move children first: min(toMove, childCount)
	u32 const childrenToMove = (toMove < childCount) ? toMove : childCount;
	for (int i = 0; i < childrenToMove; ++i)
	{
		if (!ToActorTrade(itm->Child(0), false))
			return;
	}

	// Move parent only when moving entire stack (toMove > childCount)
	if (toMove > childCount)
		ToActorTrade(itm, false);
}

bool CUIActorMenuBase::ToPartnerTrade(CUICellItem* itm, bool b_use_cursor_pos)
{
	//Перенос в список для покупки.
	PIItem iitem						= (PIItem)itm->m_pData;
	SInvItemPlace	pl;
	pl.type		= eItemPlaceRuck;
	if ( !GetPartner()->AllowItemToTrade(iitem, pl))
	{
		///R_ASSERT2( 0, make_string( "Partner can`t cell item (%s)", iitem->NameItem() ) );
		Msg( "! Partner can`t cell item (%s)", iitem->NameItem() );
		return false;
	}

	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= nullptr;
	
	if(b_use_cursor_pos)
	{
		new_owner						= CUIDragDropListEx::m_drag_item->BackList();
		VERIFY							(new_owner==GetTradePartnerList());
	}else
		new_owner						= GetTradePartnerList();


	CUICellItem* i = nullptr;
	i = old_owner->RemoveItem(itm, (old_owner == new_owner));
	
	if(b_use_cursor_pos)
		new_owner->SetItem				(i,old_owner->GetDragItemPosition());
	else
		new_owner->SetItem				(i);

	UpdatePrices();
	return true;
}

void CUIActorMenuBase::ToPartnerTradeAll(u32 item_amount)
{
	CUICellItem* itm = CurrentItem();
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toMove = (item_amount > totalCount) ? totalCount : item_amount;
	// Move children first: min(toMove, childCount)
	u32 const childrenToMove = (toMove < childCount) ? toMove : childCount;
	for (int i = 0; i < childrenToMove; ++i)
	{
		if (!ToPartnerTrade(itm->Child(0), false))
			return;
	}
	// Move parent only when moving entire stack (toMove > childCount)
	if (toMove > childCount)
		ToPartnerTrade(itm, false);
}

bool CUIActorMenuBase::ToPartnerTradeBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	// Перенос назад в список предметов NPC
	CUIDragDropListEx* old_owner = itm->OwnerList();
	CUIDragDropListEx* new_owner = nullptr;

	if (b_use_cursor_pos)
	{
		new_owner = CUIDragDropListEx::m_drag_item->BackList();
		VERIFY(new_owner == GetTradePartnerBagList());
	}
	else
		new_owner = GetTradePartnerBagList();
	CUICellItem* i = old_owner->RemoveItem(itm, (old_owner == new_owner));
	if (b_use_cursor_pos)
		new_owner->SetItem(i, old_owner->GetDragItemPosition());
	else
		new_owner->SetItem(i);

	return true;
}

void CUIActorMenuBase::ToPartnerTradeBagAll(u32 item_amount)
{
	CUICellItem* itm = CurrentItem();
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toMove = (item_amount > totalCount) ? totalCount : item_amount;
	// Move children first: min(toMove, childCount)
	u32 const childrenToMove = (toMove < childCount) ? toMove : childCount;
	for (int i = 0; i < childrenToMove; ++i)
	{
		if (!ToPartnerTradeBag(itm->Child(0), false))
			return;
	}
	// Move parent only when moving entire stack (toMove > childCount)
	if (toMove > childCount)
		ToPartnerTradeBag(itm, false);
}

bool CUIActorMenuBase::ToDeadBodyBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem quest_item = (PIItem)itm->m_pData;
	if (quest_item->IsQuestItem())
		return false;

	if ( GetPartner() )
	{
		if ( !GetPartner()->deadbody_can_take_status())
		{
			return false;
		}
		if (m_isCanMoveToPartner && GetPartner()->is_alive())
		{

			luabind::functor<bool> funct;
			R_ASSERT2(ai().script_engine().functor(m_onCanMoveToPartner, funct), "failed to get OnCanMoveToPartner functor");
			float itmWeight = quest_item->m_pInventory ? quest_item->m_pInventory->CalcItemWeight(quest_item) : quest_item->Weight();
			float partner_inv_weight = GetPartner()->inventory().CalcTotalWeight();
			float partner_max_weight = GetPartner()->MaxCarryWeight();

			if (funct(GetPartner()->cast_game_object()->lua_game_object(), quest_item->object().lua_game_object(), 0, 0, itmWeight, partner_inv_weight, partner_max_weight) == false)
				return false;
			
		}
	}
	else // box
	{
		// can_take: affects taking FROM box; placing INTO open box is always allowed
		if (m_isCanTake)
		{
			luabind::functor<bool> funct;
			R_ASSERT2(ai().script_engine().functor(m_onCanTake, funct), "failed to get OnCanTake functor");

			if (funct(GetInvBox()->cast_game_object()->lua_game_object(), quest_item->cast_game_object()->lua_game_object()) == false)
			{
				return false;
			}

		}

		if (!IsAllowPlaceToInvBox(itm)) {
			return false;
		}
	}

	CUIDragDropListEx* old_owner = itm->OwnerList();
	CUIDragDropListEx* new_owner = nullptr;

	if(b_use_cursor_pos)
	{
		new_owner						= CUIDragDropListEx::m_drag_item->BackList();
		VERIFY							(new_owner==GetPartnerList());
	}else
		new_owner						= GetPartnerList();
	
	CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );

	if(b_use_cursor_pos)
		new_owner->SetItem				(i,old_owner->GetDragItemPosition());
	else
		new_owner->SetItem				(i);

	PIItem iitem						= (PIItem)i->m_pData;

	if ( GetPartner() )
	{
		move_item_from_to				(GetInventoryOwner()->object_id(), GetPartner()->object_id(), iitem->object_id());
	}
	else // box
	{
		move_item_from_to				(GetInventoryOwner()->object_id(), GetInvBox()->ID(), iitem->object_id());
	}
	
	UpdateDeadBodyBag();
	return true;
}

bool CUIActorMenuBase::ToQuickSlot(CUICellItem* itm)
{
	PIItem iitem = (PIItem)itm->m_pData;
	CEatableItemObject* eat_item = smart_cast<CEatableItemObject*>(iitem);
	if(!eat_item)
		return false;

	//Alundaio: Prevent icons greater then 1x1 to be quick slotted
	Ivector2 iWH = iitem->GetInvGridRect().rb;
	if (iWH.x > 1 || iWH.y > 1)
		return false;
	//Alundaio: END

	u8 slot_idx = u8(m_pQuickSlot->PickCell(GetUICursor().GetCursorPosition()).x);
	if(slot_idx==255)
		return false;

	if (m_pQuickSlot && m_pQuickSlot->SetItem(create_cell_item(iitem), GetUICursor().GetCursorPosition())) {
		xr_strcpy(ACTOR_DEFS::g_quick_use_slots[slot_idx], iitem->m_section_id.c_str());
		return false;
	}

	return true;
}

bool CUIActorMenuBase::ToQuickSlotAt(CUICellItem* itm, u8 slotIndex)
{
	if (m_pQuickSlot == nullptr || slotIndex > 3)
	{
		return false;
	}

	PIItem iitem = (PIItem)itm->m_pData;
	CEatableItemObject* eatableObject = smart_cast<CEatableItemObject*>(iitem);
	if (eatableObject == nullptr)
	{
		return false;
	}

	const Ivector2 itemGrid = iitem->GetInvGridRect().rb;
	if (itemGrid.x > 1 || itemGrid.y > 1)
	{
		return false;
	}

	if (!iitem->Useful())
	{
		return false;
	}

	if (iitem->parent_id() != GetInventoryOwner()->object_id())
	{
		return false;
	}

	CUICellItem* slotCellItem = create_cell_item(iitem);
	if (m_pQuickSlot->SetItemAtQuickSlotCell(slotCellItem, slotIndex))
	{
		xr_strcpy(ACTOR_DEFS::g_quick_use_slots[slotIndex], iitem->m_section_id.c_str());
		return true;
	}

	xr_delete(slotCellItem);
	return false;
}

void CUIActorMenuBase::TransferItems( CUIDragDropListEx* pSellList, CUIDragDropListEx* pBuyList, CTrade* pTrade, bool bBuying )
{
	if (!IsGameTypeSingle())
	{
		TransferItemsMp(pSellList, pBuyList, pTrade, bBuying);
		return;
	}

	while ( pSellList->ItemsCount() )
	{
		CUICellItem* cell_item = pSellList->RemoveItem( pSellList->GetItemIdx(0), false );
		PIItem item = (PIItem)cell_item->m_pData;
		pTrade->TransferItem( item, bBuying );
		
		if ( bBuying )
		{
			SInvItemPlace	pl;
			pl.type		= eItemPlaceRuck;
			if ( pTrade->pThis.inv_owner->CInventoryOwner::AllowItemToTrade( item, pl ) )
			{
				pBuyList->SetItem( cell_item );
			}
		}
		else
		{
			pBuyList->SetItem( cell_item );
		}
	}
	pTrade->pThis.inv_owner->set_money(    pTrade->pThis.inv_owner->get_money(),    true );
	pTrade->pPartner.inv_owner->set_money( pTrade->pPartner.inv_owner->get_money(), true );
}

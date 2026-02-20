#include "stdafx.h"
#include "UIInventoryWnd.h"
#include "../Level.h"
#include "../Actor.h"
#include "../ActorCondition.h"
#include "../HUDManager.h"
#include "../Inventory.h"
#include "UIInventoryUtilities.h"

#include "UICellItem.h"
#include "UICellItemFactory.h"
#include "UIDragDropListEx.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/UICursor.h"

#include "../WeaponBinoculars.h"
#include "../WeaponKnife.h"
#include "../WeaponMagazinedWGrenade.h"

CUICellItem* CUIInventoryWnd::CurrentItem()
{
	return m_pCurrentCellItem;
}

PIItem CUIInventoryWnd::CurrentIItem()
{
	return	(m_pCurrentCellItem)?(PIItem)m_pCurrentCellItem->m_pData : nullptr;
}

void CUIInventoryWnd::SetCurrentItem(CUICellItem* itm)
{
	m_pCurrentCellItem				= itm;
	UIItemInfo.InitItem			(CurrentItem(), nullptr, CurrentIItem() ? CurrentIItem()->Cost() : u32(-1));
	TryHidePropertiesBox();
}

void CUIInventoryWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if(pWnd == &UIPropertiesBox &&	msg==PROPERTY_CLICKED)
	{
		ProcessPropertiesBoxClicked	();
	}else 
	if (UIExitButton == pWnd && BUTTON_CLICKED == msg)
	{
		GetHolder()->StartStopMenu			(this,true);
	}

	CUIWindow::SendMessage(pWnd, msg, pData);
}


void CUIInventoryWnd::InitInventory_delayed()
{
	m_b_need_reinit = true;
}

void CUIInventoryWnd::InitInventory() 
{
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	if(!pInvOwner)				return;

	m_pInv						= &pInvOwner->inventory();

	UIPropertiesBox.Hide		();
	ClearAllLists				();
	m_pMouseCapturer			= nullptr;
	SetCurrentItem				(nullptr);

	//Slots
	PIItem _itm							= m_pInv->m_slots[INV_SLOT_2].m_pIItem;
	if(_itm)
	{
		CUICellItem* itm				= create_cell_item(_itm);
		m_pUIPistolList->SetItem		(itm);
	}


	_itm								= m_pInv->m_slots[INV_SLOT_3].m_pIItem;
	if(_itm)
	{
		CUICellItem* itm				= create_cell_item(_itm);
		m_pUIAutomaticList->SetItem		(itm);
	}

	PIItem _outfit						= m_pInv->m_slots[OUTFIT_SLOT].m_pIItem;
	CUICellItem* outfit					= (_outfit)?create_cell_item(_outfit):nullptr;
	m_pUIOutfitList->SetItem			(outfit);

	TIItemContainer::iterator it, it_e;
	for(it=m_pInv->m_belt.begin(),it_e=m_pInv->m_belt.end(); it!=it_e; ++it) 
	{
		CUICellItem* itm			= create_cell_item(*it);
		m_pUIBeltList->SetItem		(itm);
	}


	
	ruck_list		= m_pInv->m_ruck;
	std::sort		(ruck_list.begin(),ruck_list.end(),InventoryUtilities::GreaterRoomInRuck);

	int i=1;
	for(it=ruck_list.begin(),it_e=ruck_list.end(); it!=it_e; ++it,++i) 
	{
		CUICellItem* itm			= create_cell_item(*it);
		m_pUIBagList->SetItem		(itm);
	}
	//fake
	_itm								= m_pInv->m_slots[GRENADE_SLOT].m_pIItem;
	if(_itm)
	{
		CUICellItem* itm				= create_cell_item(_itm);
		m_pUIBagList->SetItem			(itm);
	}

	InventoryUtilities::UpdateWeight					(UIBagWnd, true);

	m_b_need_reinit					= false;
}  

void CUIInventoryWnd::DropCurrentItem(bool b_all)
{
	CObject* current_entity = Level().CurrentEntity();
	CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;
	if(!pActor)				return;

	if(!b_all && CurrentIItem() && !CurrentIItem()->IsQuestItem())
	{
		SendEvent_Item_Drop		(CurrentIItem(), pActor->object_id());
		SetCurrentItem			(nullptr);
		InventoryUtilities::UpdateWeight			(UIBagWnd, true);
		return;
	}

	if(b_all && CurrentIItem() && !CurrentIItem()->IsQuestItem())
	{
		u32 cnt = CurrentItem()->ChildsCount();

		for(u32 i=0; i<cnt; ++i){
			CUICellItem*	itm				= CurrentItem()->PopChild(CurrentItem());
			PIItem			iitm			= (PIItem)itm->m_pData;
			SendEvent_Item_Drop				(iitm, pActor->object_id());
		}

		SendEvent_Item_Drop					(CurrentIItem(), pActor->object_id());
		SetCurrentItem						(nullptr);
		InventoryUtilities::UpdateWeight	(UIBagWnd, true);
		return;
	}
}

//------------------------------------------

bool CUIInventoryWnd::ToSlot(CUICellItem* itm, bool force_place, u16 slot_id)
{
	CUIDragDropListEx*	old_owner			= itm->OwnerList();
	PIItem	iitem							= (PIItem)itm->m_pData;
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	bool b_own_item							= (iitem->parent_id() == pInvOwner->object_id());

	if(GetInventory()->CanPutInSlot(iitem, slot_id))
	{
		CUIDragDropListEx* new_owner		= GetSlotList(slot_id);
		
		if(slot_id == GRENADE_SLOT)
			return true; //fake, sorry (((

		if (!new_owner)
			return false;

		bool result							= (!b_own_item) || GetInventory()->Slot(slot_id, iitem);
		VERIFY								(result);

		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		
		new_owner->SetItem					(i);
	
		SendEvent_Item2Slot					(iitem, pInvOwner->object_id(), slot_id);

		SendEvent_ActivateSlot				(slot_id, pInvOwner->object_id());
		
		return								true;
	}else
	{ // in case slot is busy
		if (!force_place || slot_id == NO_ACTIVE_SLOT)
			return false;

		if (GetInventory()->SlotIsPersistent(slot_id) && slot_id != DEVICE_SLOT)
			return false;

		const static bool pistolsOnly = EngineExternal()[EEngineExternalGame::EnableInventoryPistolSlot];
		if (slot_id == INV_SLOT_2 && GetInventory()->CanPutInSlot(iitem, INV_SLOT_3) && !pistolsOnly)
			return ToSlot(itm, force_place, INV_SLOT_3);

		if (slot_id == INV_SLOT_3 && GetInventory()->CanPutInSlot(iitem, INV_SLOT_2) && !pistolsOnly)
			return ToSlot(itm, force_place, INV_SLOT_2);

		PIItem	_iitem						= GetInventory()->ItemFromSlot(slot_id);
		CUIDragDropListEx* slot_list		= GetSlotList(slot_id);
		VERIFY								(slot_list->ItemsCount()==1);

		CUICellItem* slot_cell				= slot_list->GetItemIdx(0);
		VERIFY								(slot_cell && ((PIItem)slot_cell->m_pData)==_iitem);

		bool result							= ToBag(slot_cell, false);
		VERIFY								(result);

		return ToSlot						(itm, false, slot_id);
	}
}

bool CUIInventoryWnd::ToBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	PIItem	iitem						= (PIItem)itm->m_pData;

	if(GetInventory()->CanPutInRuck(iitem))
	{
		CUIDragDropListEx*	old_owner		= itm->OwnerList();
		CUIDragDropListEx*	new_owner		= nullptr;
		if(b_use_cursor_pos){
				new_owner					= CUIDragDropListEx::m_drag_item->BackList();
				VERIFY						(new_owner==m_pUIBagList);
		}else
				new_owner					= m_pUIBagList;


		bool result							= GetInventory()->Ruck(iitem);
		VERIFY								(result);
		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		if (!i)
			return false;

		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);

		SendEvent_Item2Ruck					(iitem, pInvOwner->object_id());
		return true;
	}
	return false;
}

bool CUIInventoryWnd::ToBelt(CUICellItem* itm, bool b_use_cursor_pos)
{
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	PIItem	iitem						= (PIItem)itm->m_pData;
	bool b_own_item = (iitem->parent_id() == pInvOwner->object_id());

	if(GetInventory()->CanPutInBelt(iitem))
	{
		CUIDragDropListEx*	old_owner		= itm->OwnerList();
		CUIDragDropListEx*	new_owner		= nullptr;
		if(b_use_cursor_pos){
				new_owner					= CUIDragDropListEx::m_drag_item->BackList();
				VERIFY						(new_owner==m_pUIBeltList);
		}else
				new_owner					= m_pUIBeltList;

		bool result							= (!b_own_item) || GetInventory()->Belt(iitem);
		VERIFY								(result);
		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		
		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);

		if (!b_own_item)
			SendEvent_Item2Belt				(iitem, pInvOwner->object_id());

		return								true;
	}
	else
	{ // in case belt slot is busy
		if(!iitem->Belt() || GetInventory()->BeltWidth()==0)
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

		bool result							= ToBag(slot_cell, false);
		VERIFY								(result);

		result								= ToBelt(itm, b_use_cursor_pos);
		return result;
	}
}

void CUIInventoryWnd::AddItemToBag(PIItem pItem)
{
	CUICellItem* itm						= create_cell_item(pItem);
	m_pUIBagList->SetItem					(itm);
}

bool CUIInventoryWnd::OnItemStartDrag(CUICellItem* itm)
{
	return false; //default behaviour
}

bool CUIInventoryWnd::OnItemSelected(CUICellItem* itm)
{
	SetCurrentItem		(itm);
	return				false;
}

bool CUIInventoryWnd::OnItemDrop(CUICellItem* itm)
{
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= CUIDragDropListEx::m_drag_item->BackList();
	if(old_owner==new_owner || !old_owner || !new_owner)
					return false;

	EListType t_new		= GetType(new_owner);
	EListType t_old		= GetType(old_owner);
	if(t_new == t_old)	return true;

	switch(t_new){
		case iwSlot:{
			if(GetSlotList(CurrentIItem()->BaseSlot())==new_owner)
				ToSlot	(itm, true, CurrentIItem()->BaseSlot());
		}break;
		case iwBag:{
			ToBag	(itm, true);
		}break;
		case iwBelt:{
			ToBelt	(itm, true);
		}break;
	};

	DropItem				(CurrentIItem(), new_owner);

	return true;
}

bool CUIInventoryWnd::OnItemDbClick(CUICellItem* itm)
{
	if(TryUseItem((PIItem)itm->m_pData))		
		return true;

	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	EListType t_old						= GetType(old_owner);

	switch(t_old){
		case iwSlot:{
			ToBag	(itm, false);
		}break;

		case iwBag:
		{
			PIItem iitem_to_place = (PIItem)itm->m_pData;
			if (TryUseItem(iitem_to_place))
			{
				break;
			}
			if(!ToSlot(itm, false, iitem_to_place->BaseSlot()))
			{
				if (!ToBelt(itm, false))
				{
					ToSlot(itm, true, iitem_to_place->BaseSlot());
				}
			}
		}break;

		case iwBelt:{
			ToBag	(itm, false);
		}break;
	};

	return true;
}


bool CUIInventoryWnd::OnItemRButtonClick(CUICellItem* itm)
{
	SetCurrentItem				(itm);
	ActivatePropertiesBox		();
	return						false;
}

bool CUIInventoryWnd::OnItemFocusReceive(CUICellItem* itm)
{
	itm->m_selected = true;
	set_highlight_item(itm);

	return true;
}

bool CUIInventoryWnd::OnItemFocusLost(CUICellItem* itm)
{
	if (itm)
	{
		itm->m_selected = false;
	}
	clear_highlight_lists();

	return true;
}

bool CUIInventoryWnd::OnItemFocusedUpdate(CUICellItem* itm)
{
	if (itm)
	{
		itm->m_selected = true;
		if (m_highlight_clear)
		{
			set_highlight_item(itm);
		}
	}

	return true;
}

CUIDragDropListEx* CUIInventoryWnd::GetSlotList(u32 slot_idx)
{
	if(slot_idx == NO_ACTIVE_SLOT || GetInventory()->SlotIsPersistent(slot_idx))	
		return nullptr;

	switch (slot_idx)
	{
		case INV_SLOT_2:
			return m_pUIPistolList;
			break;

		case INV_SLOT_3:
			return m_pUIAutomaticList;
			break;

		case OUTFIT_SLOT:
			return m_pUIOutfitList;
			break;

	};
	return m_pUIBagList;
}

void CUIInventoryWnd::set_highlight_item(CUICellItem* cell_item)
{
	PIItem item = (PIItem)cell_item->m_pData;
	if (!item)
	{
		return;
	}
	//highlight_item_slot(cell_item);

	highlight_armament(item, m_pUIBagList);
	m_highlight_clear = false;
}


void CUIInventoryWnd::clear_highlight_lists()
{
/*	for (u8 i = 1; i <= m_slot_count; ++i)
	{
		if (m_pInvSlotHighlight[i])
			m_pInvSlotHighlight[i]->Show(false);
	}

	if (m_QuickSlotsHighlight[0])
	{
		for (u8 i = 0; i < 4; i++)
			m_QuickSlotsHighlight[i]->Show(false);
	}
	if (m_ArtefactSlotsHighlight[0])
	{
		for (u8 i = 0; i < m_ArtefactSlotsCount; i++)
			m_ArtefactSlotsHighlight[i]->Show(false);
	}*/

	m_pUIBagList->clear_select_armament();

	m_highlight_clear = true;
}

void CUIInventoryWnd::highlight_armament(PIItem item, CUIDragDropListEx* ddlist)
{
	ddlist->clear_select_armament();
	highlight_ammo_for_weapon(item, ddlist);
	highlight_weapons_for_ammo(item, ddlist);
	highlight_weapons_for_addon(item, ddlist);
}

void CUIInventoryWnd::highlight_ammo_for_weapon( PIItem weapon_item, CUIDragDropListEx* ddlist )
{
	VERIFY( weapon_item );
	VERIFY( ddlist );
	static xr_vector<shared_str>	ammo_types;
	ammo_types.resize(0);

	CWeapon* weapon = weapon_item->cast_weapon();
	CWeaponBinoculars* binoc = weapon_item->cast_weapon_binoculars();
	CWeaponKnife* knife = weapon_item->cast_weapon_knife();
	if ( !weapon || binoc || knife)
	{
		return;
	}
	ammo_types.assign( weapon->m_ammoTypes.begin(), weapon->m_ammoTypes.end() );

	CWeaponMagazinedWGrenade* wg = weapon_item->cast_weapon_magazined_w_grenade();
	if ( wg )
	{
		if ( wg->IsGrenadeLauncherAttached() && wg->m_ammoTypes2.size() )
		{
			ammo_types.insert( ammo_types.end(), wg->m_ammoTypes2.begin(), wg->m_ammoTypes2.end() );
		}
	}
	
	if ( ammo_types.size() == 0 )
	{
		return;
	}
	xr_vector<shared_str>::iterator ite = ammo_types.end();
	
	u32 const cnt = ddlist->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if ( !item )
		{
			continue;
		}
		CWeaponAmmo* ammo = item->cast_weapon_ammo();
		if ( !ammo )
		{
			highlight_addons_for_weapon( weapon_item, ci );
			continue; // for i
		}
		shared_str const& ammo_name = item->object().cNameSect();

		xr_vector<shared_str>::iterator itb = ammo_types.begin();
		for ( ; itb != ite; ++itb )
		{
			if ( ammo_name._get() == (*itb)._get() )
			{
				ci->m_select_armament = true;
				break; // itb
			}
		}
	}//for i

}

void CUIInventoryWnd::highlight_weapons_for_ammo( PIItem ammo_item, CUIDragDropListEx* ddlist )
{
	VERIFY( ammo_item );
	VERIFY( ddlist );
	CWeaponAmmo* ammo = ammo_item->cast_weapon_ammo();
	CWeaponBinoculars* binoc = ammo_item->cast_weapon_binoculars();
	CWeaponKnife* knife = ammo_item->cast_weapon_knife();
	if ( !ammo  )
	{
		return;
	}
	
	shared_str const& ammo_name = ammo_item->object().cNameSect();

	u32 const cnt = ddlist->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if ( !item )
		{
			continue;
		}
		CWeapon* weapon = item->cast_weapon();
		if (!weapon || binoc || knife)
		{
			continue;
		}

		xr_vector<shared_str>::iterator itb = weapon->m_ammoTypes.begin();
		xr_vector<shared_str>::iterator ite = weapon->m_ammoTypes.end();
		for ( ; itb != ite; ++itb )
		{
			if ( ammo_name._get() == (*itb)._get() )
			{
				ci->m_select_armament = true;
				break; // for itb
			}
		}
		
		CWeaponMagazinedWGrenade* wg = item->cast_weapon_magazined_w_grenade();
		if ( !wg || !wg->IsGrenadeLauncherAttached() || !wg->m_ammoTypes2.size() )
		{
			continue; // for i
		}
		itb = wg->m_ammoTypes2.begin();
		ite = wg->m_ammoTypes2.end();
		for ( ; itb != ite; ++itb )
		{
			if ( ammo_name._get() == (*itb)._get() )
			{
				ci->m_select_armament = true;
				break; // for itb
			}
		}
	}//for i

}

bool CUIInventoryWnd::highlight_addons_for_weapon( PIItem weapon_item, CUICellItem* ci )
{
	PIItem item = (PIItem)ci->m_pData;
	if ( !item )
	{
		return false;
	}

	CScope* pScope = item->cast_addon_scope();
	if (pScope && weapon_item->CanAttach(item))
	{
		ci->m_select_armament = true;
		return true;
	}

	CSilencer* pSilencer = item->cast_addon_silencer();
	if ( pSilencer && weapon_item->CanAttach(pSilencer) )
	{
		ci->m_select_armament = true;
		return true;
	}

	CGrenadeLauncher* pGrenadeLauncher = item->cast_addon_grenade_launcher();
	if ( pGrenadeLauncher && weapon_item->CanAttach(pGrenadeLauncher) )
	{
		ci->m_select_armament = true;
		return true;
	}
	return false;
}

void CUIInventoryWnd::highlight_weapons_for_addon( PIItem addon_item, CUIDragDropListEx* ddlist )
{
	VERIFY( addon_item );
	VERIFY( ddlist );

	CScope* pScope = addon_item->cast_addon_scope();
	CSilencer* pSilencer = addon_item->cast_addon_silencer();
	CGrenadeLauncher* pGrenadeLauncher = addon_item->cast_addon_grenade_launcher();

	if ( !pScope && !pSilencer && !pGrenadeLauncher )
	{
		return;
	}
	
	u32 const cnt = ddlist->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if ( !item )
		{
			continue;
		}
		CWeapon* weapon = item->cast_weapon();
		if ( !weapon )
		{
			continue;
		}

		if (pScope && weapon->ScopeAttachable() && weapon->ScopeFit(pScope))
		{
			ci->m_select_armament = true;
			continue;
		}
		if ( pSilencer && weapon->CanAttach(pSilencer) )
		{
			ci->m_select_armament = true;
			continue;
		}
		if ( pGrenadeLauncher && weapon->CanAttach(pGrenadeLauncher) )
		{
			ci->m_select_armament = true;
			continue;
		}

	}//for i
}

void CUIInventoryWnd::ClearAllLists()
{
	m_pUIBagList->ClearAll					(true);
	m_pUIBeltList->ClearAll					(true);
	m_pUIOutfitList->ClearAll				(true);
	m_pUIPistolList->ClearAll				(true);
	m_pUIAutomaticList->ClearAll			(true);
}

void CUIInventoryWnd::DropAllCurrentItem(u32 item_amount)
{
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	if ( CurrentIItem() && !CurrentIItem()->IsQuestItem() )
	{
		u32 childCount = CurrentItem()->ChildsCount();
		u32 toPop = (item_amount < childCount) ? item_amount : childCount;
		for ( u32 i = 0; i < toPop; ++i )
		{
			CUICellItem*	itm  = CurrentItem()->PopChild(nullptr);
			PIItem			iitm = (PIItem)itm->m_pData;
			SendEvent_Item_Drop( iitm, pInvOwner->object_id() );
		}

		if ( item_amount > childCount )
			SendEvent_Item_Drop( CurrentIItem(), pInvOwner->object_id() );
	}
	SetCurrentItem								(nullptr);
}

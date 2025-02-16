#include "stdafx.h"
#include "UIInventoryWnd.h"
#include "../level.h"
#include "../actor.h"
#include "../ActorCondition.h"
#include "../hudmanager.h"
#include "../inventory.h"
#include "UIInventoryUtilities.h"

#include "UICellItem.h"
#include "UICellItemFactory.h"
#include "UIDragDropListEx.h"
#include "UI3tButton.h"
#include "../customoutfit.h"

CUICellItem* CUIInventoryWnd::CurrentItem()
{
	return m_pCurrentCellItem;
}

PIItem CUIInventoryWnd::CurrentIItem()
{
	return	(m_pCurrentCellItem)?(PIItem)m_pCurrentCellItem->m_pData : NULL;
}

void CUIInventoryWnd::SetCurrentItem(CUICellItem* itm)
{
	if(m_pCurrentCellItem == itm) return;

	m_pCurrentCellItem				= itm;

	UIItemInfo.InitItem			(CurrentIItem());
}

void CUIInventoryWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if(pWnd == &UIPropertiesBox &&	msg==PROPERTY_CLICKED)
	{
		ProcessPropertiesBoxClicked	();
	}else 
	if (UIExitButton == pWnd && BUTTON_CLICKED == msg)
	{
		HideDialog();
	}

	CUIWindow::SendMessage(pWnd, msg, pData);
}


void CUIInventoryWnd::InitInventory_delayed()
{
	m_b_need_reinit = true;
}

void CUIInventoryWnd::InitSlotItem(CUIDragDropListEx* list, TSlotId slot)
{
	PIItem _itm	= m_pInv->ItemFromSlot(slot);
	if (_itm)
	{
		list->SetItem(create_cell_item(_itm));
	}
}

void CUIInventoryWnd::InitInventory() 
{
	CInventoryOwner *pInvOwner	= smart_cast<CInventoryOwner*>(Level().CurrentEntity());
	if(!pInvOwner)				return;

	m_pInv						= &pInvOwner->inventory();

	UIPropertiesBox.Hide		();
	ClearAllLists				();
	m_pMouseCapturer			= NULL;
	SetCurrentItem				(NULL);

	// Slots
	InitSlotItem(m_pUIPistolList, PISTOL_SLOT);
	InitSlotItem(m_pUIAutomaticList, RIFLE_SLOT);
	if (m_pUIAutomatic2List)
	{
		InitSlotItem(m_pUIAutomatic2List, RIFLE_2_SLOT);
	}
	InitSlotItem(m_pUIKnifeList, KNIFE_SLOT);
	InitSlotItem(m_pUIBinocularList, APPARATUS_SLOT);
	InitSlotItem(m_pUITorchList, TORCH_SLOT);
	InitSlotItem(m_pUIDetectorList, DETECTOR_SLOT);
	InitSlotItem(m_pUIHelmetList, HELMET_SLOT);
	InitSlotItem(m_pUIPNVList, PNV_SLOT);
	InitSlotItem(m_pUIAnomDetectorList, ANOM_DET_SLOT);
	InitSlotItem(m_pUIOutfitList, OUTFIT_SLOT);

	// Belt
	TIItemContainer::iterator it, it_e;
	for(it=m_pInv->m_belt.begin(),it_e=m_pInv->m_belt.end(); it!=it_e; ++it) 
	{
		CUICellItem* itm			= create_cell_item(*it);
		m_pUIBeltList->SetItem		(itm);
	}
	
	// Ruck
	ruck_list		= m_pInv->m_ruck;
	std::sort		(ruck_list.begin(),ruck_list.end(),InventoryUtilities::GreaterRoomInRuck);

	int i=1;
	for(it=ruck_list.begin(),it_e=ruck_list.end(); it!=it_e; ++it,++i) 
	{
		CUICellItem* itm			= create_cell_item(*it);
		m_pUIBagList->SetItem		(itm);
	}
	//fake
	auto _itm							= m_pInv->ItemFromSlot(GRENADE_SLOT);
	if (_itm)
	{
		CUICellItem* itm				= create_cell_item(_itm);
		m_pUIBagList->SetItem			(itm);
	}

	//InventoryUtilities::UpdateWeight					(UIBagWnd, true);

	m_b_need_reinit					= false;
}  

void CUIInventoryWnd::DropCurrentItem(bool b_all)
{

	CActor *pActor			= smart_cast<CActor*>(Level().CurrentEntity());
	if(!pActor)				return;

	if(!b_all && CurrentIItem() && !CurrentIItem()->IsQuestItem())
	{
		SendEvent_Item_Drop		(CurrentIItem());
		SetCurrentItem			(NULL);
		//InventoryUtilities::UpdateWeight			(UIBagWnd, true);
		return;
	}

	if(b_all && CurrentIItem() && !CurrentIItem()->IsQuestItem())
	{
		u32 cnt = CurrentItem()->ChildsCount();

		for(u32 i=0; i<cnt; ++i)
		{
			CUICellItem*	itm				= CurrentItem()->PopChild(NULL);
			PIItem			iitm			= (PIItem)itm->m_pData;
			SendEvent_Item_Drop				(iitm);
		}

		SendEvent_Item_Drop					(CurrentIItem());
		SetCurrentItem						(NULL);
		//InventoryUtilities::UpdateWeight	(UIBagWnd, true);
		return;
	}
}

//------------------------------------------

bool CUIInventoryWnd::ToSlot(CUICellItem* itm, bool force_place, TSlotId slot_id)
{
	CUIDragDropListEx*	old_owner			= itm->OwnerList();
	PIItem	iitem							= (PIItem)itm->m_pData;

	bool canPut = GetInventory()->CanPutInSlot(iitem, slot_id);
	// Msg("ToSlot, slot: %d, canPut: %d", slot_id, canPut);
	if (canPut) {
		CUIDragDropListEx* new_owner		= GetSlotList(slot_id);
		if(slot_id==GRENADE_SLOT && !new_owner )return true; //fake, sorry (((

		//tatarinrafa: block putting pnv or helmet in slot if outfit does not allow that
		if (slot_id == HELMET_SLOT || slot_id == PNV_SLOT){

			PIItem	itemformoutfitslot = GetInventory()->ItemFromSlot(OUTFIT_SLOT);
			if (itemformoutfitslot)
			{
				CCustomOutfit* outfit = smart_cast<CCustomOutfit*>(itemformoutfitslot);//на всякий случай проверим если это броня в слоте брони, а то..
				if (outfit)
				{

					if (iitem->BaseSlot() == HELMET_SLOT && outfit->block_helmet_slot == 1)
						return false;

					if (iitem->BaseSlot() == PNV_SLOT && outfit->block_pnv_slot == 1)
						return false;
				}
			}
		}

		CCustomOutfit* outfit = smart_cast<CCustomOutfit*>(iitem);
		if (outfit){

			if (outfit->block_pnv_slot == 1){
				CInventoryItem* pnv = GetInventory()->ItemFromSlot(PNV_SLOT);
				if (pnv){

					CUICellItem* slot_cell = GetSlotList(PNV_SLOT)->GetItemIdx(0);
					ToBag(slot_cell, false);
				}
			}

			if (outfit->block_helmet_slot == 1){
				CInventoryItem* helmet = GetInventory()->ItemFromSlot(HELMET_SLOT);
				if (helmet){

					CUICellItem* slot_cell = GetSlotList(HELMET_SLOT)->GetItemIdx(0);
					ToBag(slot_cell, false);
				}
			}
		}
		//end tatarinrafa: block putting pnv or helmet in slot if outfit does not allow that

		bool result							= GetInventory()->Slot(slot_id, iitem);
		VERIFY								(result);

		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		if (!new_owner) return false;
		else new_owner->SetItem					(i);
		SendEvent_Item2Slot					(iitem, slot_id);

		SendEvent_ActivateSlot				(iitem, slot_id);
		
		return								true;
	}else
	{ // in case slot is busy
		if(!force_place ||  slot_id==NO_ACTIVE_SLOT || GetInventory()->m_slots[slot_id].m_bPersistent) return false;

		if ( slot_id == RIFLE_SLOT && GetInventory()->CanPutInSlot(iitem, RIFLE_2_SLOT))
			return ToSlot(itm, force_place, RIFLE_2_SLOT);

		if ( slot_id == RIFLE_2_SLOT && GetInventory()->CanPutInSlot(iitem, RIFLE_SLOT))
			return ToSlot(itm, force_place, RIFLE_SLOT);

		TSlotId oldSlot						= iitem->CurrSlot();
		PIItem	_iitem						= GetInventory()->ItemFromSlot(slot_id);
		CUIDragDropListEx* slot_list		= GetSlotList(slot_id);
		VERIFY								(slot_list->ItemsCount()==1);

		CUICellItem* slot_cell				= slot_list->GetItemIdx(0);
		VERIFY								(slot_cell && ((PIItem)slot_cell->m_pData)==_iitem);

		bool result							= ToBag(slot_cell, false);
		VERIFY								(result);

		return								ToSlot(itm, false, slot_id);
	}
}

bool CUIInventoryWnd::ToBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem	iitem						= (PIItem)itm->m_pData;

	if(GetInventory()->CanPutInRuck(iitem))
	{
		CUIDragDropListEx*	old_owner		= itm->OwnerList();
		CUIDragDropListEx*	new_owner		= NULL;
		if(b_use_cursor_pos){
				new_owner					= CUIDragDropListEx::m_drag_item->BackList();
				VERIFY						(new_owner==m_pUIBagList);
		}else
				new_owner					= m_pUIBagList;


		bool result							= GetInventory()->Ruck(iitem);
		VERIFY								(result);
		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		
		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);

		if (!result) return true;
		SendEvent_Item2Ruck					(iitem);
		return true;
	}
	return false;
}

bool CUIInventoryWnd::ToBelt(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem	iitem						= (PIItem)itm->m_pData;

	if (GetInventory()->CanPutInBelt(iitem, false))
	{
		if (GetInventory()->CanPutInBelt(iitem, true))
		{
			CUIDragDropListEx*	old_owner		= itm->OwnerList();
			CUIDragDropListEx*	new_owner		= NULL;
			if(b_use_cursor_pos){
					new_owner					= CUIDragDropListEx::m_drag_item->BackList();
					VERIFY						(new_owner==m_pUIBeltList);
			}else
					new_owner					= m_pUIBeltList;

			CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );

			if(b_use_cursor_pos)
				new_owner->SetItem				(i,old_owner->GetDragItemPosition());
			else
				new_owner->SetItem				(i);
		}

		bool result							= GetInventory()->Belt(iitem);
		if (result)
		{
			SendEvent_Item2Belt					(iitem);
		}
		return								true;
	}
	return									false;
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
	if (m_pCurrentCellItem) m_pCurrentCellItem->Mark(false);
	SetCurrentItem		(itm);
	ColorizeAmmo		(itm);
	if (itm) itm->Mark(true);

	return				false;
}

void CUIInventoryWnd::ColorizeAmmo(CUICellItem* itm)
{
	CInventoryItem* inventoryitem = (CInventoryItem*) itm->m_pData;
	if (!inventoryitem) return;

	//clear texture color
	//for bag
	u32 item_count = m_pUIBagList->ItemsCount();
		for (u32 i=0;i<item_count;++i) {
			CUICellItem* bag_item = m_pUIBagList->GetItemIdx(i);
			PIItem invitem = (PIItem) bag_item->m_pData;

			bag_item->SetTextureColor				(0xffffffff);
		}
	//for belt
	u32 belt_item_count = m_pUIBeltList->ItemsCount();
		for (u32 i=0;i<belt_item_count;++i) {
			CUICellItem* belt_item = m_pUIBeltList->GetItemIdx(i);
			PIItem invitem = (PIItem) belt_item->m_pData;

			belt_item->SetTextureColor				(0xffffffff);
		}

	CWeaponMagazined* weapon = smart_cast<CWeaponMagazined*>(inventoryitem);
	if (!weapon) return;

	xr_vector<shared_str> ammo_types = weapon->m_ammoTypes;
	
	u32 color = pSettings->r_color("inventory_color_ammo","color");

	//for bag
	for (size_t id = 0;id<ammo_types.size();++id) {
	u32 item_count = m_pUIBagList->ItemsCount();
		for (u32 i=0;i<item_count;++i) {
			CUICellItem* bag_item = m_pUIBagList->GetItemIdx(i);
			PIItem invitem = (PIItem) bag_item->m_pData;

			if (invitem && xr_strcmp(invitem->object().cNameSect(), ammo_types[id])==0 && invitem->Useful()) {
				bag_item->SetTextureColor				(color);
				break;										//go out from loop, because we can't have 2 CUICellItem's with same section
			}

		}
	}

	//for belt
	for (size_t id = 0;id<ammo_types.size();++id) {
	u32 belt_item_count = m_pUIBeltList->ItemsCount();
		for (u32 i=0;i<belt_item_count;++i) {
			CUICellItem* belt_item = m_pUIBeltList->GetItemIdx(i);
			PIItem invitem = (PIItem) belt_item->m_pData;

			if (invitem && xr_strcmp(invitem->object().cNameSect(), ammo_types[id])==0 && invitem->Useful()) {
				belt_item->SetTextureColor				(color);
			}

		}
	}
}

void CUIInventoryWnd::SumAmmoByDrop(CUICellItem* cell_itm, CUIDragDropListEx* owner)
{
	u32 idx = owner->GetItemIdx				(owner->GetDragItemPosition());
	if (idx==u32(-1)) return;
	if (!(idx<owner->ItemsCount())) return;

	CUICellItem* itemTo = owner->GetItemIdx(idx);

	if (!itemTo) return;

	CWeaponAmmo* ammoCurrent =	 smart_cast<CWeaponAmmo*>((CInventoryItem*)cell_itm->m_pData);
	CWeaponAmmo* ammoTo		 =	 smart_cast<CWeaponAmmo*>((CInventoryItem*)itemTo->m_pData);

	if (!ammoCurrent || !ammoTo) return;

	u16 freeSpaceTo = ammoTo->m_boxSize - ammoTo->m_boxCurr;

	if (freeSpaceTo>=ammoCurrent->m_boxCurr)
	{
		ammoTo->m_boxCurr+=ammoCurrent->m_boxCurr;
		DeleteFromInventory			((CInventoryItem*)cell_itm->m_pData);
	} else {
		ammoCurrent->m_boxCurr-=freeSpaceTo;
		ammoTo->m_boxCurr=ammoTo->m_boxSize;
	}

}

bool CUIInventoryWnd::OnItemDrop(CUICellItem* itm)
{
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= CUIDragDropListEx::m_drag_item->BackList();

	if(!old_owner || !new_owner)
		return false;
	
	EListType t_new		= GetType(new_owner);
	EListType t_old		= GetType(old_owner);
	
	if (old_owner==new_owner)
	{
		if (t_new==iwBelt)
			SumAmmoByDrop(itm, old_owner);

		return false;
	}

	if (t_new != iwSlot && t_new == t_old) return true;

	switch(t_new){
		case iwSlot:{
			TSlotId targetSlot = GetSlot(new_owner, itm);
			if (SlotIsCompatible(targetSlot, itm))
			{
				ToSlot	(itm, true, targetSlot);
			}
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
	auto iitem = (PIItem)itm->m_pData;
	if(TryUseItem(iitem))		
		return true;

	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	EListType t_old						= GetType(old_owner);

	switch(t_old){
		case iwSlot:{
			ToBag	(itm, false);
		}break;

		case iwBag:{
			TSlotId baseSlot = iitem->BaseSlot();
			if (!ToSlot(itm, false, baseSlot)) {
				if (!ToBelt(itm, false))
					ToSlot(itm, true, baseSlot);
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

CUIDragDropListEx* CUIInventoryWnd::GetSlotList(TSlotId slot_idx)
{
	if(slot_idx == NO_ACTIVE_SLOT || GetInventory()->m_slots[slot_idx].m_bPersistent)	return NULL;
	switch (slot_idx)
	{
		case PISTOL_SLOT:
			return m_pUIPistolList;
			break;

		case RIFLE_SLOT:
			return m_pUIAutomaticList;
			break;

		case RIFLE_2_SLOT:
			return m_pUIAutomatic2List;
			break;

		case KNIFE_SLOT:
			return m_pUIKnifeList;
			break;

		case APPARATUS_SLOT:
			return m_pUIBinocularList;
			break;

		case TORCH_SLOT:
			return m_pUITorchList;
			break;

		case OUTFIT_SLOT:
			return m_pUIOutfitList;
			break;

		case DETECTOR_SLOT:
			return m_pUIDetectorList;
			break;

		case HELMET_SLOT:
			return m_pUIHelmetList;
			break;

		case PNV_SLOT:
			return m_pUIPNVList;
			break;

		case ANOM_DET_SLOT:
			return m_pUIAnomDetectorList;
			break;


	};
	return NULL;
}



void CUIInventoryWnd::ClearAllLists()
{
	m_pUIBagList->ClearAll(true);
	m_pUIBeltList->ClearAll(true);

	m_pUIOutfitList->ClearAll(true);

	m_pUIPistolList->ClearAll(true);
	m_pUIAutomaticList->ClearAll(true);
	if (m_pUIAutomatic2List)
	{
		m_pUIAutomatic2List->ClearAll(true);
	}
	m_pUIKnifeList->ClearAll(true);
	m_pUIBinocularList->ClearAll(true);
	m_pUITorchList->ClearAll(true);
	m_pUIDetectorList->ClearAll(true);
	m_pUIHelmetList->ClearAll(true);
	m_pUIPNVList->ClearAll(true);
	m_pUIAnomDetectorList->ClearAll(true);
}
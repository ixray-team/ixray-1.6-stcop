#include "StdAfx.h"
#include "UIActorMenuBase.h"
#include "UIDragDropListEx.h"
#include "UICellItem.h"
#include "../InventoryOwner.h"
#include "../IPowerManager.h"
#include "../antigas_filter.h"
#include "../antigas.h"
#include "UIItemInfo.h"
#include "UIDragDropReferenceList.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/UICursor.h"
#include "../script_game_object.h"

bool CUIActorMenuBase::OnItemDrop(CUICellItem* itm)
{
	InfoCurItem( nullptr );

	bool is_on_item_dropped_callback_processed = false;

	CUIDragDropListEx*	old_owner = itm->OwnerList();
	CUIDragDropListEx*	new_owner = CUIDragDropListEx::m_drag_item->BackList();

	if ( !old_owner || !new_owner )
	{
		return false;
	}

	EDDListType t_new = GetListType(new_owner);
	EDDListType t_old = GetListType(old_owner);

	if ( !AllowItemDrops(t_old, t_new) )
	{
		Msg("incorrect action [%d]->[%d]",t_old, t_new);
		return true;
	}
	
	if (old_owner == new_owner)
	{
		if (!is_on_item_dropped_callback_processed)
		{
			CGameObject* GO1 = CurrentIItem() != nullptr ? CurrentIItem()->cast_game_object() : nullptr;
			CGameObject* GO2 = nullptr;

			if (m_lastFocusRecivedItem != nullptr && m_lastFocusLostItem_id != u16(0xffff) && m_lastFocusRecivedItem->object_id() != m_lastFocusLostItem_id && m_lastFocusRecivedItem->m_pInventory != nullptr) 
			{
				GO2 = m_lastFocusRecivedItem->cast_game_object();
			}

			is_on_item_dropped_callback_processed = true;

			if (GO1 != nullptr && GO2 != nullptr && GO1->H_Parent()->ID() == GO2->H_Parent()->ID())
			{
				AntigasFilter* filter = smart_cast<AntigasFilter*>(GO1);
				IAntigas* antigas = smart_cast<IAntigas*>(GO2);
				if (filter != nullptr && antigas != nullptr && antigas->IsFilterInWhiteList(GO1->cNameSect()))
				{
					if (antigas->InstallFilter(GO1->cast_inventory_item())) {
						old_owner->RemoveItem(itm, false);
						SetCurrentItem(nullptr);
						return true;
					}
				}

				PowerCell* oPowerCell = smart_cast<PowerCell*>(GO1);

				PowerBank* oPowerBank = smart_cast<PowerBank*>(GO2);
				if (oPowerCell != nullptr && oPowerBank != nullptr)
				{
					if (oPowerBank->InsertPowerCell(oPowerCell))
					{
						old_owner->RemoveItem(itm, false);
						SetCurrentItem(nullptr);
						return true;
					}
				}

				IPowerManager* oPowerManager = smart_cast<IPowerManager*>(GO2);
				if (oPowerCell != nullptr && oPowerManager != nullptr)
				{
					if (oPowerManager->IstallPowerCell(oPowerCell))
					{
						old_owner->RemoveItem(itm, false);
						SetCurrentItem(nullptr);
						return true;
					}
				}
			}

			if (m_isItemDropped) 
			{
				luabind::functor<bool> funct1;
				R_ASSERT2(ai().script_engine().functor(m_onItemDropped, funct1), "failed to get OnItemDropped functor");
				if (funct1(GO1 ? GO1->lua_game_object() : (0), GO2 ? GO2->lua_game_object() : (0), (int)t_old, (int)t_new) == false)
				{
					return false;
				}
			}
		}
		else
		{
			return false;
		}
	}

	switch(t_new)
	{
	case iTrashSlot:
		{
			if(CurrentIItem()->IsQuestItem())
				return true;

			if(t_old==iQuickSlot)	
			{
				old_owner->RemoveItem(itm, false);
				return true;
			}

			SendEvent_Item_Drop(CurrentIItem(), GetInventoryOwner()->object_id());
			SetCurrentItem(nullptr);
		}break;
	case iActorSlot:
		{
			u16 slot_to_place;
			if( CanSetItemToList(CurrentIItem(), new_owner, slot_to_place) )
			{
				ToSlot	(itm, true, slot_to_place);
			}
			else if (m_pQuickSlot)
			{
				Frect quickRect;
				m_pQuickSlot->GetAbsoluteRect(quickRect);
				Fvector2 cursorPos = GetUICursor().GetCursorPosition();
				if (quickRect.in(cursorPos))
					ToQuickSlot(itm);
			}
		}break;
	case iActorBag:
		{
			ToBag	(itm, true);
		}break;
	case iActorBelt:
		{
			ToBelt	(itm, true);
		}break;
	case iActorTrade:
		{
			ToActorTrade(itm, true);
		}break;
	case iPartnerTrade:
		{
			if(t_old!=iPartnerTradeBag)	
				return false;
			ToPartnerTrade(itm, true);
		}break;
	case iPartnerTradeBag:
		{
			if(t_old!=iPartnerTrade)	
				return false;
			ToPartnerTradeBag(itm, true);
		}break;
	case iDeadBodyBag:
		{
			ToDeadBodyBag(itm, true);
		}break;
	case iQuickSlot:
		{
			ToQuickSlot(itm);
		}break;
	};

	OnItemDropped(CurrentIItem(), new_owner, old_owner);

	if (!is_on_item_dropped_callback_processed)
	{
		CGameObject* GO1 = CurrentIItem() != nullptr ? CurrentIItem()->cast_game_object() : nullptr;
		CGameObject* GO2 = nullptr;

		if (m_lastFocusRecivedItem != nullptr && m_lastFocusLostItem_id != u16(0xffff) && m_lastFocusRecivedItem->object_id() != m_lastFocusLostItem_id && m_lastFocusRecivedItem->m_pInventory != nullptr)
		{
			GO2 = m_lastFocusRecivedItem->cast_game_object();
		}

		is_on_item_dropped_callback_processed = true;

		if (GO1 != nullptr && GO2 != nullptr && GO1->H_Parent()->ID() == GO2->H_Parent()->ID())
		{
			AntigasFilter* filter = smart_cast<AntigasFilter*>(GO1);
			IAntigas* antigas = smart_cast<IAntigas*>(GO2);
			if (filter != nullptr && antigas != nullptr && antigas->IsFilterInWhiteList(GO1->cNameSect()))
			{
				if (antigas->InstallFilter(GO1->cast_inventory_item())) {
					old_owner->RemoveItem(itm, false);
					SetCurrentItem(nullptr);
					return true;
				}
			}

			PowerCell* oPowerCell = smart_cast<PowerCell*>(GO1);
			PowerBank* oPowerBank = smart_cast<PowerBank*>(GO2);
			if (PowerBank* oPowerBank = smart_cast<PowerBank*>(GO2))
			{
				if (oPowerBank->InsertPowerCell(oPowerCell))
				{
					old_owner->RemoveItem(itm, false);
					SetCurrentItem(nullptr);
					return true;
				}
			}

			IPowerManager* oPowerManager = smart_cast<IPowerManager*>(GO2);
			if (oPowerCell != nullptr && oPowerManager != nullptr)
			{
				if (oPowerManager->IstallPowerCell(oPowerCell))
				{
					old_owner->RemoveItem(itm, false);
					SetCurrentItem(nullptr);
					return true;
				}
			}
		}

		if (m_isItemDropped)
		{
			luabind::functor<bool> funct1;
			R_ASSERT2(ai().script_engine().functor(m_onItemDropped, funct1), "failed to get OnItemDropped functor");
			if (funct1(GO1 ? GO1->lua_game_object() : (0), GO2 ? GO2->lua_game_object() : (0), (int)t_old, (int)t_new) == false)
			{
				return false;
			}
		}
	}

	UpdateConditionProgressBars	();
	UpdateItemsPlace			();

	return true;
}

bool CUIActorMenuBase::OnItemDropped(PIItem itm, CUIDragDropListEx* new_owner, CUIDragDropListEx* old_owner)
{
	CUICellItem*	_citem	= (new_owner->ItemsCount()==1) ? new_owner->GetItemIdx(0) : nullptr;
	PIItem _iitem	= _citem ? (PIItem)_citem->m_pData : nullptr;

	if (!_iitem)
	{
		return false;
	}
	if (!_iitem->CanAttach(itm))
	{
		return false;
	}

	if (old_owner != GetActorList() && old_owner != GetTradeActorBagList())
	{
		return false;
	}

	AttachAddon						(_iitem);

	return							true;
}

bool CUIActorMenuBase::OnItemFocusedUpdate(CUICellItem* itm)
{
	if (itm)
	{
		itm->m_selected = true;
		if (m_currMenuMode == mmTrade)
		{
			if (!CUIDragDropListEx::m_drag_item && itm != _tradeHoverCell)
			{
				clear_highlight_lists();
				_tradeHoverCell = itm;
				PIItem pitm = (PIItem)itm->m_pData;
				if (pitm != nullptr)
				{
					set_highlight_item(itm);
				}
			}
		}
		else if (m_highlight_clear)
		{
			set_highlight_item(itm);
		}
	}

	VERIFY(m_ItemInfo);
	if (itm != nullptr && Device.dwTimeContinual < itm->FocusReceiveTime() + m_ItemInfo->delay)
	{
		return true; //false
	}
	if (CUIDragDropListEx::m_drag_item || m_UIPropertiesBox->IsShown() || !m_item_info_view)
	{
		return true;
	}

	InfoCurItem(itm);
	return true;
}

bool CUIActorMenuBase::OnItemFocusLost(CUICellItem* itm)
{
	_tradeHoverCell = nullptr;

	if ( itm )
	{
		if (itm->HasValidInventoryBinding())
		{
			if (PIItem iItm = (PIItem)itm->m_pData)
			{
				m_lastFocusLostItem_id = iItm->object_id();
			}
		}
		itm->m_selected = false;
	}
	else 
	{
		m_lastFocusLostItem_id = u16(0xffff);
	}

	InfoCurItem( nullptr );
	clear_highlight_lists();

	if (m_isItemFocusLost)
	{
		luabind::functor<bool> funct1;
		if (ai().script_engine().functor(m_onItemFocusLost, funct1))
		{
			PIItem _iitem = (itm != nullptr && itm->HasValidInventoryBinding())
				? (PIItem)itm->m_pData
				: nullptr;

			if (CGameObject* GO = _iitem ? _iitem->cast_game_object() : nullptr)
			{
				funct1(GO->lua_game_object());
			}
		}
	}

	return true;
}

bool CUIActorMenuBase::OnItemStartDrag(CUICellItem* itm)
{
	if (m_currMenuMode == mmTrade)
	{
		_tradeHoverCell = nullptr;
		clear_highlight_lists();
	}
	InfoCurItem( nullptr );
	return false; //default behaviour
}

bool CUIActorMenuBase::OnItemRButtonClick(CUICellItem* itm)
{
	SetCurrentItem( itm );
	InfoCurItem( nullptr );
	ActivatePropertiesBox();
	m_item_info_view = false;
	return false;
}

bool CUIActorMenuBase::OnItemSelected(CUICellItem* itm)
{
	SetCurrentItem		(itm);
	InfoCurItem			(nullptr);
	m_item_info_view	= false;
	return				false;
}

bool CUIActorMenuBase::OnItemFocusReceive(CUICellItem* itm)
{
	InfoCurItem( nullptr );
	m_item_info_view = true;

	itm->m_selected = true;
	if (m_currMenuMode != mmTrade)
	{
		set_highlight_item( itm );
	}

	m_lastFocusRecivedItem = (PIItem)itm->m_pData;
	m_cell_lastFocusRecivedItem = itm;
	if (m_isItemFocusReceive)
	{
		luabind::functor<bool> funct1;
		R_ASSERT2(ai().script_engine().functor(m_onItemFocusReceive, funct1), "failed to get OnItemFocusReceive functor");
		PIItem _iitem = (PIItem)itm->m_pData;

		CGameObject* GO = _iitem ? _iitem->cast_game_object() : nullptr;
		if (GO)
			funct1(GO->lua_game_object());
		
	}

	return true;
}

bool CUIActorMenuBase::OnItemDbClick(CUICellItem* itm)
{
	SetCurrentItem(itm);
	InfoCurItem( nullptr );
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	EDDListType t_old					= GetListType(old_owner);
	bool bItemPack = itm->ChildsCount() > 0;

	switch ( t_old )
	{
	case iActorSlot:
		{
			if (m_currMenuMode == mmDeadBodySearch) 
			{
				// FFx0001
				if (IsAllowPlaceToInvBox(itm)) 
				{
					bool bResult = ToDeadBodyBag(itm, false);
					if (pInput->GetControllerMode() && bResult)
						SetCurrentItem(nullptr);
				}
			}
			else 
			{
				if (pInput->GetControllerMode() && m_currMenuMode == mmUpgrade)
				{
					PIItem pItem = CurrentIItem();
					if (CanUpgradeItem(pItem))
					{
						SetAuxMode(eActorMenuControllerAuxMode::eAuxMode_Upgrade);
					}
				}
				// FFx0001
				else if (IsAllowTakeFromInvBox(itm)) 
				{
					bool bResult = ToBag(itm, false);
					if (pInput->GetControllerMode() && bResult)
						SetCurrentItem(nullptr);
				}
				else if (m_currMenuMode == mmInventory && !bItemPack && TryHolsterPistolHolsterSlotDbClick(itm))
				{
					if (pInput->GetControllerMode())
						SetCurrentItem(nullptr);
				}
			}
			break;
		}
	case iActorBag:
		{
			if (pInput->GetControllerMode() && m_currMenuMode == mmUpgrade)
			{
				PIItem pItem = CurrentIItem();
				if (CanUpgradeItem(pItem))
				{
					SetAuxMode(eActorMenuControllerAuxMode::eAuxMode_Upgrade);
				}
				break;
			}
			if ( m_currMenuMode == mmTrade )
			{
				bool bResult = ToActorTrade( itm, false );
				if (pInput->GetControllerMode() && bResult)
					SetCurrentItem(nullptr);
				break;
			}
			else
				if ( m_currMenuMode == mmDeadBodySearch )
				{
					bool bResult = ToDeadBodyBag( itm, false );
					if (pInput->GetControllerMode() && bResult && !bItemPack)
						SetCurrentItem(nullptr);
					break;
				}
				if(m_currMenuMode!=mmUpgrade && TryUseItem( itm ))
				{
					if (pInput->GetControllerMode() && !bItemPack)
						SetCurrentItem(nullptr);
					break;
				}
				if (!bItemPack && TryHolsterPistolBagDbClick(itm))
				{
					if (pInput->GetControllerMode())
						SetCurrentItem(nullptr);
					break;
				}
				if ( TryActiveSlot( itm ) )
				{
					break;
				}
				PIItem iitem_to_place = (PIItem)itm->m_pData;
				if ( !ToSlot( itm, false, iitem_to_place->BaseSlot() ) )
				{
					if ( !ToBelt( itm, false ) )
					{
						bool bResult = ToSlot( itm, true, iitem_to_place->BaseSlot() );
						if (pInput->GetControllerMode() && bResult)
							SetCurrentItem(nullptr);
					}
					else
					{
						if (pInput->GetControllerMode())
							SetCurrentItem(nullptr);
					}
				}
				else
				{
					if (pInput->GetControllerMode())
						SetCurrentItem(nullptr);
				}
				break;
		}
	case iActorBelt:
		{
			bool bResult = ToBag( itm, false );
			if (pInput->GetControllerMode() && bResult)
				SetCurrentItem(nullptr);
			break;
		}
	case iActorTrade:
		{
			bool bResult = ToBag( itm, false );
			if (pInput->GetControllerMode() && bResult && !bItemPack)
				SetCurrentItem(nullptr);
			break;
		}
	case iPartnerTradeBag:
		{
			bool bResult = ToPartnerTrade( itm, false );
			if (pInput->GetControllerMode() && bResult && !bItemPack)
				SetCurrentItem(nullptr);
			break;
		}
	case iPartnerTrade:
		{
			bool bResult = ToPartnerTradeBag( itm, false );
			if (pInput->GetControllerMode() && bResult && !bItemPack)
				SetCurrentItem(nullptr);
			break;
		}
	case iDeadBodyBag:
		{
			bool bResult = ToBag( itm, false );
			if (pInput->GetControllerMode() && bResult && !bItemPack)
				SetCurrentItem(nullptr);
			break;
		}
	case iQuickSlot:
		{
			if (!pInput->GetControllerMode())
				ToQuickSlot(itm);
		}break;

	}; //switch 

	UpdateConditionProgressBars();
	UpdateItemsPlace();

	return true;
}

void CUIActorMenuBase::BindDragDropListEvents(CUIDragDropListEx* lst)
{
	if (lst == nullptr)
		return;

	lst->m_f_item_drop				= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemDrop);
	lst->m_f_item_start_drag		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemStartDrag);
	lst->m_f_item_db_click			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemDbClick);
	lst->m_f_item_selected			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemSelected);
	lst->m_f_item_rbutton_click		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemRButtonClick);
	lst->m_f_item_focus_received	= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemFocusReceive);
	lst->m_f_item_focus_lost		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemFocusLost);
	lst->m_f_item_focused_update	= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIActorMenuBase::OnItemFocusedUpdate);
	lst->m_f_content_reset			= CUIDragDropListEx::CONTENT_RESET_EVENT(this, &CUIActorMenuBase::OnDragDropListContentReset);
}

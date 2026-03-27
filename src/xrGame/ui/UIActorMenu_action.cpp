////////////////////////////////////////////////////////////////////////////
//	Module 		: UIActorMenu_action.cpp
//	Created 	: 14.10.2008
//	Author		: Evgeniy Sokolov (sea)
//	Description : UI ActorMenu actions implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UIActorMenu.h"
#include "UIActorStateInfo.h"
#include "../Actor.h"
#include "UIGameSP.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../InventoryBox.h"
#include "object_broker.h"
#include "UIInventoryUtilities.h"
#include "game_cl_base.h"
#include "../../xrEngine/xr_input.h"
#include "UITalkWnd.h"
#include "UITalkDialogWnd.h"
#include "../../xrUI/UICursor.h"
#include "UICellItem.h"
#include "UICharacterInfo.h"
#include "UIItemInfo.h"
#include "UIDragDropListEx.h"
#include "UIInventoryUpgradeWnd.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UIMessageBoxEx.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "UIMainIngameWnd.h"
#include "UITalkWnd.h"
#include "UITalkDialogWnd.h"
#include "UIInvUpgradeInfo.h"
#include "antigas.h"
#include "antigas_filter.h"

#include "PowerBank.h"
#include "PowerCell.h"
#include "IPowerManager.h"

class CUITrashIcon :public ICustomDrawDragItem
{
	CUIStatic			m_icon;
public:
	CUITrashIcon		()
	{
		m_icon.SetWndSize		(Fvector2().set(29.0f*UI().get_current_kx(), 36.0f));
		m_icon.SetStretchTexture(true);
//		m_icon.SetAlignment		(waCenter);
		m_icon.InitTexture		("ui_inGame2_inv_trash");
	}
	virtual void		OnDraw		(CUIDragItem* drag_item)
	{
		Fvector2 pos			= drag_item->GetWndPos();
		Fvector2 icon_sz		= m_icon.GetWndSize();
		Fvector2 drag_sz		= drag_item->GetWndSize();

		pos.x			-= icon_sz.x;
		pos.y			+= drag_sz.y;

		m_icon.SetWndPos(pos);
//		m_icon.SetWndSize(sz);
		m_icon.Draw		();
	}

};
void CUIActorMenu::OnDragItemOnTrash(CUIDragItem* item, bool b_receive)
{
	if(b_receive && !CurrentIItem()->IsQuestItem())
		item->SetCustomDraw(new CUITrashIcon());
	else
		item->SetCustomDraw(nullptr);
}

bool CUIActorMenu::OnMouseAction( float x, float y, EUIMessages mouse_action )
{
	inherited::OnMouseAction( x, y, mouse_action );
	return true; // no click`s
}

bool CUIActorMenu::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	InfoCurItem( nullptr );
	if ( is_binded(kDROP, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action && CurrentIItem() && !CurrentIItem()->IsQuestItem()
			&& CurrentIItem()->parent_id()==m_pActorInvOwner->object_id() )
		{

			SendEvent_Item_Drop		(CurrentIItem(), m_pActorInvOwner->object_id());
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

	if ( is_binded(kUSE, dik) || is_binded(kINVENTORY, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			g_btnHint->Discard();
			HideDialog();

			if (m_pActorInvOwner->IsTalking())
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

			if (m_pActorInvOwner->IsTalking())
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

void CUIActorMenu::MoveSelector(eUIDirection4 dir, bool bAllowAreaExit)
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

void CUIActorMenu::UpdateInfoWindowVisibility()
{
	if (m_AuxMode == eAuxMode_Upgrade)
	{
		m_pUpgradeWnd->SetInfoVisible(m_bShowInfoWnds);
		return;
	}
	
	// For grid item/slots
	CUIDragDropListEx* pDragDropList = dynamic_cast<CUIDragDropListEx*>(m_ui_navigation_selection);
	if (pDragDropList)
	{
		if (!m_bShowInfoWnds)
		{
			if (m_ItemInfo->IsEnabled())
				InfoCurItem(nullptr);
		}
		else
		{
			if (!CUIDragDropListEx::m_drag_item && !m_UIPropertiesBox->IsShown())
			{
				CUICellItem* pCellItem = pDragDropList->GetSelectedItem();
				if (pCellItem && ((PIItem)pCellItem->m_pData) == m_ItemInfo->CurrentItem())
					return;
				
				InfoCurItem(pCellItem);
			}
		}
	}
}


bool CUIActorMenu::AnyInfoWindowOpen() const
{
	if (m_ItemInfo && m_ItemInfo->CurrentItem())
		return true;
	if (m_upgrade_info && m_upgrade_info->get_upgrade())
		return true;

	return false;
}

void CUIActorMenu::OnPressUserKey()
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
		if (pDragDropList && pDragDropList == m_pInventoryBagList && pInput->GetControllerMode())
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

void CUIActorMenu::OnBtnExitClicked(CUIWindow* w, void* d)
{
	g_btnHint->Discard();
	HideDialog();

	if (m_pActorInvOwner->IsTalking())
		CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
}

void CUIActorMenu::OnMesBoxYes( CUIWindow*, void* )
{
	switch( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		break;
	case mmTrade:
		break;
	case mmUpgrade:
		if (m_repair_mode == 1)
		{
			RepairEffect_CurItem();
			m_repair_mode = 0;
		}
		else if (m_repair_mode == 2)
		{
			PerformDisassemble();
			m_repair_mode = 0;
		}
		else
		{
			m_pUpgradeWnd->OnMesBoxYes();
		}
		break;
	case mmDeadBodySearch:
		break;
	default:
		R_ASSERT(0);
		break;
	}
	UpdateItemsPlace();
}

void CUIActorMenu::OnMesBoxNo(CUIWindow*, void*)
{
	switch(m_currMenuMode)
	{
	case mmUndefined:
		break;
	case mmInventory:
		break;
	case mmTrade:
		break;
	case mmUpgrade:
		m_repair_mode = 0;
		break;
	case mmDeadBodySearch:
		break;
	default:
		R_ASSERT(0);
		break;
	}
	UpdateItemsPlace();
}

bool CUIActorMenu::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
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

			if (m_pActorInvOwner->IsTalking())
				CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
			return true;
		}
	}
	return false;
}

bool CUIActorMenu::OnGamepadKeyHold(int id)
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
		}
	}

	return false;
}

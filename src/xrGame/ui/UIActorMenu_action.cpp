////////////////////////////////////////////////////////////////////////////
//	Module 		: UIActorMenu_action.cpp
//	Created 	: 14.10.2008
//	Author		: Evgeniy Sokolov (sea)
//	Description : UI ActorMenu actions implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
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

#include "StdAfx.h"
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

void CUIInventoryWnd::SetCurrentItem(CUICellItem* itm)
{
	m_pCurrentCellItem				= itm;
	m_ItemInfo->InitItem			(CurrentItem(), nullptr, CurrentIItem() ? CurrentIItem()->Cost() : u32(-1));
	TryHidePropertiesBox();
}

void CUIInventoryWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if (pWnd == m_UIPropertiesBox && msg == PROPERTY_CLICKED)
	{
		ProcessPropertiesBoxClicked(this, nullptr);
	}
	else if (UIExitButton == pWnd && BUTTON_CLICKED == msg)
	{
		HideDialog();
	}

	inherited::SendMessage(pWnd, msg, pData);
}


void CUIInventoryWnd::InitInventory_delayed()
{
	m_b_need_reinit = true;
}

void CUIInventoryWnd::InitInventory()
{
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	if (!pInvOwner)
	{
		return;
	}

	m_pInvOwner					= pInvOwner;

	InitInventoryContents		(GetActorList());

	InventoryUtilities::UpdateWeight					(UIBagWnd, true);
	if (m_pInvList[OUTFIT_SLOT])
	{
		if (CUIOutfitDragDropList* outfitSlot = smart_cast<CUIOutfitDragDropList*>(m_pInvList[OUTFIT_SLOT]))
		{
			outfitSlot->SetOutfit();
		}
	}

	SetAreaSelectionTo				(m_pUIBagList);
	m_b_need_reinit					= false;
}  

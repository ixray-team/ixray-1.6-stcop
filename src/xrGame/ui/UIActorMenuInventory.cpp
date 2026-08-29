#include "StdAfx.h"
#include "UIActorMenu.h"
#include "../Inventory.h"
#include "../InventoryOwner.h"
#include "UIInventoryUtilities.h"
#include "UIItemInfo.h"
#include "../Level.h"
#include "UICellItemFactory.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UICellCustomItems.h"
#include "UIItemInfo.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "../../xrUI/Widgets/UIListBoxItem.h"
#include "UIMainIngameWnd.h"
#include "UIGameCustom.h"
#include "eatable_item_object.h"
#include "../../xrEngine/xr_input.h"
#include "../Silencer.h"
#include "../Scope.h"
#include "../GrenadeLauncher.h"
#include "../Artefact.h"
#include "../eatable_item.h"
#include "../BottleItem.h"
#include "../WeaponMagazined.h"
#include "../medkit.h"
#include "../antirad.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../../xrUI/UICursor.h"
#include "../MPPlayersBag.h"
#include "../player_hud.h"
#include "../CustomDevice.h"
#include "../PDA.h"

#include "../actor_defs.h"
#include "../InventoryBox.h"
#include "InventorySorter.h"

#include "../game_sv_single.h"
#include "ai_object_location.h"
#include "ui_drop_amount.h"
#include "PowerBank.h"
#include "nvg.h"

using namespace luabind; //Alundaio

void CUIActorMenu::InitInventoryMode()
{
	m_pInventoryBagList->Show(true);
	m_pInventoryBeltList->Show(true);

	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		if (m_pInvList[i])
			m_pInvList[i]->Show(true);
	}
	if (m_pQuickSlot)
	m_pQuickSlot->Show(true);
	m_pTrashList->Show(true);
	m_RightDelimiter->Show(false);
	if (m_clock_value)
		m_clock_value->Show(true);

	InitInventoryContents(m_pInventoryBagList);

	SetAreaSelectionTo(m_pInventoryBagList);

	VERIFY(CurrentGameUI());
	CurrentGameUI()->UIMainIngameWnd->ShowZoneMap(true);
}

void CUIActorMenu::DeInitInventoryMode()
{
	m_pTrashList->Show(false);
	if (m_clock_value)
		m_clock_value->Show(false);
	clear_highlight_lists();
}

bool CUIActorMenu::DropAllItemsFromRuck( bool quest_force )
{
	if ( !IsShown() || !m_pInventoryBagList || m_currMenuMode != mmInventory )
	{
		return false;
	}

	u32 const ci_count = m_pInventoryBagList->ItemsCount();
	for ( u32 i = 0; i < ci_count; ++i )
	{
		CUICellItem* ci = m_pInventoryBagList->GetItemIdx( i );
		VERIFY( ci );
		PIItem item = (PIItem)ci->m_pData;
		VERIFY( item );

		if ( !quest_force && item->IsQuestItem() )
		{
			continue;
		}

		u32 const cnt = ci->ChildsCount();
		for( u32 j = 0; j < cnt; ++j )
		{
			CUICellItem*	child_ci   = ci->PopChild(nullptr);
			PIItem			child_item = (PIItem)child_ci->m_pData;
			SendEvent_Item_Drop( child_item, m_pActorInvOwner->object_id() );
		}
		SendEvent_Item_Drop( item, m_pActorInvOwner->object_id() );
	}

	SetCurrentItem( nullptr );
	return true;
}

bool CUIActorMenu::ToSlotScript(CScriptGameObject* GO, bool force_place, u16 slot_id)
{
	CInventoryItem* iitem = GO->object().dcast_CObject()->cast_inventory_item();

	if (!iitem || !m_pActorInvOwner->inventory().InRuck(iitem))
		return false;

	CUIDragDropListEx* invlist = GetListByType(iActorBag);
	CUICellContainer* c = invlist->GetContainer();

	xrCriticalSectionGuard guard(c->csUi);
	CUIWindow::WINDOW_LIST& child_list = c->GetChildWndList();

	for (WINDOW_LIST_it it = child_list.begin(); child_list.end() != it; ++it)
	{
		CUICellItem* i = (CUICellItem*)(*it);
		PIItem	pitm = (PIItem)i->m_pData;
		if (pitm == iitem)
		{
			ToSlot(i, force_place, slot_id);
			return true;
		}
	}
	return false;
}

bool CUIActorMenu::ToBeltScript(CScriptGameObject* GO, bool b_use_cursor_pos)
{
	CInventoryItem* iitem = GO->object().dcast_CObject()->cast_inventory_item();

	if (!iitem || !m_pActorInvOwner->inventory().InRuck(iitem))
		return false;

	CUIDragDropListEx* invlist = GetListByType(iActorBag);
	CUICellContainer* c = invlist->GetContainer();

	xrCriticalSectionGuard guard(c->csUi);
	CUIWindow::WINDOW_LIST& child_list = c->GetChildWndList();

	for (WINDOW_LIST_it it = child_list.begin(); child_list.end() != it; ++it)
	{
		CUICellItem* i = (CUICellItem*)(*it);
		PIItem	pitm = (PIItem)i->m_pData;
		if (pitm == iitem)
		{
			ToBelt(i, b_use_cursor_pos);
			return true;
		}
	}
	return false;
}

void CUIActorMenu::UpdateOutfit()
{
	for ( u8 i = 0; i < m_ArtefactSlotsCount; ++i )
	{
		m_belt_list_over[i]->SetVisible( true );
	}

	u32 af_count = m_pActorInvOwner->inventory().BeltWidth();
	//VERIFY( 0 <= af_count && af_count <= 5 );

	VERIFY( m_pInventoryBeltList );
	CCustomOutfit* outfit    = m_pActorInvOwner->GetOutfit();
	if(outfit && !outfit->bIsHelmetAvaliable && m_HelmetOver)
		m_HelmetOver->Show(true);
	else if (m_HelmetOver)
		m_HelmetOver->Show(false);

	if ( !outfit )
	{
		MoveArtefactsToBag();
		return;
	}

	Ivector2 afc;
	afc.x = m_pInventoryBeltList->CellsCapacity().x;
	afc.y = m_pInventoryBeltList->CellsCapacity().y;

	m_pInventoryBeltList->SetCellsCapacity( afc );

	for ( u8 i = 0; i < af_count; ++i )
	{
		m_belt_list_over[i]->SetVisible( false );
	}
}

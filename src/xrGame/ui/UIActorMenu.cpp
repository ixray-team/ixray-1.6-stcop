#include "StdAfx.h"
#include "UIActorMenu.h"
#include "UIActorStateInfo.h"
#include "../Actor.h"
#include "UIGameSP.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../InventoryBox.h"
#include "object_broker.h"
#include "../ai/monsters/basemonster/base_monster.h"
#include "UIInventoryUtilities.h"
#include "game_cl_base.h"
#include "UITalkDialogWnd.h"
#include "../Weapon.h"
#include "../WeaponKnife.h"
#include "../WeaponBinoculars.h"
#include "../WeaponMagazinedWGrenade.h"
#include "../WeaponAmmo.h"
#include "../Silencer.h"
#include "../Scope.h"
#include "../GrenadeLauncher.h"
#include "../trade_parameters.h"
#include "../ActorHelmet.h"
#include "../CustomOutfit.h"
#include "../eatable_item.h"
#include "UITalkWnd.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIItemStateDisplay.h"
#include "../../xrUI/UICursor.h"
#include "UICellItem.h"
#include "UICharacterInfo.h"
#include "UIItemInfo.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UIInventoryUpgradeWnd.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UIMessageBoxEx.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "UIMainIngameWnd.h"
#include "../trade.h"
#include "Car.h"
#include "../../xrEngine/string_table.h"
#include "InventorySorter.h"
#include "../../xrEngine/xr_input.h"
#include "UIInvUpgradeInfo.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"
#include "ui_drop_amount.h"

void CUIActorMenu::OnSortTabChanged(CUIWindow* w, void* pData)
{
    if (!m_pInventorySorter)
    {
        return;
    }

    if (m_currMenuMode == mmUndefined)
    {
        return;
    }

    CUITabControl* tabControl = smart_cast<CUITabControl*>(w);
    if (!tabControl)
    {
        return;
    }

    const ESortTabsLayoutSlot sortSlot = GetSortTabsSlotByWindow(w);
    if (sortSlot == eSortTabsLayoutCount)
    {
        return;
    }

    m_sortCategoryId[sortSlot] = tabControl->GetActiveId();
    if (!m_sortCategoryId[sortSlot].size())
    {
        m_sortCategory[sortSlot] = EInventorySortCategory::All;
    }
    else
    {
        m_sortCategory[sortSlot] = m_pInventorySorter->GetCategoryById(m_sortCategoryId[sortSlot]);
    }

    ApplySortForSlot(sortSlot);
}

void CUIActorMenu::ApplySortForSlot(ESortTabsLayoutSlot sortSlot)
{
    switch (sortSlot)
    {
    case eSortTabsInventory:
    case eSortTabsUpgrade:
    {
        UpdateActorBagList();
        break;
    }
    case eSortTabsTradeActor:
    {
        if (m_currMenuMode == mmTrade)
        {
            UpdateTradeActorBagList();
        }
        else
        {
            UpdateActorBagList();
        }
        break;
    }
    case eSortTabsTradePartner:
    {
        UpdateTradePartnerBagList();
        break;
    }
    case eSortTabsDeadBody:
    {
        UpdateDeadBodyBagList();
        break;
    }
    default:
    {
        break;
    }
    }
}

void CUIActorMenu::SetActor(CInventoryOwner* io)
{
	R_ASSERT			(!IsShown());
	m_last_time			= Device.dwTimeGlobal;
	m_pActorInvOwner	= io;
	
	if (IsGameTypeSingle())
	{
		if (io)
			m_ActorCharacterInfo->InitCharacter(m_pActorInvOwner);
		else
			m_ActorCharacterInfo->ClearInfo();
	}
	else
	{
		SetActorInfoMP();
	}
}

void CUIActorMenu::ReloadActorInfo()
{
	if (m_pActorInvOwner != nullptr)
	{
		m_ActorCharacterInfo->ClearInfo();
		m_ActorCharacterInfo->InitCharacter(m_pActorInvOwner);
	}
}

void CUIActorMenu::SetPartner(CInventoryOwner* io)
{
	R_ASSERT(!IsShown());
	m_pPartnerInvOwner = io;

	if (m_pPartnerInvOwner != nullptr)
	{
		CBaseMonster* pMonster = m_pPartnerInvOwner->cast_base_monster();

		if (pMonster != nullptr || m_pPartnerInvOwner->use_simplified_visual())
		{
			m_PartnerCharacterInfo->ClearInfo();

			if (pMonster != nullptr)
			{
				const char* icon = "npc_icon_unknown_data";

				if (pSettings->line_exist(pMonster->cNameSect(), "icon"))
				{
					icon = pSettings->r_string(pMonster->cNameSect(), "icon");
				}

				m_PartnerCharacterInfo->InitCharacter("", icon);
			}
		}
		else if (CCar* pCar = m_pPartnerInvOwner->cast_car())
		{
			if (pSettings->line_exist(pCar->cNameSect(), "icon"))
			{
				shared_str Name = "";

				if (pSettings->line_exist(pCar->cNameSect(), "icon"))
				{
					Name = pSettings->r_string(pCar->cNameSect(), "name");
					Name = g_pStringTable->translate(Name);
				}

				m_PartnerCharacterInfo->InitCharacter
				(
					Name.c_str(),
					pSettings->r_string(pCar->cNameSect(), "icon")
				);
			}
		}
		else
		{
			m_PartnerCharacterInfo->InitCharacter(m_pPartnerInvOwner);
		}

		SetInvBox(nullptr);
	}
	else
	{
		m_PartnerCharacterInfo->ClearInfo();
	}
}

void CUIActorMenu::SetInvBox(CInventoryBox* box)
{
	R_ASSERT			(!IsShown());
	m_pInvBox = box;
	if ( box )
	{
		m_pInvBox->set_in_use( true );
		SetPartner( nullptr );
	}
}

void CUIActorMenu::SetMenuMode(EMenuMode mode)
{
	if (!pInput->GetControllerMode())
	{
		SetCurrentItem(nullptr);
	}
	m_hint_wnd->set_text( nullptr );
	
	if ( mode != m_currMenuMode )
	{
		switch(m_currMenuMode)
		{
		case mmUndefined:
			break;
		case mmInventory:
			DeInitInventoryMode();
			break;
		case mmTrade:
			DeInitTradeMode();
			break;
		case mmUpgrade:
			DeInitUpgradeMode();
			break;
		case mmDeadBodySearch:
			DeInitDeadBodySearchMode();
			break;
		default:
			R_ASSERT(0);
			break;
		}

		CurrentGameUI()->UIMainIngameWnd->ShowZoneMap(false);

		m_currMenuMode = mode;
		switch(mode)
		{
		case mmUndefined:
#ifdef DEBUG
			Msg("* now is Undefined mode");
#endif // #ifdef DEBUG
			ResetMode();
			break;
		case mmInventory:
			InitInventoryMode();
#ifdef DEBUG
			Msg("* now is Inventory mode");
#endif // #ifdef DEBUG
			break;
		case mmTrade:
			InitTradeMode();
#ifdef DEBUG
			Msg("* now is Trade mode");
#endif // #ifdef DEBUG
			break;
		case mmUpgrade:
			InitUpgradeMode();
#ifdef DEBUG
			Msg("* now is Upgrade mode");
#endif // #ifdef DEBUG
			break;
		case mmDeadBodySearch:
			InitDeadBodySearchMode();
#ifdef DEBUG
			Msg("* now is DeadBodySearch mode");
#endif // #ifdef DEBUG
			break;
		default:
			R_ASSERT(0);
			break;
		}
		UpdateConditionProgressBars();
		CurModeToScript();
	}//if

	if ( m_pActorInvOwner )
	{
		UpdateOutfit();
		UpdateActor();
	}
	UpdateButtonsLayout();

	ShowSortTabsForCurrentMode();
}

void CUIActorMenu::UpdateSortTabsLayout()
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

void CUIActorMenu::ShowSortTabsForCurrentMode()
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

CUIActorMenu::ESortTabsLayoutSlot CUIActorMenu::GetSortTabsSlotByWindow(CUIWindow* window) const
{
	for (u8 i = 0; i < eSortTabsLayoutCount; ++i)
	{
		if (m_sortTabControl[i] == window)
		{
			return (ESortTabsLayoutSlot)i;
		}
	}

	return eSortTabsLayoutCount;
}

void CUIActorMenu::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent		(pWnd, msg, pData);
}

void CUIActorMenu::Show(bool status)
{
	inherited::Show							(status);
	if(status)
	{
		SetMenuMode							(m_currMenuMode);
		PlaySnd								(eSndOpen);
		m_ActorStateInfo->UpdateActorInfo	(m_pActorInvOwner);

		if (pInput->GetControllerMode())
		{
			m_bShowInfoWnds = false;
		}
	}else
	{
		PlaySnd								(eSndClose);
		SetMenuMode							(mmUndefined);
	}
	m_ActorStateInfo->Show					(status);
}

void CUIActorMenu::Draw()
{
	//CurrentGameUI()->UIMainIngameWnd->DrawZoneMap();
	//CurrentGameUI()->UIMainIngameWnd->DrawMainIndicatorsForInventory();

	inherited::Draw	();
	m_ItemInfo->Draw();
	m_hint_wnd->Draw();
}

void CUIActorMenu::CheckSelectors()
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

void CUIActorMenu::Update()
{	
	if (pInput->GetControllerMode())
	{
		CheckSelectors();
		UpdateInfoWindowVisibility();
	}

	{ // all mode
		m_last_time = Device.dwTimeGlobal;
		m_ActorStateInfo->UpdateActorInfo( m_pActorInvOwner );

		if (!IsGameTypeSingle())
			UpdateActorMoneyMP();
	}

	switch ( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		{
			UpdateConditionProgressBars();

			if (m_clock_value)
			{
				m_clock_value->SetText(InventoryUtilities::GetGameTimeAsString(
			    InventoryUtilities::etpTimeToMinutes).c_str());
			}
			CurrentGameUI()->UIMainIngameWnd->UpdateZoneMap();
			break;
		}
	case mmTrade:
		{
			if(m_pPartnerInvOwner->inventory().ModifyFrame() != m_trade_partner_inventory_state)
				InitPartnerInventoryContents	();
			CheckDistance					();

			if (m_trade_button)
			{
				m_trade_button->Show(!pInput->GetControllerMode());
			}
			if (m_trade_buy_button)
			{
				m_trade_buy_button->Show(!pInput->GetControllerMode());
			}
			if (m_trade_sell_button)
			{
				m_trade_sell_button->Show(!pInput->GetControllerMode());
			}
			break;
		}
	case mmUpgrade:
		{
			UpdateUpgradeItem();
			CheckDistance();
			break;
		}
	case mmDeadBodySearch:
		{
			m_takeall_button->Show(!pInput->GetControllerMode());
			if (m_putall_button)
				m_putall_button->Show(!pInput->GetControllerMode());
			break;
		}
	default: R_ASSERT(0); break;
	}
	
	inherited::Update();
	m_ItemInfo->Update();
	m_hint_wnd->Update();
	m_exit_button->Show(!pInput->GetControllerMode());
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

bool CUIActorMenu::StopAnyMove()  // true = актёр не идёт при открытом меню
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

void CUIActorMenu::CheckDistance()
{
	CGameObject* pActorGO = m_pActorInvOwner ? m_pActorInvOwner->cast_game_object() : nullptr;
	CGameObject* pPartnerGO	= m_pPartnerInvOwner ? m_pPartnerInvOwner->cast_game_object() : nullptr;
	CGameObject* pBoxGO	= m_pInvBox ? m_pInvBox->cast_game_object() : nullptr;
	VERIFY(pActorGO && (pPartnerGO || pBoxGO));

	if (pPartnerGO)
	{
		if ((pActorGO->Position().distance_to(pPartnerGO->Position()) > 3.0f) && !m_pPartnerInvOwner->NeedOsoznanieMode())
		{
			g_btnHint->Discard();
			HideDialog();

			if (m_pActorInvOwner->IsTalking())
				CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
		}
	}
	else if (pBoxGO)
	{
		if (pActorGO->Position().distance_to( pBoxGO->Position() ) > 3.0f)
		{
			g_btnHint->Discard();
			HideDialog();

			if (m_pActorInvOwner->IsTalking())
				CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
		}
	}
}

void CUIActorMenu::SetCurrentItem(CUICellItem* itm)
{
	if (pInput->GetControllerMode())
	{
		if (m_pCurrentCellItem)
			OnItemFocusLost(m_pCurrentCellItem);
		if (!itm)
			m_bShowInfoWnds = false;
	}
	m_repair_mode = 0;
	m_pCurrentCellItem = itm;
	if ( !itm )
	{
		InfoCurItem( nullptr );
	}
	else
	{
		if (pInput->GetControllerMode())
		{
			OnItemFocusReceive(itm);
			set_highlight_item(itm);
		}
	}
	TryHidePropertiesBox();

	if ( m_currMenuMode == mmUpgrade )
	{
		SetupUpgradeItem();
	}
}

void CUIActorMenu::InfoCurItem( CUICellItem* cell_item )
{
	if ( !cell_item )
	{
		m_ItemInfo->InitItem( nullptr );
		return;
	}
	PIItem current_item = (PIItem)cell_item->m_pData;

	PIItem compare_item = nullptr;
	u16    compare_slot = current_item->BaseSlot();
	if ( compare_slot != NO_ACTIVE_SLOT )
	{
		compare_item = m_pActorInvOwner->inventory().ItemFromSlot(compare_slot);
	}

	if(GetMenuMode()==mmTrade)
	{
		CInventoryOwner* item_owner = current_item->m_pInventory->GetOwner() ? current_item->m_pInventory->GetOwner()->cast_inventory_owner() : nullptr;
		u32 item_price = u32(-1);
		if(item_owner && item_owner==m_pActorInvOwner)
			item_price = m_partner_trade->GetItemPrice(current_item, true);
		else
			item_price = m_partner_trade->GetItemPrice(current_item, false);

		//if(item_price>500)
		//	item_price = iFloor(item_price/10+0.5f)*10;

		CWeaponAmmo* ammo = current_item->cast_weapon_ammo();
		if(ammo)
		{
			for( u32 j = 0; j < cell_item->ChildsCount(); ++j )
			{
				u32 tmp_price	= 0;
				PIItem jitem	= (PIItem)cell_item->Child(j)->m_pData;
				CInventoryOwner* ammo_owner = jitem->m_pInventory->GetOwner() ? jitem->m_pInventory->GetOwner()->cast_inventory_owner() : nullptr;
				if(ammo_owner && ammo_owner==m_pActorInvOwner)
					tmp_price = m_partner_trade->GetItemPrice(jitem, true);
				else
					tmp_price = m_partner_trade->GetItemPrice(jitem, false);

				//if(tmp_price>500)
				//	tmp_price = iFloor(tmp_price/10+0.5f)*10;

				item_price		+= tmp_price;
			}
		}

		if(	!current_item->CanTrade() || 
			(!m_pPartnerInvOwner->trade_parameters().enabled(CTradeParameters::action_buy(0), 
															current_item->object().cNameSect()) &&
			item_owner && item_owner==m_pActorInvOwner)
		)
			m_ItemInfo->InitItem	( cell_item, compare_item, u32(-1), "st_no_trade_tip_1" );
		else if(item_owner && item_owner==m_pActorInvOwner &&
				current_item->GetCondition() < m_pPartnerInvOwner->trade_parameters().buy_item_condition_factor)
			m_ItemInfo->InitItem	( cell_item, compare_item, u32(-1), "st_no_trade_tip_2" );
		else
			m_ItemInfo->InitItem	( cell_item, compare_item, item_price );
	}
	else
		m_ItemInfo->InitItem	( cell_item, compare_item, u32(-1));

//	m_ItemInfo->InitItem	( current_item, compare_item );
	float dx_pos = GetWndRect().left;
	if (!pInput->GetControllerMode())
		fit_in_rect(m_ItemInfo, Frect().set( 0.0f, 0.0f, UI_BASE_WIDTH - dx_pos, UI_BASE_HEIGHT ), 10.0f, dx_pos );
	else
	{
		Frect stickToRect;
		cell_item->GetAbsoluteRect(stickToRect);
		const float border = 10.0f;
		fit_infownd_in_rect(m_ItemInfo, stickToRect, Frect().set(0, 0, UI_BASE_WIDTH - dx_pos, UI_BASE_HEIGHT), border, dx_pos);
	}
}

void CUIActorMenu::CallMessageBoxYesNo( LPCSTR text )
{
	m_message_box_yes_no->SetText( text );
	m_message_box_yes_no->func_on_ok = CUIWndCallback::void_function( this, &CUIActorMenu::OnMesBoxYes );
	m_message_box_yes_no->func_on_no = CUIWndCallback::void_function( this, &CUIActorMenu::OnMesBoxNo );
	m_message_box_yes_no->ShowDialog(false);
}

void CUIActorMenu::CallMessageBoxOK( LPCSTR text )
{
	m_message_box_ok->SetText( text );
	m_message_box_ok->ShowDialog(false);
}

void CUIActorMenu::ResetMode()
{
	ClearAllLists();
	m_pMouseCapturer = nullptr;
	m_UIPropertiesBox->Hide();
	SetCurrentItem(nullptr);
	SetAreaSelectionTo(nullptr);

	for (u8 i = 0; i < eSortTabsLayoutCount; ++i)
	{
		m_sortCategory[i] = EInventorySortCategory::All;
		m_sortCategoryId[i] = "";
		if (m_sortTabControl[i])
		{
			m_sortTabControl[i]->ResetTab();
		}
	}
}

void CUIActorMenu::UpdateActorMoneyMP()
{
	if (!Level().game || !Game().local_player || !m_pActorInvOwner || IsGameTypeSingle() )
	{
		m_ActorMoney->SetText("");
		return;
	}

	s32 money = Game().local_player->money_for_round;

	string64 buf;
	xr_sprintf( buf, "%d RU", money );
	m_ActorMoney->SetText( buf );
}

void CUIActorMenu::SetActorInfoMP()
{
	if (!Level().game || !Game().local_player || !m_pActorInvOwner || IsGameTypeSingle())
	{
		m_ActorCharacterInfo->ClearInfo();
		return;
	}

	if (IsGameTypeSingleCompatible())
	{
		m_ActorCharacterInfo->InitCharacter(m_pActorInvOwner);
	}
	else
	{
		m_ActorCharacterInfo->InitCharacter(Game().local_player->getName(), "ui_npc_u_nebo_1");
	}

	UpdateActorMoneyMP();
}

void CUIActorMenu::OnSuccessRepairMP(PIItem item)
{
	UpdateItemsPlace();
	UpdateConditionProgressBars();

	if (CurrentItem() && ((PIItem)m_upgrade_selected->m_pData)->object_id() == item->object_id())
	{
		SeparateUpgradeItem();
	}
}

void CUIActorMenu::HideDialog()
{
	if (!IsShown())
		return;

	CBackpackAnimator* backpack_animator = m_pActorInvOwner->cast_actor()->HudAnimator()->BackpackAnimator();

	if (backpack_animator != nullptr
		&& backpack_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHidden
		&& backpack_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHiding)
	{
		backpack_animator->SetState(CHudStateAnimator::EAnimatorStates::eHiding);
	}

	GetHolder()->StopDialog(this);
}

// Controller UI

bool CUIActorMenu::MoveAreaSelector(eUIDirection4 dir)
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

eUIDirection4 CUIActorMenu::GetNaviDirection(CUIWindow* pWndFrom, CUIWindow* pWndTo)
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


void CUIActorMenu::SetAreaSelectionTo(CUIWindow* pSelection)
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
			frmPos = pSelection->GetWndPos();
			frmPos.sub(m_selectorPadding);

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

void CUIActorMenu::SetAuxMode(eActorMenuControllerAuxMode mode)
{
	m_AuxMode = mode;

	switch (mode)
	{
	case eActorMenuControllerAuxMode::eAuxMode_Upgrade:
		{
			if (m_ui_aux_selector)
			{
				Fvector2 frmSize = m_pUpgradeWnd->GetWndSize();
				Fvector2 frmPos = m_pUpgradeWnd->GetWndPos();

				if (frmSize.x > 0 && frmSize.y > 0)
				{
					m_ui_aux_selector->SetWndSize(frmSize);
					m_ui_aux_selector->SetWndPos(frmPos);
					m_ui_aux_selector_shown = true;
				}
				else
					m_ui_aux_selector_shown = false;
			}
			m_pUpgradeWnd->SetActiveForController(true);
			m_upgrade_info->init_upgrade(nullptr, nullptr);
		}
		break;
	default:
		m_ui_aux_selector_shown = false;
		m_pUpgradeWnd->SetActiveForController(false);
	}
}

void CUIActorMenu::UpdateGamepadLegend()
{
	if (!m_gamepad_legend)
		return;

	m_gamepad_legend->Show(!m_pItemDropAmountWnd->IsShown());
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
				if (pDragDropList && pDragDropList == m_pInventoryBagList)
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

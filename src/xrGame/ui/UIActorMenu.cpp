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
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/ui_base.h"
#include "UIHelperGame.h"
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

void CUIActorMenu::Update()
{	
	if (pInput->GetControllerMode())
	{
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
			if (m_pPartnerInvOwner->inventory().ModifyFrame() != m_trade_partner_inventory_state)
				InitPartnerInventoryContents();
			
			CheckDistance();
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
	UpdateActorWeightBarTooltip();
	m_ItemInfo->Update();
	m_hint_wnd->Update();
	m_exit_button->Show(!pInput->GetControllerMode());
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

void CUIActorMenu::CallMessageBoxYesNo( const char* text )
{
	m_message_box_yes_no->SetText( text );
	m_message_box_yes_no->func_on_ok = CUIWndCallback::void_function( this, &CUIActorMenu::OnMesBoxYes );
	m_message_box_yes_no->func_on_no = CUIWndCallback::void_function( this, &CUIActorMenu::OnMesBoxNo );
	m_message_box_yes_no->ShowDialog(false);
}

void CUIActorMenu::CallMessageBoxOK( const char* text )
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

namespace
{
	bool IsActorWeightHintRectInsideViewport(const Frect& viewport, const Frect& hintRect)
	{
		return (viewport.x1 < hintRect.x1) && (viewport.x2 > hintRect.x2) && (viewport.y1 < hintRect.y1)
			&& (viewport.y2 > hintRect.y2);
	}
} // namespace

void CUIActorMenu::InitActorWeightSection(CUIXml& uiXml, CUIXmlInit& xmlInit)
{
	constexpr const char* kActorWeightRow = "actor_weight_row";
	constexpr const char* kCaption = "actor_weight_row:actor_weight_caption";
	constexpr const char* kWeightStatic = "actor_weight_row:actor_weight";
	constexpr const char* kWeightBar = "actor_weight_row:weight_status_bar";
	constexpr const char* kWeightMax = "actor_weight_row:actor_weight_max";

	m_ActorWeightRow = nullptr;
	m_ActorWeightBar = nullptr;
	m_ActorWeight = nullptr;

	if (uiXml.NavigateToNode(kActorWeightRow, 0))
	{
		m_ActorWeightRow = new CUIWindow();
		m_ActorWeightRow->SetAutoDelete(true);
		AttachChild(m_ActorWeightRow);
		R_ASSERT2(xmlInit.InitWindow(uiXml, kActorWeightRow, 0, m_ActorWeightRow), kActorWeightRow);

		m_ActorBottomInfo = UIHelper::CreateStatic(uiXml, kCaption, m_ActorWeightRow);
		R_ASSERT(m_ActorBottomInfo != nullptr);

		if (uiXml.NavigateToNode(kWeightBar, 0))
		{
			m_ActorWeightBar = new CUIProgressBar();
			m_ActorWeightBar->SetAutoDelete(true);
			m_ActorWeightRow->AttachChild(m_ActorWeightBar);
			R_ASSERT2(xmlInit.InitProgressBar(uiXml, kWeightBar, 0, m_ActorWeightBar), kWeightBar);
			m_ActorWeightBar->Enable(true);
			m_ActorWeightBar->Show(true);
		}
		else
		{
			m_ActorWeight = UIHelper::CreateStatic(uiXml, kWeightStatic, m_ActorWeightRow);
			R_ASSERT(m_ActorWeight != nullptr);
		}

		m_ActorWeightMax = UIHelper::CreateStatic(uiXml, kWeightMax, m_ActorWeightRow);
		R_ASSERT(m_ActorWeightMax != nullptr);
		return;
	}

	m_ActorBottomInfo = UIHelper::CreateStatic(uiXml, "actor_weight_caption", this);
	m_ActorWeight = UIHelper::CreateStatic(uiXml, "actor_weight", this);
	m_ActorWeightMax = UIHelper::CreateStatic(uiXml, "actor_weight_max", this);
}

void CUIActorMenu::UpdateActorWeightBarTooltip()
{
	if (m_ActorWeightBar == nullptr || m_pActorInvOwner == nullptr)
	{
		return;
	}

	if (!m_ActorWeightBar->CursorOverWindow())
	{
		if (g_statHint->Owner() == m_ActorWeightBar)
		{
			g_statHint->Discard();
		}
		return;
	}

	if (g_statHint->Owner() != nullptr)
	{
		return;
	}

	if (Device.dwTimeContinual < m_ActorWeightBar->FocusReceiveTime() + 700)
	{
		return;
	}

	const float totalWeight = m_pActorInvOwner->inventory().CalcTotalWeight();
	const float maxCarry = m_pActorInvOwner->MaxCarryWeight();
	const char* kgStr = g_pStringTable->translate("st_kg").c_str();
	const char* maxCaption = g_pStringTable->translate("ui_inv_max_weight").c_str();

	string256 hintBuf;
	xr_sprintf(hintBuf, "%.3f %s\n%s %.3f %s", totalWeight, kgStr, maxCaption, maxCarry, kgStr);
	g_statHint->SetHintText(m_ActorWeightBar, hintBuf);

	Fvector2 cursorPos = GetUICursor().GetCursorPosition();
	Frect visRect;
	visRect.set(0.0f, 0.0f, UI_BASE_WIDTH, UI_BASE_HEIGHT);

	Frect hintRect;
	hintRect.set(0.0f, 0.0f, g_statHint->GetWidth(), g_statHint->GetHeight());
	hintRect.add(cursorPos.x, cursorPos.y);

	hintRect.sub(0.0f, hintRect.height());
	if (!IsActorWeightHintRectInsideViewport(visRect, hintRect))
	{
		hintRect.sub(hintRect.width(), 0.0f);
	}
	if (!IsActorWeightHintRectInsideViewport(visRect, hintRect))
	{
		hintRect.add(0.0f, hintRect.height());
	}
	if (!IsActorWeightHintRectInsideViewport(visRect, hintRect))
	{
		hintRect.add(hintRect.width(), 45.0f);
	}

	g_statHint->SetWndPos(hintRect.lt);
}

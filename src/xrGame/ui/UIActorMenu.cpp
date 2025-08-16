#include "stdafx.h"
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
#include "../CustomDetector.h"
#include "../eatable_item.h"

#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/UICursor.h"
#include "UICellItem.h"
#include "UICharacterInfo.h"
#include "UIItemInfo.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UIInventoryUpgradeWnd.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UIMessageBoxEx.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "UIMainIngameWnd.h"
#include "../trade.h"
#include "Car.h"
#include "../xrEngine/string_table.h"

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
	SetCurrentItem( nullptr );
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
}

void CUIActorMenu::PlaySnd(eActorMenuSndAction a)
{
	if (sounds[a]._handle())
        sounds[a].play					(nullptr, sm_2D);
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
	}else
	{
		PlaySnd								(eSndClose);
		SetMenuMode							(mmUndefined);
	}
	m_ActorStateInfo->Show					(status);
}

void CUIActorMenu::Draw()
{
	CurrentGameUI()->UIMainIngameWnd->DrawZoneMap();
	CurrentGameUI()->UIMainIngameWnd->DrawMainIndicatorsForInventory();

	inherited::Draw	();
	//m_ActorStateInfo->Draw();
	m_ItemInfo->Draw();
	m_hint_wnd->Draw();
}

void CUIActorMenu::Update()
{	
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
			CheckDistance();
			break;
		}
	default: R_ASSERT(0); break;
	}
	
	inherited::Update();
	m_ItemInfo->Update();
	m_hint_wnd->Update();
}

bool CUIActorMenu::StopAnyMove()  // true = актёр не идёт при открытом меню
{
	switch ( m_currMenuMode )
	{
	case mmInventory:
		return false;
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
		}
	}
	else //pBoxGO
	{
		VERIFY(pBoxGO);
		if (pActorGO->Position().distance_to( pBoxGO->Position() ) > 3.0f)
		{
			g_btnHint->Discard();
			HideDialog();
		}
	}
}

EDDListType CUIActorMenu::GetListType(CUIDragDropListEx* l)
{
	if(l==m_pInventoryBagList)			return iActorBag;
	if(l==m_pInventoryBeltList)			return iActorBelt;

	for (u8 i = 1; i <= m_slot_count; ++i)
	{
		if (m_pInvList[i] && m_pInvList[i] == l)
			return iActorSlot;
	}

	if(l==m_pTradeActorBagList)			return iActorBag;
	if(l==m_pTradeActorList)			return iActorTrade;
	if(l==m_pTradePartnerBagList)		return iPartnerTradeBag;
	if(l==m_pTradePartnerList)			return iPartnerTrade;
	if(l==m_pDeadBodyBagList)			return iDeadBodyBag;

	if(l==m_pQuickSlot && m_pQuickSlot)					return iQuickSlot;
	if(l==m_pTrashList)					return iTrashSlot;

	R_ASSERT(0);
	
	return iInvalid;
}

CUIDragDropListEx* CUIActorMenu::GetListByType(EDDListType t)
{
	switch(t)
	{
		case iActorBag:
			{
				if(m_currMenuMode==mmTrade)
					return m_pTradeActorBagList;
				else
					return m_pInventoryBagList;
			}break;
		case iDeadBodyBag:
			{
				return m_pDeadBodyBagList;
			}break;
		case iActorBelt:
			{
				return m_pInventoryBeltList;
			}break;
		default:
			{
				R_ASSERT("invalid call");
			}break;
	}
	return nullptr;
}

CUICellItem* CUIActorMenu::CurrentItem()
{
	return m_pCurrentCellItem;
}

PIItem CUIActorMenu::CurrentIItem()
{
	return	(m_pCurrentCellItem)? (PIItem)m_pCurrentCellItem->m_pData : nullptr;
}

void CUIActorMenu::SetCurrentItem(CUICellItem* itm)
{
	m_repair_mode = 0;
	m_pCurrentCellItem = itm;
	if ( !itm )
	{
		InfoCurItem( nullptr );
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
		else if(current_item->GetCondition()<m_pPartnerInvOwner->trade_parameters().buy_item_condition_factor)
			m_ItemInfo->InitItem	( cell_item, compare_item, u32(-1), "st_no_trade_tip_2" );
		else
			m_ItemInfo->InitItem	( cell_item, compare_item, item_price );
	}
	else
		m_ItemInfo->InitItem	( cell_item, compare_item, u32(-1));

//	m_ItemInfo->InitItem	( current_item, compare_item );
	float dx_pos = GetWndRect().left;
	fit_in_rect(m_ItemInfo, Frect().set( 0.0f, 0.0f, UI_BASE_WIDTH - dx_pos, UI_BASE_HEIGHT ), 10.0f, dx_pos );
}

void CUIActorMenu::UpdateItemsPlace()
{
	switch ( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		
		break;
	case mmTrade:
		UpdatePrices();
		break;
	case mmUpgrade:
		SetupUpgradeItem();
		break;
	case mmDeadBodySearch:
		UpdateDeadBodyBag();
		break;
	default:
		R_ASSERT(0);
		break;
	}

	if ( m_pActorInvOwner )
	{
		UpdateOutfit();
		UpdateActor();
	}
}

// ================================================================

void CUIActorMenu::clear_highlight_lists()
{
	for (u8 i = 1; i <= m_slot_count; ++i)
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
	}

	m_pInventoryBagList->clear_select_armament();

	switch ( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		break;
	case mmTrade:
		m_pTradeActorBagList->clear_select_armament();
		m_pTradeActorList->clear_select_armament();
		m_pTradePartnerBagList->clear_select_armament();
		m_pTradePartnerList->clear_select_armament();
		break;
	case mmUpgrade:
		break;
	case mmDeadBodySearch:
		m_pDeadBodyBagList->clear_select_armament();
		break;
	}
	m_highlight_clear = true;
}
void CUIActorMenu::highlight_item_slot(CUICellItem* cell_item)
{
	PIItem item = (PIItem)cell_item->m_pData;
	if (!item)
	{
		return;
	}

	if (CUIDragDropListEx::m_drag_item)
	{
		return;
	}

	u16 slot_id = item->BaseSlot();
	const static bool pistolsOnly = EngineExternal()[EEngineExternalGame::EnableInventoryPistolSlot];
	if ((slot_id == INV_SLOT_2 || slot_id == INV_SLOT_3) && !pistolsOnly)
	{
		if (m_pInvSlotHighlight[INV_SLOT_2])
		{
			m_pInvSlotHighlight[INV_SLOT_2]->Show(true);
		}

		if (m_pInvSlotHighlight[INV_SLOT_3])
		{
			m_pInvSlotHighlight[INV_SLOT_3]->Show(true);
		}

		return;
	}

	if (m_pInvSlotHighlight[slot_id])
	{
		m_pInvSlotHighlight[slot_id]->Show(true);
		return;
	}

	if (CEatableItem* eatable = item->cast_eatable_item())
	{
		if (cell_item->OwnerList() && GetListType(cell_item->OwnerList()) == iQuickSlot)
		{
			return;
		}

		if (m_QuickSlotsHighlight[0])
		{
			for (u8 i = 0; i < 4; i++)
			{
				m_QuickSlotsHighlight[i]->Show(true);
			}
		}
		return;
	}

	if (CArtefact* artefact = item->cast_artefact())
	{
		if (cell_item->OwnerList() && GetListType(cell_item->OwnerList()) == iActorBelt)
		{
			return;
		}

		Ivector2 cap = m_pInventoryBeltList->CellsCapacity();
		if (m_ArtefactSlotsHighlight[0])
		{
			for (u8 i = 0; i < cap.x; i++)
			{
				m_ArtefactSlotsHighlight[i]->Show(true);
			}
		}
		return;
	}
}
void CUIActorMenu::set_highlight_item(CUICellItem* cell_item)
{
	PIItem item = (PIItem)cell_item->m_pData;
	if (!item)
	{
		return;
	}

	highlight_item_slot(cell_item);

	switch ( m_currMenuMode )
	{
	case mmUndefined:
	case mmInventory:
	case mmUpgrade:
		{
			highlight_armament( item, m_pInventoryBagList );
			break;
		}
	case mmTrade:
		{
			highlight_armament( item, m_pTradeActorBagList );
			highlight_armament( item, m_pTradeActorList );
			highlight_armament( item, m_pTradePartnerBagList );
			highlight_armament( item, m_pTradePartnerList );
			break;
		}
	case mmDeadBodySearch:
		{
			highlight_armament( item, m_pInventoryBagList );
			highlight_armament( item, m_pDeadBodyBagList );
			break;
		}
	}
	m_highlight_clear = false;
}

void CUIActorMenu::highlight_armament( PIItem item, CUIDragDropListEx* ddlist )
{
	ddlist->clear_select_armament();
	highlight_ammo_for_weapon( item, ddlist );
	highlight_weapons_for_ammo( item, ddlist );
	highlight_weapons_for_addon( item, ddlist );
	highlight_related_config_sections(item, ddlist); // FFx001 ++
}

// FFx0001 ++
// Highlight separated by delimeter ',' related item sections on mouseover from the actor's inventory is item config include line highlight_related_sections with separated sections
void CUIActorMenu::highlight_related_config_sections(PIItem item, CUIDragDropListEx* ddlist)
{
	VERIFY(item);
	VERIFY(ddlist);

	if (item->m_HiglightRelatedItemSections.size() > 0)
	{
		u32 const cnt = ddlist->ItemsCount();
		for (size_t j = 0; j < item->m_HiglightRelatedItemSections.size(); ++j)
		{
			for (u32 i = 0; i < cnt; ++i)
			{
				CUICellItem* ci = ddlist->GetItemIdx(i);
				PIItem _item = (PIItem)ci->m_pData;
				if (!_item)
				{
					continue;
				}

				const shared_str item_section = _item->object().cNameSect();
				const shared_str to_higlight_section = item->m_HiglightRelatedItemSections[j];

				if (item_section.c_str() != nullptr && to_higlight_section.c_str() != nullptr && xr_strcmp(to_higlight_section, item_section) == 0)
				{
					ci->m_select_armament = true;
				}
			}
		}
	}
}

void CUIActorMenu::highlight_ammo_for_weapon(PIItem weapon_item, CUIDragDropListEx* ddlist)
{
	VERIFY(weapon_item);
	VERIFY(ddlist);
	static RStringVec ammo_types;
	ammo_types.resize(0);

	CWeapon* weapon = weapon_item->cast_weapon();
	CWeaponBinoculars* binoc = weapon_item->cast_weapon_binoculars();
	CWeaponKnife* knife = weapon_item->cast_weapon_knife();
	if (!weapon || binoc || knife)
	{
		return;
	}

	ammo_types.assign(weapon->m_ammoTypes.begin(), weapon->m_ammoTypes.end());

	CWeaponMagazinedWGrenade* wg = weapon_item->cast_weapon_magazined_w_grenade();
	if (wg && wg->IsGrenadeLauncherAttached() && wg->m_ammoTypes2.size())
	{
		ammo_types.insert(ammo_types.end(), wg->m_ammoTypes2.begin(), wg->m_ammoTypes2.end());
	}
	
	if (ammo_types.size() == 0)
	{
		return;
	}
	
	u32 const cnt = ddlist->ItemsCount();
	for (u32 i = 0; i < cnt; ++i)
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if (!item)
		{
			continue;
		}
		CWeaponAmmo* ammo = item->cast_weapon_ammo();
		if (!ammo)
		{
			highlight_addons_for_weapon(weapon_item, ci);
			continue; // for i
		}
		shared_str const& ammo_name = item->object().cNameSect();

		for (const shared_str& ammo_type : ammo_types)
		{
			if (ammo_name._get() == ammo_type._get())
			{
				ci->m_select_armament = true;
				break;
			}
		}
	}//for i
}

void CUIActorMenu::highlight_weapons_for_ammo(PIItem ammo_item, CUIDragDropListEx* ddlist)
{
	VERIFY(ammo_item);
	VERIFY(ddlist);
	CWeaponAmmo* ammo = ammo_item->cast_weapon_ammo();
	CWeaponBinoculars* binoc = ammo_item->cast_weapon_binoculars();
	CWeaponKnife* knife = ammo_item->cast_weapon_knife();
	if (!ammo)
	{
		return;
	}
	
	shared_str const& ammo_name = ammo_item->object().cNameSect();

	u32 const cnt = ddlist->ItemsCount();
	for (u32 i = 0; i < cnt; ++i)
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if (!item)
		{
			continue;
		}

		CWeapon* weapon = item->cast_weapon();
		if (!weapon || binoc || knife)
		{
			continue;
		}

		for (const shared_str& ammo_type : weapon->m_ammoTypes)
		{
			if (ammo_name._get() == ammo_type._get())
			{
				ci->m_select_armament = true;
				break;
			}
		}
		
		CWeaponMagazinedWGrenade* wg = item->cast_weapon_magazined_w_grenade();
		if (!wg || !wg->IsGrenadeLauncherAttached() || !wg->m_ammoTypes2.size())
		{
			continue; // for i
		}

		for (const shared_str& ammo_type2 : wg->m_ammoTypes2)
		{
			if (ammo_name._get() == ammo_type2._get())
			{
				ci->m_select_armament = true;
				break;
			}
		}
	}//for i
}

bool CUIActorMenu::highlight_addons_for_weapon(PIItem weapon_item, CUICellItem* ci)
{
	PIItem item = (PIItem)ci->m_pData;
	if (!item)
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
	if (pSilencer && weapon_item->CanAttach(pSilencer))
	{
		ci->m_select_armament = true;
		return true;
	}

	CGrenadeLauncher* pGrenadeLauncher = item->cast_addon_grenade_launcher();
	if (pGrenadeLauncher && weapon_item->CanAttach(pGrenadeLauncher))
	{
		ci->m_select_armament = true;
		return true;
	}
	return false;
}

void CUIActorMenu::highlight_weapons_for_addon(PIItem addon_item, CUIDragDropListEx* ddlist)
{
	VERIFY(addon_item);
	VERIFY(ddlist);

	CScope*	pScope = addon_item->cast_addon_scope();
	CSilencer* pSilencer = addon_item->cast_addon_silencer();
	CGrenadeLauncher* pGrenadeLauncher = addon_item->cast_addon_grenade_launcher();

	if (!pScope && !pSilencer && !pGrenadeLauncher)
	{
		return;
	}
	
	u32 const cnt = ddlist->ItemsCount();
	for (u32 i = 0; i < cnt; ++i)
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if (!item)
		{
			continue;
		}

		CWeapon* weapon = item->cast_weapon();
		if (!weapon)
		{
			continue;
		}

		if (pScope && weapon->ScopeAttachable() && weapon->ScopeFit(pScope))
		{
			ci->m_select_armament = true;
			continue;
		}

		if (pSilencer && weapon->CanAttach(pSilencer))
		{
			ci->m_select_armament = true;
			continue;
		}

		if (pGrenadeLauncher && weapon->CanAttach(pGrenadeLauncher))
		{
			ci->m_select_armament = true;
			continue;
		}

	}//for i
}

// -------------------------------------------------------------------=
void CUIActorMenu::ClearAllLists()
{
	m_pInventoryBagList->ClearAll(true);
	m_pInventoryBeltList->ClearAll(true);

	for (u8 i = 1; i <= m_slot_count; ++i)
	{
		if (m_pInvList[i])
			m_pInvList[i]->ClearAll(true);
	}

	if (m_pQuickSlot)
		m_pQuickSlot->ClearAll(true);

	m_pTradeActorBagList->ClearAll(true);
	m_pTradeActorList->ClearAll(true);
	m_pTradePartnerBagList->ClearAll(true);
	m_pTradePartnerList->ClearAll(true);
	m_pDeadBodyBagList->ClearAll(true);

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
	ClearAllLists				();
	m_pMouseCapturer			= nullptr;
	m_UIPropertiesBox->Hide		();
	SetCurrentItem				(nullptr);
}

void CUIActorMenu::UpdateActorMoneyMP()
{
	if ( !&Level() || !Level().game || !Game().local_player || !m_pActorInvOwner || IsGameTypeSingle() )
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
	if (!&Level() || !Level().game || !Game().local_player || !m_pActorInvOwner || IsGameTypeSingle())
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
bool CUIActorMenu::CanSetItemToList(PIItem item, CUIDragDropListEx* l, u16& ret_slot)
{
	u16 item_slot = item->BaseSlot();
	if( GetSlotList(item_slot)==l )
	{
		ret_slot	= item_slot;
		return		true;
	}

	const static bool pistolsOnly = EngineExternal()[EEngineExternalGame::EnableInventoryPistolSlot];
	if (item_slot == INV_SLOT_3 && l == m_pInvList[INV_SLOT_2] && !pistolsOnly)
	{
		ret_slot	= INV_SLOT_2;
		return		true;
	}

	if (item_slot == INV_SLOT_2&& l == m_pInvList[INV_SLOT_3] && !pistolsOnly)
	{
		ret_slot	= INV_SLOT_3;
		return		true;
	}

	return false;
}
void CUIActorMenu::UpdateConditionProgressBars()
{
	for (u8 i = 1; i <= m_slot_count; ++i)
	{
		PIItem itm = m_pActorInvOwner->inventory().ItemFromSlot(i);
		if (m_pInvSlotProgress[i])
			m_pInvSlotProgress[i]->SetProgressPos(itm ? iCeil(itm->GetCondition() * 10.f) / 10.f : 0);
	}

	//Highlight 'equipped' items in actor bag
	CUIDragDropListEx* slot_list = m_pInventoryBagList;
	u32 const cnt = slot_list->ItemsCount();
	for (u32 i = 0; i < cnt; ++i)
	{
		CUICellItem* ci = slot_list->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if (!item)
			continue;

		if (item->m_highlight_equipped && item->m_pInventory && item->m_pInventory->ItemFromSlot(item->BaseSlot()) == item)
			ci->m_select_equipped = true;
		else
			ci->m_select_equipped = false;
	}
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
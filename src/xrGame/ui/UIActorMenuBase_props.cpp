#include "stdafx.h"
#include "UIActorMenuBase.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "../../xrEngine/xr_input.h"
#include "../InventoryOwner.h"
#include "../../xrUI/UICursor.h"
#include "UICellItem.h"
#include "../Medkit.h"
#include "../eatable_item_object.h"
#include "../WeaponMagazined.h"
#include "../PDA.h"
#include "../Inventory.h"
#include "../../xrEngine/string_table.h"
#include "../ai_object_location.h"
#include "../game_sv_single.h"

bool RemoveItemFromList(CUIDragDropListEx* lst, PIItem pItem);
void move_item_from_to(u16 from_id, u16 to_id, u16 what_id);

void CUIActorMenuBase::TryHidePropertiesBox()
{
	if (m_UIPropertiesBox->IsShown())
	{
		m_UIPropertiesBox->Hide();
	}
}

void CUIActorMenuBase::ActivatePropertiesBox()
{
	TryHidePropertiesBox();

	if(!(m_currMenuMode == mmInventory || m_currMenuMode == mmDeadBodySearch || m_currMenuMode == mmUpgrade || m_currMenuMode == mmTrade)) 
	{
		return;
	}

	PIItem item = CurrentIItem();
	if(!item) 
	{
		return;
	}

	CUICellItem* cell_item = CurrentItem();
	m_UIPropertiesBox->RemoveAll();
	bool b_show = false;

	PropertiesBoxForSlots(cell_item, item, b_show);
	if (!(m_currMenuMode == mmTrade && (GetTradePartnerBagList()->IsOwner(cell_item) || GetTradePartnerList()->IsOwner(cell_item))))
	{
		PropertiesBoxForWeapon(cell_item, item, b_show);
	}
	if (m_currMenuMode == mmInventory || m_currMenuMode == mmDeadBodySearch)
	{
		PropertiesBoxForAddon(item, b_show);
		PropertiesBoxForUsing(item, b_show);
		PropertiesBoxForQuickSlots(cell_item, item, b_show);
		PropertiesBoxForPlaying(item, b_show);
		PropertiesBoxForParse(item, b_show);
		PropertiesBoxForDrop(cell_item, item, b_show);
	}
	else if(m_currMenuMode == mmUpgrade) 
	{
		PropertiesBoxForRepair(item, b_show);
		PropertiesBoxForUpgrade(item, b_show);
	}
	else if (m_currMenuMode == mmTrade)
	{
		PropertiesBoxForTrade(cell_item, item, b_show);
		if (m_isDonateCurrentItem)
		{
			CUIDragDropListEx* invlist = GetListByType(iActorBag);
			if (invlist->IsOwner(cell_item))
				PropertiesBoxForDonate(item, b_show);
		}
	}

	//-Alundaio

	if(b_show) 
	{
		m_UIPropertiesBox->AutoUpdateSize();

		Fvector2 cursor_pos_;
		Frect vis_rect;
		GetAbsoluteRect(vis_rect);

		if (!pInput->GetControllerMode())
			cursor_pos_ = GetUICursor().GetCursorPosition();
		else
			cell_item->GetAbsolutePos(cursor_pos_);
		
		cursor_pos_.sub(vis_rect.lt);
		m_UIPropertiesBox->Show(vis_rect, cursor_pos_);
		PlaySnd(eProperties);
	}
}

void CUIActorMenuBase::PropertiesBoxForUsing(PIItem item, bool& b_show)
{
	const char* act_str = nullptr;
	CGameObject* GO = smart_cast<CGameObject*>(item);
	shared_str	section_name = GO->cNameSect();

	//ability to set eat string from settings
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "default_use_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_EAT_ACTION);
		b_show = true;
	}
	else
	{
		CMedkit* pMedkit = smart_cast<CMedkit*>		(item);
		CAntirad* pAntirad = smart_cast<CAntirad*>		(item);
		CEatableItem* pEatableItem = smart_cast<CEatableItem*>	(item);
		CBottleItem* pBottleItem = smart_cast<CBottleItem*>	(item);

		if (pMedkit || pAntirad)
		{
			act_str = "st_use";
		}
		else if (pBottleItem)
		{
			act_str = "st_drink";
		}
		else if (pEatableItem)
		{
			act_str = *pEatableItem->UseText;
		}
		if (act_str)
		{
			m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_EAT_ACTION);
			b_show = true;
		}
	}

	if (IAntigas* pAntigas = smart_cast<IAntigas*>(item))
	{
		if (pAntigas->OnPropertiesBoxForUsing(m_UIPropertiesBox))
		{
			b_show = true;
		}
	}

	if (PowerBank* oPowerBank = smart_cast<PowerBank*>(item))
	{
		if (oPowerBank->OnPropertiesBoxForUsing(m_UIPropertiesBox))
		{
			b_show = true;
		}
	}

	if (IPowerManager* oPowerManager = smart_cast<IPowerManager*>(item))
	{
		if (oPowerManager->OnPropertiesBoxForUsing(m_UIPropertiesBox))
		{
			b_show = true;
		}
	}

	if (CNVG* oCNVG = smart_cast<CNVG*>(item))
	{
		if (oCNVG->OnVNGPropertiesBoxForUsing(m_UIPropertiesBox))
		{
			b_show = true;
		}
	}

	//1st Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use1_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_EAT2_ACTION);
		b_show = true;
	}

	//2nd Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use2_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_EAT3_ACTION);
		b_show = true;
	}

	//3rd Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use3_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_EAT4_ACTION);
		b_show = true;
	}

	//4th Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use4_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_EAT5_ACTION);
		b_show = true;
	}
}

void CUIActorMenuBase::PropertiesBoxForQuickSlots(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	if (cell_item == nullptr || m_pQuickSlot == nullptr)
	{
		return;
	}
	if (item->parent_id() != GetInventoryOwner()->object_id())
	{
		return;
	}

	CEatableItemObject* eatableObject = smart_cast<CEatableItemObject*>(item);
	if (eatableObject == nullptr)
	{
		return;
	}

	const Ivector2 itemGrid = item->GetInvGridRect().rb;
	if (itemGrid.x > 1 || itemGrid.y > 1)
	{
		return;
	}

	if (!item->Useful())
	{
		return;
	}

	m_UIPropertiesBox->AddItem("st_put_in_quick_slot_1", nullptr, INVENTORY_TO_QUICK_SLOT_1);
	m_UIPropertiesBox->AddItem("st_put_in_quick_slot_2", nullptr, INVENTORY_TO_QUICK_SLOT_2);
	m_UIPropertiesBox->AddItem("st_put_in_quick_slot_3", nullptr, INVENTORY_TO_QUICK_SLOT_3);
	m_UIPropertiesBox->AddItem("st_put_in_quick_slot_4", nullptr, INVENTORY_TO_QUICK_SLOT_4);
	b_show = true;
}

void CUIActorMenuBase::PropertiesBoxForPlaying(PIItem item, bool& b_show)
{
	CPda* pPda = item->cast_pda();
	if (!pPda || !pPda->CanPlayScriptFunction())
		return;

	const char* act_str = "st_play";
	m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_PLAY_ACTION);
	b_show = true;
}

void CUIActorMenuBase::PropertiesBoxForSlots(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	R_ASSERT(GetInventoryOwner());
	if (item->parent_id() != GetInventoryOwner()->object_id())
	{
		return;
	}

	CCustomOutfit* pOutfit = item->cast_outfit();
	CHelmet* pHelmet = item->cast_helmet();
	CBackpack* pBackpack = item->cast_backpack();
	CInventory* pInv = GetInventory();

	// Флаг-признак для невлючения пункта контекстного меню: Dreess Outfit, если костюм уже надет
	bool bAlreadyDressed = false;
	u16 cur_slot = item->BaseSlot();

	if (cur_slot == GRENADE_SLOT)
	{
		return;
	}

	if (!pOutfit && !pHelmet && !pBackpack && cur_slot != NO_ACTIVE_SLOT && !pInv->SlotIsPersistent(cur_slot) && pInv->CanPutInSlot(item, cur_slot, true))
	{
		m_UIPropertiesBox->AddItem("st_move_to_slot", nullptr, INVENTORY_TO_SLOT_ACTION);
		b_show = true;
	}
	if (item->Belt() && pInv->CanPutInBelt(item))
	{
		m_UIPropertiesBox->AddItem("st_move_on_belt", nullptr, INVENTORY_TO_BELT_ACTION);
		b_show = true;
	}

	if (item->Ruck() && pInv->CanPutInRuck(item) && (cur_slot == NO_ACTIVE_SLOT || !pInv->SlotIsPersistent(cur_slot)))
	{
		if (!pOutfit)
		{
			if (!pHelmet)
			{
				if (!pBackpack)
				{
					m_UIPropertiesBox->AddItem("st_move_to_bag", nullptr, INVENTORY_TO_BAG_ACTION);
				}
				else
				{
					m_UIPropertiesBox->AddItem("st_undress_backpack", nullptr, INVENTORY_TO_BAG_ACTION);
				}
			}
			else
			{
				m_UIPropertiesBox->AddItem("st_undress_helmet", nullptr, INVENTORY_TO_BAG_ACTION);
			}
		}
		else
		{
			m_UIPropertiesBox->AddItem("st_undress_outfit", nullptr, INVENTORY_TO_BAG_ACTION);
		}

		bAlreadyDressed = true;
		b_show = true;
	}

	if (pOutfit && !bAlreadyDressed)
	{
		m_UIPropertiesBox->AddItem("st_dress_outfit", nullptr, INVENTORY_TO_SLOT_ACTION);
		b_show = true;
	}

	CCustomOutfit* outfit_in_slot = GetInventoryOwner()->GetOutfit();
	if (pHelmet && !bAlreadyDressed && (!outfit_in_slot || outfit_in_slot->bIsHelmetAvaliable))
	{
		m_UIPropertiesBox->AddItem("st_dress_helmet", nullptr, INVENTORY_TO_SLOT_ACTION);
		b_show = true;
	}

	if (pBackpack && !bAlreadyDressed)
	{
		m_UIPropertiesBox->AddItem("st_dress_backpack", nullptr, INVENTORY_TO_SLOT_ACTION);
		b_show = true;
	}
}

void CUIActorMenuBase::PropertiesBoxForDrop(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	if (!item->IsQuestItem())
	{
		if (m_currMenuMode != mmDeadBodySearch)
		{
			m_UIPropertiesBox->AddItem("st_drop", nullptr, INVENTORY_DROP_ACTION);
			b_show = true;

			if (cell_item->ChildsCount())
			{
				if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
				{
					m_UIPropertiesBox->AddItem("st_drop_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_DROP_ACTION);
				}
				m_UIPropertiesBox->AddItem("st_drop_all", (void*)INVENTORY_ALL_CODE, INVENTORY_DROP_ACTION);
			}
		}
		else
		{
			if (item->parent_id() == GetInventoryOwner()->object_id())
			{
				m_UIPropertiesBox->AddItem("st_move_to", nullptr, INVENTORY_DROP_ACTION);
				b_show = true;

				if (cell_item->ChildsCount())
				{
					if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
					{
						m_UIPropertiesBox->AddItem("st_move_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_DROP_ACTION);
					}
					m_UIPropertiesBox->AddItem("st_move_all", (void*)INVENTORY_ALL_CODE, INVENTORY_DROP_ACTION);
				}
			}
		}
	}
	if (item->parent_id() != GetInventoryOwner()->object_id())
	{
		m_UIPropertiesBox->AddItem("st_take_to", nullptr, INVENTORY_DROP_ACTION);
		b_show = true;

		if (cell_item->ChildsCount())
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_UIPropertiesBox->AddItem("st_take_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_DROP_ACTION);
			}
			m_UIPropertiesBox->AddItem("st_take_all", (void*)INVENTORY_ALL_CODE, INVENTORY_DROP_ACTION);
		}
	}
}

void CUIActorMenuBase::PropertiesBoxForParse(PIItem item, bool& b_show)
{
	if (pSettings->line_exist(item->m_section_id, "parse_spawn_items") && pSettings->line_exist(item->m_section_id, "parse_spawn_chances"))
	{
		m_UIPropertiesBox->AddItem("st_parse", nullptr, INVENTORY_PARSE_ITEM);
		b_show = true;
	}
}

void CUIActorMenuBase::PropertiesBoxForAddon(PIItem item, bool& b_show)
{
	//присоединение аддонов к активному слоту (2 или 3)

	CScope* pScope = item->cast_addon_scope();
	CSilencer* pSilencer = item->cast_addon_silencer();
	CGrenadeLauncher* pGrenadeLauncher = item->cast_addon_grenade_launcher();

	CInventory* inv = GetInventory();
	R_ASSERT(inv);

	PIItem	item_in_slot_2 = inv->ItemFromSlot(INV_SLOT_2);
	PIItem	item_in_slot_3 = inv->ItemFromSlot(INV_SLOT_3);
	PIItem	item_in_slot_pistol_new = inv->ItemFromSlot(PISTOL_SLOT_NEW);

	if (!item_in_slot_2 && !item_in_slot_3)
		return;

	if (pScope)
	{
		if (item_in_slot_pistol_new && item_in_slot_pistol_new->CanAttach(pScope))
		{
			shared_str str = g_pStringTable->translate("st_attach_scope_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_pistol_new->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_pistol_new, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if (item_in_slot_2 && item_in_slot_2->CanAttach(pScope))
		{
			shared_str str = g_pStringTable->translate("st_attach_scope_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_2->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON);
			//			m_UIPropertiesBox->AddItem( "st_attach_scope_to_pistol",  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
			b_show = true;
		}
		if (item_in_slot_3 && item_in_slot_3->CanAttach(pScope))
		{
			shared_str name = g_pStringTable->translate("st_attach_scope_to_rifle");
			shared_str add_name = g_pStringTable->translate("st_attach_scope_to_pistol");
			shared_str str;
			if (!xr_strcmp(name, "st_attach_scope_to_rifle") && xr_strcmp(add_name, "st_attach_scope_to_pistol"))
			{
				str = add_name;
			}
			else
				str = name;

			str.printf("%s %s", str.c_str(), item_in_slot_3->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON);
			//			m_UIPropertiesBox->AddItem( "st_attach_scope_to_rifle",  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
			b_show = true;
		}
		return;
	}

	if (pSilencer)
	{
		if (item_in_slot_pistol_new && item_in_slot_pistol_new->CanAttach(pSilencer))
		{
			shared_str str = g_pStringTable->translate("st_attach_silencer_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_pistol_new->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_pistol_new, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if (item_in_slot_2 && item_in_slot_2->CanAttach(pSilencer))
		{
			shared_str str = g_pStringTable->translate("st_attach_silencer_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_2->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if (item_in_slot_3 && item_in_slot_3->CanAttach(pSilencer))
		{
			shared_str name = g_pStringTable->translate("st_attach_silencer_to_rifle");
			shared_str add_name = g_pStringTable->translate("st_attach_silencer_to_pistol");
			shared_str str;
			if (!xr_strcmp(name, "st_attach_silencer_to_rifle") &&
				xr_strcmp(add_name, "st_attach_silencer_to_pistol"))
			{
				str = add_name;
			}
			else
				str = name;

			str.printf("%s %s", str.c_str(), item_in_slot_3->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		return;
	}

	if (pGrenadeLauncher)
	{
		if (item_in_slot_pistol_new && item_in_slot_pistol_new->CanAttach(item_in_slot_pistol_new))
		{
			shared_str str = g_pStringTable->translate("st_attach_gl_to_rifle");
			str.printf("%s %s", str.c_str(), item_in_slot_pistol_new->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_pistol_new, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if (item_in_slot_2 && item_in_slot_2->CanAttach(pGrenadeLauncher))
		{
			shared_str str = g_pStringTable->translate("st_attach_gl_to_rifle");
			str.printf("%s %s", str.c_str(), item_in_slot_2->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON);
			//			m_UIPropertiesBox->AddItem( "st_attach_gl_to_pistol",  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
			b_show = true;
		}
		if (item_in_slot_3 && item_in_slot_3->CanAttach(pGrenadeLauncher))
		{
			shared_str str = g_pStringTable->translate("st_attach_gl_to_rifle");
			str.printf("%s %s", str.c_str(), item_in_slot_3->m_name.c_str());
			m_UIPropertiesBox->AddItem(str.c_str(), (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON);
			//			m_UIPropertiesBox->AddItem( "st_attach_gl_to_rifle",  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
			b_show = true;
		}
	}
}

void CUIActorMenuBase::PropertiesBoxForWeapon(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	//отсоединение аддонов от вещи
	CWeapon* pWeapon = item->cast_weapon();
	if (!pWeapon)
	{
		return;
	}

	if (pWeapon->GrenadeLauncherAttachable())
	{
		if (pWeapon->IsGrenadeLauncherAttached())
		{
			m_UIPropertiesBox->AddItem("st_detach_gl", nullptr, INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON);
			b_show = true;
		}
		else
		{
		}
	}
	if (pWeapon->ScopeAttachable())
	{
		if (pWeapon->IsScopeAttached())
		{
			m_UIPropertiesBox->AddItem("st_detach_scope", nullptr, INVENTORY_DETACH_SCOPE_ADDON);
			b_show = true;
		}
		else
		{
		}
	}
	if (pWeapon->SilencerAttachable())
	{
		if (pWeapon->IsSilencerAttached())
		{
			m_UIPropertiesBox->AddItem("st_detach_silencer", nullptr, INVENTORY_DETACH_SILENCER_ADDON);
			b_show = true;
		}
		else
		{
		}
	}
	if (pWeapon->cast_weapon_magazined() != nullptr && IsGameTypeSingleCompatible())
	{
		bool b = (pWeapon->GetAmmoElapsed() || pWeapon->IsChamber() && pWeapon->GetAmmoChamberElapsed());
		if (!b)
		{
			for (u32 i = 0; i < cell_item->ChildsCount(); ++i)
			{
				CWeapon* data = (CWeapon*)cell_item->Child(i)->m_pData;
				CWeaponMagazined* weap_mag = data != nullptr ? data->cast_weapon_magazined() : nullptr;
				if (weap_mag != nullptr && (weap_mag->GetAmmoElapsed() || weap_mag->IsChamber() && weap_mag->GetAmmoChamberElapsed()))
				{
					b = true;
					break; // for
				}
			}
		}
		if (b)
		{
			m_UIPropertiesBox->AddItem("st_unload_magazine", nullptr, INVENTORY_UNLOAD_MAGAZINE);
			b_show = true;
		}
	}
}

//Alundaio: Ability to donate item during trade
void CUIActorMenuBase::PropertiesBoxForDonate(PIItem item, bool& b_show)
{
	m_UIPropertiesBox->AddItem("st_donate", nullptr, INVENTORY_DONATE_ACTION);
	b_show = true;
}
//-Alundaio

void CUIActorMenuBase::PropertiesBoxForTrade(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	CUIDragDropListEx* pOwnerList = cell_item->OwnerList();
	if (pOwnerList == GetTradeActorList())
	{
		m_UIPropertiesBox->AddItem("st_remove_from_offer", nullptr, INVENTORY_TO_BAG_ACTION);
		if (cell_item->ChildsCount())
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_UIPropertiesBox->AddItem("st_remove_from_offer_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_TO_BAG_ACTION);
			}
			m_UIPropertiesBox->AddItem("st_remove_from_offer_all", (void*)INVENTORY_ALL_CODE, INVENTORY_TO_BAG_ACTION);
		}
		b_show = true;
	}
	else if (pOwnerList == GetTradeActorBagList())
	{
		if (CanMoveToPartner(item))
		{
			m_UIPropertiesBox->AddItem("st_move_to_offer", nullptr, INVENTORY_SHOP_OFFER_ITEM_ACTION);
			if (cell_item->ChildsCount())
			{
				if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
				{
					m_UIPropertiesBox->AddItem("st_move_to_offer_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_SHOP_OFFER_ITEM_ACTION);
				}
				m_UIPropertiesBox->AddItem("st_move_to_offer_all", (void*)INVENTORY_ALL_CODE, INVENTORY_SHOP_OFFER_ITEM_ACTION);
			}
			b_show = true;
		}
	}
	else if (pOwnerList == GetTradePartnerList())
	{
		m_UIPropertiesBox->AddItem("st_remove_from_cart", nullptr, INVENTORY_SHOP_UNCHOOSE_ITEM_ACTION);
		if (cell_item->ChildsCount())
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_UIPropertiesBox->AddItem("st_remove_from_cart_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_SHOP_UNCHOOSE_ITEM_ACTION);
			}
			m_UIPropertiesBox->AddItem("st_remove_from_cart_all", (void*)INVENTORY_ALL_CODE, INVENTORY_SHOP_UNCHOOSE_ITEM_ACTION);
		}
		b_show = true;
	}
	else if (pOwnerList == GetTradePartnerBagList())
	{
		SInvItemPlace	pl;
		pl.type = eItemPlaceRuck;
		if (GetPartner()->AllowItemToTrade(item, pl))
		{
			m_UIPropertiesBox->AddItem("st_move_to_cart", nullptr, INVENTORY_SHOP_CHOOSE_ITEM_ACTION);
			if (cell_item->ChildsCount())
			{
				if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
				{
					m_UIPropertiesBox->AddItem("st_move_to_cart_amount", (void*)INVENTORY_AMOUNT_CODE, INVENTORY_SHOP_CHOOSE_ITEM_ACTION);
				}
				m_UIPropertiesBox->AddItem("st_move_to_cart_all", (void*)INVENTORY_ALL_CODE, INVENTORY_SHOP_CHOOSE_ITEM_ACTION);
			}
			b_show = true;
		}
	}
}

void CUIActorMenuBase::PropertiesBoxForUpgrade(PIItem item, bool& b_show)
{
	if (CanUpgradeItem(item))
	{
		m_UIPropertiesBox->AddItem("st_upgrade", nullptr, INVENTORY_UPGRADE);
		b_show = true;
	}
}

void CUIActorMenuBase::PropertiesBoxForRepair(PIItem item, bool& b_show)
{
	CCustomOutfit* pOutfit = item->cast_outfit();
	CWeapon*       pWeapon = item->cast_weapon();
	CHelmet*       pHelmet = item->cast_helmet();

	if ( (pOutfit || pWeapon || pHelmet) && item->GetCondition() < 0.99f )
	{
		m_UIPropertiesBox->AddItem( "st_repair", nullptr, INVENTORY_REPAIR );
		b_show = true;
	}
}

void CUIActorMenuBase::ProcessPropertiesBoxClicked(CUIWindow* w, void* d)
{
	PIItem item	= CurrentIItem();
	CUICellItem* cell_item = CurrentItem();
	if (!m_UIPropertiesBox->GetClickedItem() || !item || !cell_item || !cell_item->OwnerList())
	{
		return;
	}
	CWeapon* weapon = item->cast_weapon();
	
	// we dont want to clear selection all the time. If for example DROP-ONE was selected
	// for a stack of items and we still have remaining items (we need that selection).
	// So for dropall we clear selection, for drop 1 item we check how many items are left in stack.
	// Same for eat, use, move
	bool bClearCurrentItem = true;

	if (IAntigas* oAntigas = smart_cast<IAntigas*>(item->cast_inventory_item()))
	{
		if (oAntigas->OnProcessPropertiesBoxClicked(m_UIPropertiesBox))
		{
			return;
		}
	}

	if (PowerBank* oPowerBank = smart_cast<PowerBank*>(item->cast_inventory_item()))
	{
		if (oPowerBank->OnProcessPropertiesBoxClicked(m_UIPropertiesBox))
		{
			return;
		}
	}

	if (IPowerManager* oPowerManager = smart_cast<IPowerManager*>(item->cast_inventory_item()))
	{
		if (oPowerManager->OnProcessPropertiesBoxClicked(m_UIPropertiesBox))
		{
			return;
		}
	}

	if (CNVG* oCNVG = smart_cast<CNVG*>(item->cast_inventory_item()))
	{
		if (oCNVG->OnVNGPropertiesBoxClicked(m_UIPropertiesBox))
		{
			return;
		}
	}

	switch ( m_UIPropertiesBox->GetClickedItem()->GetTAG() )
	{
	case INVENTORY_TO_SLOT_ACTION:	ToSlot( cell_item, true, item->BaseSlot() );		break;
	case INVENTORY_TO_BELT_ACTION:	
		ToBelt( cell_item, false );		break;
	case INVENTORY_TO_BAG_ACTION:
	{
		void* d = m_UIPropertiesBox->GetClickedItem()->GetData();
		if (d == (void*)INVENTORY_ALL_CODE)
		{
			ToBagAll(cell_item->ChildsCount() + 1);
		}
		else if (d == (void*)INVENTORY_AMOUNT_CODE)
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeFromOffer, item);
			}
		}
		else
		{
			ToBag(cell_item, false);
		}
		break;
	}
	case INVENTORY_DONATE_ACTION:
	{
		DonateCurrentItem(cell_item);
		break;
	}
	case INVENTORY_EAT_ACTION:
	{
		TryUseItem(cell_item);
		break;
	}
	case INVENTORY_EAT2_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		const char* functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use1_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct1;
			if (ai().script_engine().functor(functor_name, funct1))
			{
				if (funct1(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_EAT3_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		const char* functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use2_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct2;
			if (ai().script_engine().functor(functor_name, funct2))
			{
				if (funct2(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_EAT4_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		const char* functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use3_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct3;
			if (ai().script_engine().functor(functor_name, funct3))
			{
				if (funct3(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_EAT5_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		const char* functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use4_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct4;
			if (ai().script_engine().functor(functor_name, funct4))
			{
				if (funct4(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_DROP_ACTION:
		{
			void* d_ = m_UIPropertiesBox->GetClickedItem()->GetData();
			if(m_currMenuMode != mmDeadBodySearch) 
			{
				if(d_ == (void*)INVENTORY_ALL_CODE)
				{
					DropAllCurrentItem(cell_item->ChildsCount() + 1);
				}
				else if (m_pItemDropAmountWnd != nullptr && m_pItemDropAmountWnd->HasInitializedLayout() && d_ == (void*)INVENTORY_AMOUNT_CODE)
				{
					m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeDrop, item);
				}
				else
				{
					SendEvent_Item_Drop(item, GetInventoryOwner()->object_id());
				}
			}
			else 
			{
				if(item->parent_id() == GetInventoryOwner()->object_id())
				{
					auto ownerID = GetPartner() ? GetPartner()->object_id() : GetInvBox()->ID();
					bool isAllowPlace = IsAllowPlaceToInvBox(cell_item);

					if(d_ == (void*)INVENTORY_ALL_CODE && isAllowPlace)
					{
						MoveAllCurrentItem(cell_item->ChildsCount() + 1);
					}
					else if (m_pItemDropAmountWnd != nullptr && m_pItemDropAmountWnd->HasInitializedLayout() && d_ == (void*)INVENTORY_AMOUNT_CODE && isAllowPlace)
					{
						m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeMove, item);
					}
					else if (isAllowPlace) 
					{
						move_item_from_to(item->parent_id(), ownerID, item->object_id());
					}
				}
				else 
				{
					if(d_ == (void*)INVENTORY_ALL_CODE)
					{
						TakeAllCurrentItem(cell_item->ChildsCount() + 1);
					}
					else if (m_pItemDropAmountWnd != nullptr && m_pItemDropAmountWnd->HasInitializedLayout() && d_ == (void*)INVENTORY_AMOUNT_CODE)
					{
						m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeTake, item);
					}
					else
					{
						ToBag(cell_item, false);
					}
				}
			}
			break;
		}
	case INVENTORY_ATTACH_ADDON:
		{
			PIItem item_ = CurrentIItem(); // temporary storing because of AttachAddon is setting curiitem to nullptr
			AttachAddon((PIItem)(m_UIPropertiesBox->GetClickedItem()->GetData()));
			if(m_currMenuMode==mmDeadBodySearch)
				RemoveItemFromList(GetPartnerList(), item_);
			
			break;
		}
	case INVENTORY_DETACH_SCOPE_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetScopeName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem* child_itm = cell_item->Child(i);
				PIItem child_iitm = (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm ? child_iitm->cast_weapon() : nullptr;
				if (child_iitm && wpn)
				{
					DetachAddon(wpn->GetScopeName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_DETACH_SILENCER_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetSilencerName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem* child_itm = cell_item->Child(i);
				PIItem child_iitm = (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm ? child_iitm->cast_weapon() : nullptr;
				if (child_iitm && wpn)
				{
					DetachAddon(wpn->GetSilencerName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetGrenadeLauncherName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem* child_itm = cell_item->Child(i);
				PIItem child_iitm = (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm ? child_iitm->cast_weapon() : nullptr;
				if (child_iitm && wpn)
				{
					DetachAddon(wpn->GetGrenadeLauncherName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_RELOAD_MAGAZINE:
		if ( weapon )
		{
			weapon->Action( kWPN_RELOAD, CMD_START );
		}
		break;
	case INVENTORY_UNLOAD_MAGAZINE:
		{
			CWeapon* weapon = (CWeapon*)cell_item->m_pData;
			CWeaponMagazined* weap_mag = weapon ? weapon->cast_weapon_magazined() : nullptr;
			if (weap_mag == nullptr)
			{
				break;
			}

			UnloadWeapon(weap_mag);
			if (!weap_mag->IsGrenadeMode())
			{
				weap_mag->UnloadChamber();
			}

			weap_mag->m_bHaveShell = false;
			weap_mag->m_bNeedPumpState = false;

			for (u32 i = 0; i < cell_item->ChildsCount(); ++i)
			{
				CUICellItem* child_itm = cell_item->Child(i);
				weapon = (CWeapon*)child_itm->m_pData;
				CWeaponMagazined* child_weap_mag = weapon ? weapon->cast_weapon_magazined() : nullptr;
				if (child_weap_mag != nullptr)
				{
					UnloadWeapon(child_weap_mag);
					if (!child_weap_mag->IsGrenadeMode())
					{
						child_weap_mag->UnloadChamber();
					}

					child_weap_mag->m_bHaveShell = false;
					child_weap_mag->m_bNeedPumpState = false;
				}
			}
			break;
		}
	case INVENTORY_REPAIR:
		{
			TryRepairItem(this,0);
			return;
			break;
		}
	case INVENTORY_UPGRADE:
	{
		SetAuxMode(eAuxMode_Upgrade);
		if (pInput->GetControllerMode()) 
			return;
		break;
	}
	case INVENTORY_PLAY_ACTION:
		{
			if (CPda* pPda = item->cast_pda())
			{
				pPda->PlayScriptFunction();
			}
			break;
		}
	case INVENTORY_PARSE_ITEM:
	{
		game_sv_Single* tpGame = Level().Server->game != nullptr ? Level().Server->game->cast_game_sv_single() : nullptr;
		if (tpGame == nullptr) {
			break;
		}

		CActor* actor = Level().CurrentEntity() ? Level().CurrentEntity()->cast_actor() : nullptr;
		if (actor == nullptr) {
			break;
		}

		extern CSE_Abstract* CALifeSimulator__spawn_item2(CALifeSimulator* self_, const char* section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent);

		int Count = item->m_parse_params.m_items.size();
		int Count2 = item->m_parse_params.m_chances.size();

		for (int i = 0; i < Count; ++i)
		{
			float chance = 0.0f;

			if (i >= Count2)
			{
				chance = item->m_parse_params.m_chances.back();
			}
			else
			{
				chance = item->m_parse_params.m_chances[i];
			}

			if (chance >= ::Random.randF(0.0f, 1.0f))
			{
				CALifeSimulator__spawn_item2(&tpGame->alife(), *item->m_parse_params.m_items[i], actor->Position(), actor->ai_location().level_vertex_id(), actor->ai_location().game_vertex_id(), actor->ID());
			}
		}
		item->object().DestroyObject();
	}break;
	case INVENTORY_SHOP_OFFER_ITEM_ACTION:
	{
		void* d = m_UIPropertiesBox->GetClickedItem()->GetData();
		if (d == (void*)INVENTORY_ALL_CODE)
		{
			ToActorTradeAll(cell_item->ChildsCount() + 1);
		}
		else if (d == (void*)INVENTORY_AMOUNT_CODE)
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeToOffer, item);
			}
		}
		else
		{
			ToActorTrade(cell_item, false);
		}
	}
	break;
	case INVENTORY_SHOP_CHOOSE_ITEM_ACTION:
	{
		void* d = m_UIPropertiesBox->GetClickedItem()->GetData();
		if (d == (void*)INVENTORY_ALL_CODE)
		{
			ToPartnerTradeAll(cell_item->ChildsCount() + 1);
		}
		else if (d == (void*)INVENTORY_AMOUNT_CODE)
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeToCart, item);
			}
		}
		else
		{
			ToPartnerTrade(cell_item, false);
		}
	}
	break;
	case INVENTORY_SHOP_UNCHOOSE_ITEM_ACTION:
	{
		void* d = m_UIPropertiesBox->GetClickedItem()->GetData();
		if (d == (void*)INVENTORY_ALL_CODE)
		{
			ToPartnerTradeBagAll(cell_item->ChildsCount() + 1);
		}
		else if (d == (void*)INVENTORY_AMOUNT_CODE)
		{
			if (m_pItemDropAmountWnd && m_pItemDropAmountWnd->HasInitializedLayout())
			{
				m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeFromCart, item);
			}
		}
		else
		{
			ToPartnerTradeBag(cell_item, false);
		}
	}
	break;

	case INVENTORY_TO_QUICK_SLOT_1:
		ToQuickSlotAt(cell_item, 0);
		break;
	case INVENTORY_TO_QUICK_SLOT_2:
		ToQuickSlotAt(cell_item, 1);
		break;
	case INVENTORY_TO_QUICK_SLOT_3:
		ToQuickSlotAt(cell_item, 2);
		break;
	case INVENTORY_TO_QUICK_SLOT_4:
		ToQuickSlotAt(cell_item, 3);
		break;

	}

	UpdateItemsPlace();
	UpdateConditionProgressBars();
}

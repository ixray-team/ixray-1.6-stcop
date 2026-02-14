#include "StdAfx.h"
#include "UIOwnerPropertiesBox.h"

#include "UICellCustomItems.h"
#include "UICellItem.h"

#include "../xrUI/Widgets/UIListBoxItem.h"
#include "../xrUI/Widgets/UIMessages.h"
#include "../xrUI/Widgets/UIPropertiesBox.h"

#include "../ai_object_location.h"
#include "../antirad.h"
#include "../BottleItem.h"
#include "../game_sv_single.h"
#include "../GrenadeLauncher.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../Level.h"
#include "../medkit.h"
#include "../Pda.h"
#include "../Scope.h"
#include "../Silencer.h"
#include "../Weapon.h"
#include "../WeaponMagazined.h"

#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"

void CUIOwnerPropertiesBox::TryHidePropertiesBox()
{
	if (m_UIPropertiesBox->IsShown())
	{
		m_UIPropertiesBox->Hide();
	}
}

void CUIOwnerPropertiesBox::PropertiesBoxForUsing(PIItem item, bool& b_show)
{
	LPCSTR act_str = nullptr;
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
		m_UIPropertiesBox->AddItem(act_str, NULL, INVENTORY_EAT2_ACTION);
		b_show = true;
	}

	//2nd Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use2_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, NULL, INVENTORY_EAT3_ACTION);
		b_show = true;
	}

	//3rd Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use3_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, NULL, INVENTORY_EAT4_ACTION);
		b_show = true;
	}

	//4th Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use4_text", 0);
	if (act_str)
	{
		m_UIPropertiesBox->AddItem(act_str, NULL, INVENTORY_EAT5_ACTION);
		b_show = true;
	}
}

void CUIOwnerPropertiesBox::PropertiesBoxForPlaying(PIItem item, bool& b_show)
{
	CPda* pPda = item->cast_pda();
	if (!pPda || !pPda->CanPlayScriptFunction())
		return;

	LPCSTR act_str = "st_play";
	m_UIPropertiesBox->AddItem(act_str, nullptr, INVENTORY_PLAY_ACTION);
	b_show = true;
}

void CUIOwnerPropertiesBox::PropertiesBoxForSlots(CUICellItem* cell_item, PIItem item, bool& b_show)
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

void CUIOwnerPropertiesBox::PropertiesBoxForDrop(CUICellItem* cell_item, PIItem item, bool& b_show)
{
}

void CUIOwnerPropertiesBox::PropertiesBoxForParse(PIItem item, bool& b_show)
{
	if (pSettings->line_exist(item->m_section_id, "parse_spawn_items") && pSettings->line_exist(item->m_section_id, "parse_spawn_chances"))
	{
		m_UIPropertiesBox->AddItem("st_parse", nullptr, INVENTORY_PARSE_ITEM);
		b_show = true;
	}
}

void CUIOwnerPropertiesBox::PropertiesBoxForAddon(PIItem item, bool& b_show)
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

void CUIOwnerPropertiesBox::PropertiesBoxForWeapon(CUICellItem* cell_item, PIItem item, bool& b_show)
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

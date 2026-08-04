#include "StdAfx.h"
#include "UIActorMenuBase.h"
#include "UICellItem.h"
#include "../PowerCell.h"
#include "../WeaponMagazinedWGrenade.h"
#include "../InventoryWeaponSlotLayout.h"

void CUIActorMenuBase::clear_highlight_lists()
{
	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		if (m_pInvSlotHighlight[i])
			m_pInvSlotHighlight[i]->Show(false);
	}

	for (u8 i = 0; i < 4; i++)
	{
		if (m_QuickSlotsHighlight[i])
			m_QuickSlotsHighlight[i]->Show(false);
	}
	for (u8 i = 0; i < m_ArtefactSlotsCount; i++)
	{
		if (m_ArtefactSlotsHighlight[i])
			m_ArtefactSlotsHighlight[i]->Show(false);
	}
	if (GetActorList())
		GetActorList()->clear_select_armament();

	switch ( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		break;
	case mmTrade:
		GetTradeActorBagList()->clear_select_armament();
		GetTradeActorList()->clear_select_armament();
		GetTradePartnerBagList()->clear_select_armament();
		GetTradePartnerList()->clear_select_armament();
		break;
	case mmUpgrade:
		break;
	case mmDeadBodySearch:
		GetPartnerList()->clear_select_armament();
		break;
	}
	m_highlight_clear = true;
}

void CUIActorMenuBase::set_highlight_item(CUICellItem* cell_item)
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
			highlight_armament( item, GetActorList() );
			break;
		}
	case mmTrade:
		{
			highlight_armament( item, GetTradeActorBagList() );
			highlight_armament( item, GetTradeActorList() );
			highlight_armament( item, GetTradePartnerBagList() );
			highlight_armament( item, GetTradePartnerList() );
			break;
		}
	case mmDeadBodySearch:
		{
			highlight_armament( item, GetActorList() );
			highlight_armament( item, GetPartnerList() );
			break;
		}
	}
	m_highlight_clear = false;
}

void CUIActorMenuBase::highlight_armament( PIItem item, CUIDragDropListEx* ddlist )
{
	ddlist->clear_select_armament();
	highlight_ammo_for_weapon( item, ddlist );
	highlight_weapons_for_ammo( item, ddlist );
	highlight_weapons_for_addon( item, ddlist );
	highlight_related_config_sections(item, ddlist); // FFx001 ++
	highlight_antigas_for_filter(item, ddlist); // FFx001 ++
	highlight_power_banks_for_power_cell(item, ddlist); // FFx001 ++
	highlight_power_manager_for_power_cell(item, ddlist); // FFx001 ++
}

// FFx0001 ++
void CUIActorMenuBase::highlight_power_manager_for_power_cell(PIItem item, CUIDragDropListEx* ddlist)
{
	VERIFY(item);
	VERIFY(ddlist);

	if (PowerCell* oPowerCell = smart_cast<PowerCell*>(item->cast_inventory_item()))
	{
		u32 const cnt = ddlist->ItemsCount();
		for (u32 i = 0; i < cnt; ++i)
		{
			CUICellItem* ci = ddlist->GetItemIdx(i);
			PIItem _item = (PIItem)ci->m_pData;
			if (!_item)
			{
				continue;
			}

			if (IPowerManager* oPowerManager = smart_cast<IPowerManager*>(_item->cast_inventory_item()))
			{
				if (oPowerManager->IsPowerCellInWhiteList(item->m_section_id))
				{
					ci->m_select_armament = true;
				}
			}
		}
	}
}

// FFx0001 ++
void CUIActorMenuBase::highlight_power_banks_for_power_cell(PIItem item, CUIDragDropListEx* ddlist)
{
	VERIFY(item);
	VERIFY(ddlist);

	if (PowerCell* oPowerCell = smart_cast<PowerCell*>(item->cast_inventory_item()))
	{
		u32 const cnt = ddlist->ItemsCount();
		for (u32 i = 0; i < cnt; ++i)
		{
			CUICellItem* ci = ddlist->GetItemIdx(i);
			PIItem _item = (PIItem)ci->m_pData;
			if (!_item)
			{
				continue;
			}

			if (PowerBank* oPowerBank = smart_cast<PowerBank*>(_item->cast_inventory_item()))
			{
				if (oPowerBank->IsPowerCellInWhiteList(item->m_section_id))
				{
					ci->m_select_armament = true;
				}
			}
		}
	}
}

// FFx0001 ++
void CUIActorMenuBase::highlight_antigas_for_filter(PIItem item, CUIDragDropListEx* ddlist)
{
	VERIFY(item);
	VERIFY(ddlist);

	if (AntigasFilter* aFilter = smart_cast<AntigasFilter*>(item->cast_inventory_item()))
	{
		u32 const cnt = ddlist->ItemsCount();
		for (u32 i = 0; i < cnt; ++i)
		{
			CUICellItem* ci = ddlist->GetItemIdx(i);
			PIItem _item = (PIItem)ci->m_pData;
			if (!_item)
			{
				continue;
			}

			if (IAntigas* oAntigas = smart_cast<IAntigas*>(_item->cast_inventory_item()))
			{
				if (oAntigas->IsFilterInWhiteList(item->m_section_id))
				{
					ci->m_select_armament = true;
				}
			}
		}
	}
}

// FFx0001 ++
// Highlight separated by delimeter ',' related item sections on mouseover from the actor's inventory is item config include line highlight_related_sections with separated sections
void CUIActorMenuBase::highlight_related_config_sections(PIItem item, CUIDragDropListEx* ddlist)
{
	VERIFY(item);
	VERIFY(ddlist);

	if (!item->m_HiglightRelatedItemSections.empty())
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

void CUIActorMenuBase::highlight_ammo_for_weapon(PIItem weapon_item, CUIDragDropListEx* ddlist)
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

void CUIActorMenuBase::highlight_weapons_for_ammo(PIItem ammo_item, CUIDragDropListEx* ddlist)
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

bool CUIActorMenuBase::highlight_addons_for_weapon(PIItem weapon_item, CUICellItem* ci)
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

void CUIActorMenuBase::highlight_weapons_for_addon(PIItem addon_item, CUIDragDropListEx* ddlist)
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

void CUIActorMenuBase::highlight_item_slot(CUICellItem* cell_item)
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
	if ((slot_id == INV_SLOT_2 || slot_id == INV_SLOT_3) && !InventorySecondarySlotPairingStrict())
	{
		if (m_pInvSlotHighlight[INV_SLOT_2])
		{
			m_pInvSlotHighlight[INV_SLOT_2]->Show(true);
		}

		if (m_pInvSlotHighlight[INV_SLOT_3])
		{
			m_pInvSlotHighlight[INV_SLOT_3]->Show(true);
		}

		// Flex slot pair: also show dedicated holster highlight for BaseSlot INV_SLOT_2 (holster is separate UI).
		if (slot_id == INV_SLOT_2 && m_pInvSlotHighlight[PISTOL_SLOT_NEW])
		{
			m_pInvSlotHighlight[PISTOL_SLOT_NEW]->Show(true);
		}

		return;
	}

	// Equipped item: use current slot so holster (PISTOL_SLOT_NEW) highlights instead of BaseSlot INV_SLOT_2 only.
	if (item->CurrPlace() == eItemPlaceSlot && m_pInvSlotHighlight[item->CurrSlot()])
	{
		m_pInvSlotHighlight[item->CurrSlot()]->Show(true);
		return;
	}

	if (m_pInvSlotHighlight[slot_id])
	{
		m_pInvSlotHighlight[slot_id]->Show(true);
		return;
	}

	if (item->cast_eatable_item() != nullptr)
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

	if (item->cast_artefact() != nullptr)
	{
		if (cell_item->OwnerList() && GetListType(cell_item->OwnerList()) == iActorBelt)
		{
			return;
		}

		if (GetBeltList())
		{
			Ivector2 cap = GetBeltList()->CellsCapacity();
			for (u8 i = 0; i < cap.x; i++)
			{
				if (m_ArtefactSlotsHighlight[i])
					m_ArtefactSlotsHighlight[i]->Show(true);
			}
		}
		return;
	}
}

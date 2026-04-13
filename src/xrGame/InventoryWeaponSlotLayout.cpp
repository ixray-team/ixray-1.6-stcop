#include "StdAfx.h"
#include "InventoryWeaponSlotLayout.h"
#include "Inventory.h"
#include "inventory_item.h"
#include "../xrEngine/xr_level_controller.h"

bool InventorySecondarySlotPairingStrict()
{
	static bool isInitialized = false;
	static bool cachedValue = false;
	if (!isInitialized)
	{
		if (pSettings != nullptr)
		{
			cachedValue = !!READ_IF_EXISTS(
				pSettings,
				r_bool,
				"inventory",
				"inventory_secondary_slot_pairing_strict",
				false);
			isInitialized = true;
		}
		return false;
	}
	return cachedValue;
}

bool InventoryHolsterPistolSlotActiveInSettings()
{
	static bool isInitialized = false;
	static bool cachedValue = false;
	if (!isInitialized)
	{
		if (pSettings == nullptr)
		{
			return false;
		}
		cachedValue = !!READ_IF_EXISTS(
			pSettings,
			r_bool,
			"inventory",
			"slot_active_16",
			false);
		isInitialized = true;
	}
	return cachedValue;
}

bool InventoryHolsterExclusivePistolFootprint(CInventoryItem* item)
{
	if (item == nullptr)
	{
		return false;
	}
	// Binoculars use a weapon-style class and often share a 2x1 footprint with pistols; they belong in BINOCULAR_SLOT, not PISTOL_SLOT_NEW.
	if (item->cast_weapon_binoculars() != nullptr)
	{
		return false;
	}
	if (item->cast_weapon() == nullptr)
	{
		return false;
	}
	Irect const gr = item->GetInvGridRect();
	return gr.x2 == 2 && gr.y2 == 1;
}

u16 InventoryWeaponHotkeyToInventorySlot(u16 gameActionCmd)
{
	if (gameActionCmd == kWPN_7)
	{
		return PISTOL_SLOT_NEW;
	}
	if (gameActionCmd >= kWPN_1 && gameActionCmd <= kWPN_6)
	{
		return (u16)(gameActionCmd - kWPN_1 + 1u);
	}
	return NO_ACTIVE_SLOT;
}

bool InventoryAnySidearmCellOccupied(const CInventory& inv)
{
	return inv.ItemFromSlot(INV_SLOT_2) != nullptr || inv.ItemFromSlot(PISTOL_SLOT_NEW) != nullptr;
}


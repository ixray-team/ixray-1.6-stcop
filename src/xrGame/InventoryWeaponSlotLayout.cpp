#include "stdafx.h"
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
		if (pSettings == nullptr)
		{	
			return false;
		}
		cachedValue = 
			pSettings->read_if_exists<bool>(
			"inventory",
			"inventory_secondary_slot_pairing_strict",
			false);
		isInitialized = true;
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
		cachedValue = 
			pSettings->read_if_exists<bool>(
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
	// Knives belong in KNIFE_SLOT, not PISTOL_SLOT_NEW, even when their inventory footprint matches pistols.
	if (item->cast_weapon_knife() != nullptr)
	{
		return false;
	}
	if (item->cast_weapon() == nullptr)
	{
		return false;
	}
	return item->BaseSlot() == INV_SLOT_2;
}

u16 InventoryResolveSidearmEquipSlot(CInventoryItem* item)
{
	VERIFY(item);
	u16 const baseSlot = item->BaseSlot();
	if (!InventoryHolsterPistolSlotActiveInSettings())
	{
		return baseSlot;
	}
	if (!InventoryHolsterExclusivePistolFootprint(item))
	{
		return baseSlot;
	}
	return PISTOL_SLOT_NEW;
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

namespace
{
xr_vector<u16> g_weaponCycleSlots;
bool g_weaponCycleLoaded = false;

void BuildDefaultWeaponCycleSlots()
{
	g_weaponCycleSlots.clear();
	g_weaponCycleSlots.push_back(KNIFE_SLOT);
	if (InventoryHolsterPistolSlotActiveInSettings())
	{
		g_weaponCycleSlots.push_back(PISTOL_SLOT_NEW);
	}
	g_weaponCycleSlots.push_back(INV_SLOT_2);
	g_weaponCycleSlots.push_back(INV_SLOT_3);
	g_weaponCycleSlots.push_back(GRENADE_SLOT);
	g_weaponCycleSlots.push_back(ARTEFACT_SLOT);
}

void LoadWeaponCycleSlots()
{
	g_weaponCycleSlots.clear();

	if (pSettings != nullptr && pSettings->line_exist("inventory", "weapon_cycle_slots"))
	{
		const char* slotList = pSettings->r_string("inventory", "weapon_cycle_slots");
		for (int i = 0, itemCount = _GetItemCount(slotList); i < itemCount; ++i)
		{
			string512 slotToken = {};
			_GetItem(slotList, i, slotToken);
			const int slotId = atoi(slotToken);
			if (slotId >= KNIFE_SLOT && slotId <= LAST_SLOT)
			{
				g_weaponCycleSlots.push_back(static_cast<u16>(slotId));
			}
		}
	}

	if (g_weaponCycleSlots.empty())
	{
		BuildDefaultWeaponCycleSlots();
	}

	g_weaponCycleLoaded = true;
}
} // namespace

void InventoryWeaponCycleInvalidate()
{
	g_weaponCycleLoaded = false;
	g_weaponCycleSlots.clear();
}

xr_span<const u16> InventoryWeaponCycleSlots()
{
	if (!g_weaponCycleLoaded)
	{
		LoadWeaponCycleSlots();
	}
	return xr_span<const u16>(g_weaponCycleSlots.data(), g_weaponCycleSlots.size());
}

u16 InventoryWeaponSlotToGameAction(u16 slotId)
{
	if (slotId == PISTOL_SLOT_NEW)
	{
		return kWPN_7;
	}
	if (slotId == ARTEFACT_SLOT)
	{
		return kARTEFACT;
	}
	if (slotId >= KNIFE_SLOT && slotId <= BOLT_SLOT)
	{
		return static_cast<u16>(kWPN_1 + (slotId - KNIFE_SLOT));
	}
	return kWeaponCycleNoGameAction;
}


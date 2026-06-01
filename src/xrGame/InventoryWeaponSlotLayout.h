#pragma once

#include "../xrServerEntities/inventory_space.h"

bool InventorySecondarySlotPairingStrict();

inline bool IsSidearmPhysicalSlot(u16 slot)
{
	return slot == INV_SLOT_2 || slot == PISTOL_SLOT_NEW;
}

inline bool IsPrimaryPhysicalSlot(u16 slot)
{
	return slot == INV_SLOT_3;
}

inline bool IsSidearmOrPrimaryPhysicalSlot(u16 slot)
{
	return IsSidearmPhysicalSlot(slot) || IsPrimaryPhysicalSlot(slot);
}

u16 InventoryWeaponHotkeyToInventorySlot(u16 gameActionCmd);

class CInventory;
class CInventoryItem;
bool InventoryAnySidearmCellOccupied(const CInventory& inv);

bool InventoryHolsterPistolSlotActiveInSettings();

bool InventoryHolsterExclusivePistolFootprint(CInventoryItem* item);

u16 InventoryResolveSidearmEquipSlot(CInventoryItem* item);


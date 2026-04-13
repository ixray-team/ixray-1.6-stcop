#pragma once

#include "../xrServerEntities/inventory_space.h"

// Sidearm / primary slot layout; UI pairing for slots 2-3: [inventory] inventory_secondary_slot_pairing_strict.

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

// kWPN_1..kWPN_6 map to inventory slots 1..6; kWPN_7 maps to PISTOL_SLOT_NEW.
u16 InventoryWeaponHotkeyToInventorySlot(u16 gameActionCmd);

class CInventory;
class CInventoryItem;
bool InventoryAnySidearmCellOccupied(const CInventory& inv);

// True when [inventory] slot_active_16 enables the dedicated holster slot (PISTOL_SLOT_NEW).
bool InventoryHolsterPistolSlotActiveInSettings();

// Standard pistols use a 2x1 inventory footprint (inv_grid stored in Irect rb; see UICellCustomItems).
bool InventoryHolsterExclusivePistolFootprint(CInventoryItem* item);


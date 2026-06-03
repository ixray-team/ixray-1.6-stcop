#include "stdafx.h"
#include "UICellItemFactory.h"
#include "UICellCustomItems.h"

CUICellItem* create_cell_item(CInventoryItem* itm)
{
	VERIFY(itm);
	CUICellItem* cell_item = nullptr;

	if (CWeaponAmmo* pAmmo = itm->cast_weapon_ammo())
	{
		cell_item = new CUIAmmoCellItem(pAmmo);
	}
	else if (CWeapon* pWeapon = itm->cast_weapon())
	{
		cell_item = new CUIWeaponCellItem(pWeapon);
	}
	else
	{
		cell_item = new CUIInventoryCellItem(itm);
	}

	return cell_item;
}

#pragma once

#include "UIRadialMenu.h"

class CInventoryItem;
class CInventory;

class CUIRadialMenuWeapon : public CUIRadialMenu
{
	typedef CUIRadialMenu		inherited;

public:
	CUIRadialMenuWeapon();
	virtual ~CUIRadialMenuWeapon();

	virtual void Draw();
	virtual void Update();
	virtual void Init();
	virtual void TryActivateSelectedSector();

	virtual bool OnGamepadKeyAction(int dik, EUIMessages keyboard_action);

	virtual bool StopAnyMove() { return false; }
	virtual bool NeedCursor()const { return false; }
	virtual bool NeedCenterCursor()const { return false; }

	void UpdateGamepadLegend();

protected:
	virtual CInventoryItem* GetInventorySlotFromSector(CInventory& inventory, u32 sector_index);
	virtual u16 GetSlotIdInSector(u16 sector_index);
	CInventoryItem* GetSelectedItem(RadialMenuItem itm);
};
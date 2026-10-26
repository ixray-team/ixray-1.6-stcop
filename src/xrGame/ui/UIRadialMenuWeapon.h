#pragma once

#include "UIRadialMenu.h"

class CInventoryItem;
class CInventory;

class CUIRadialMenuWeapon : public CUIRadialMenu
{
	typedef CUIRadialMenu		inherited;

public:
	CUIRadialMenuWeapon();
	virtual ~CUIRadialMenuWeapon() override;

	virtual void Draw() override;
	virtual void Update() override;
	virtual void Init() override;
	virtual void TryActivateSelectedSector() override;

	virtual bool OnGamepadKeyAction(int dik, EUIMessages keyboard_action) override;

	virtual bool StopAnyMove() override { return false; }
	virtual bool NeedCursor()const override { return false; }
	virtual bool NeedCenterCursor()const override { return false; }

	void UpdateGamepadLegend();

protected:
	virtual CInventoryItem* GetInventorySlotFromSector(CInventory& inventory, u32 sector_index);
	virtual u16 GetSlotIdInSector(u16 sector_index);
	CInventoryItem* GetSelectedItem(RadialMenuItem itm);
};
#pragma once

class CUIOwnerPropertiesBox;

#include "UIDragDropListEx.h"
#include "ui_drop_amount.h"
#include "../inventory_item.h"

#include "../../xrUI/Widgets/UIWindow.h"

class CUIPropertiesBox;
class CUIWindow;
class CUIActorMenu;
class CInventory;
class CInventoryItem;
class CInventoryOwner;
class CInventoryBox;

const u64 INVENTORY_ALL_CODE = 33;
const u64 INVENTORY_AMOUNT_CODE = 77;

enum eActorMenuControllerAuxMode {
	eAuxMode_None = 0,
	eAuxMode_Upgrade,
	eAuxMode_QuickSlot,
	eAuxMode_BeltSlot
};

class CUIOwnerPropertiesBox
{
protected:
	virtual	void				PropertiesBoxForWeapon		(CUICellItem* cell_item, PIItem item, bool& b_show);
	virtual	void				PropertiesBoxForAddon		(PIItem item, bool& b_show);
	virtual	void				PropertiesBoxForUsing		(PIItem item, bool& b_show);
	virtual	void				PropertiesBoxForPlaying		(PIItem item, bool& b_show);
	virtual	void				PropertiesBoxForDrop		(CUICellItem* cell_item, PIItem item, bool& b_show);
	virtual	void				PropertiesBoxForSlots		(CUICellItem* cell_item, PIItem item, bool& b_show);
	virtual	void				PropertiesBoxForParse		(PIItem item, bool& b_show);

	virtual	void				TryHidePropertiesBox		();

public:
	CUIPropertiesBox*			m_UIPropertiesBox;
	virtual CInventory*			GetInventory				() { return nullptr; }
	virtual CInventoryOwner*	GetInventoryOwner			() { return nullptr; }
};

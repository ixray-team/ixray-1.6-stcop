#include "StdAfx.h"
#include "UIRadialMenuWeapon.h"

#include "../Actor.h"
#include "../Inventory.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../CustomDevice.h"
#include "../player_hud.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/Widgets/UI3dStatic.h"
#include "UIInventoryUtilities.h"

#define RADIAL_MENU_XML "radial_menu.xml"

CUIRadialMenuWeapon::CUIRadialMenuWeapon()
{
	Init();
}
CUIRadialMenuWeapon::~CUIRadialMenuWeapon()
{
}

void CUIRadialMenuWeapon::Init()
{
	CUIXml					uiXml;
	if (uiXml.Load(CONFIG_PATH, UI_PATH, RADIAL_MENU_XML))
	{
		inherited::Init(&uiXml);
	}
}

bool CUIRadialMenuWeapon::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	if (inherited::OnGamepadKeyAction(key, gamepad_action)) 
		return true;

	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		CActor* owner = g_pGameLevel->CurrentViewEntity()->cast_actor();
		switch (get_binded_action(key, agUIRadialWeapon))
		{
			case kNIGHT_VISION:
			{
				owner->SwitchNightVision();
				return true;
			}
			case kWPN_NEXT:
			{
				owner->inventory().Action(kWPN_NEXT, CMD_START);
				return true;
			}
			case kWPN_FIREMODE_NEXT:
			{
				owner->inventory().Action(kWPN_FIREMODE_NEXT, CMD_START);
				PlaySnd(eSndFireMode);
				return true;
			}
			case kWPN_FUNC:
			{
				owner->inventory().Action(kWPN_FUNC, CMD_START);
				PlaySnd(eSndGrenadeMode);
				return true;
			}
			case kDROP:
			{
				owner->g_PerformDrop();
				return true;
			}
		}
	}

	return false;
}

void CUIRadialMenuWeapon::TryActivateSelectedSector()
{
	CActor* owner = g_pGameLevel->CurrentViewEntity()->cast_actor();
	CInventory& inventory = owner->inventory();

	if (selected_index == -1)
		return;

	u16 slotId = GetSlotIdInSector(selected_index);

	PlaySnd(eSndSelect);

	if (slotId == DEVICE_SLOT)
	{
		PIItem det_active = inventory.ItemFromSlot(slotId);
		if (det_active)
		{
			if (CCustomDevice* det = det_active->cast_custom_device())
				det->switch_device();
		}
	}
	else if (slotId != NO_ACTIVE_SLOT)
	{
		PIItem item = inventory.ItemFromSlot(slotId);

		// Dont switch if weapon are blocked
		if (item && !inventory.IsSlotBlocked(item) && inventory.ActiveItem() != item)
			inventory.ActiveWeapon(slotId);
	}
	else
	{
		// HANDS

		// turn off device (detector/flashlight)
		PIItem det_active = inventory.ItemFromSlot(DEVICE_SLOT);
		if (det_active)
		{
			CCustomDevice* det = det_active->cast_custom_device();
			if (det && det->GetState() == CCustomDevice::eIdle)
				det->switch_device();
		}

		inventory.Activate(NO_ACTIVE_SLOT);
	}
}

u16 CUIRadialMenuWeapon::GetSlotIdInSector(u16 sector_index)
{
	if (sector_index >= 0 && sector_index < sectors_count)
		return slotsInSectors[sector_index];

	return NO_ACTIVE_SLOT;
}

CInventoryItem* CUIRadialMenuWeapon::GetInventorySlotFromSector(CInventory& inventory, u32 sector_index)
{
	CInventoryItem* item = inventory.ItemFromSlot(GetSlotIdInSector(sector_index));
	return item;
}

void	CUIRadialMenuWeapon::Draw()
{
	CActor* owner = g_pGameLevel->CurrentViewEntity()->cast_actor();

	if (owner)
	{
		inherited::Draw();
		for (u32 i = 0; i < sectors_count; ++i)
		{
			CInventoryItem* item = GetSelectedItem(i);
			slotBackgrounds[i]->Draw();
			if (item)
			{
				Irect grect = item->GetInvGridRect();
				if (grect.y2 > 0 && grect.x2 > 0)
				{
					slotIcons[i]->SetShader(InventoryUtilities::GetEquipmentIconsShader(item->IconsTexture.c_str()));

					float scaleIcon = item->ScaleIcon;
					Frect texture_rect = {};
					texture_rect.lt.set(grect.x1 * INV_GRID_WIDTH(scaleIcon), grect.y1 * INV_GRID_HEIGHT(scaleIcon));
					texture_rect.rb.set(grect.x2 * INV_GRID_WIDTH(scaleIcon), grect.y2 * INV_GRID_HEIGHT(scaleIcon));
					texture_rect.rb.add(texture_rect.lt);
					slotIcons[i]->SetTextureRect(texture_rect);

					shared_str sect_name = item->object().cNameSect();
					InventoryUtilities::InventoryIconParams icons_struct = InventoryUtilities::GetInventoryIconParams(sect_name.c_str());
					if (psActorFlags.test(AF_3D_ICONS_INV))
					{
						slotIcons[i]->SetVisual(icons_struct._3d_static_visual);
						slotIcons[i]->SetXYZ(icons_struct._3d_static_rotate);
						slotIcons[i]->SetScaleFactor(icons_struct._3d_static_scale);
						slotIcons[i]->SetBonesVisible(item->object().Visual()->dcast_PKinematics());
					}
					else
					{
						slotIcons[i]->SetVisual(nullptr);
					}

					Fvector2 v_r = {grect.x2 * slotIconDefaultSizes[i].x, grect.y2 * slotIconDefaultSizes[i].y};
					v_r.x *= UI().get_current_kx();

					if (grect.x2 < 5 && grect.y2 == 1)
					{
						v_r.mul(2.f);
					}

					slotIcons[i]->GetUIStaticItem().SetSize(v_r);
					slotIcons[i]->SetWidth(v_r.x);
					slotIcons[i]->SetHeight(v_r.y);

					CInventory& inventory = owner->inventory();
					if (!inventory.IsSlotBlocked(item) || GetSlotIdInSector(i) == DEVICE_SLOT)
					{
						slotIcons[i]->SetTextureColor(clrSlotIcon);
					}
					else
					{
						slotIcons[i]->SetTextureColor(clrSlotIconBlocked);
					}

				}
			}
			else
			{
				slotIcons[i]->SetWidth(slotIconDefaultSizes[i].x * UI().get_current_kx());
				slotIcons[i]->SetHeight(slotIconDefaultSizes[i].y);
			}
			slotIcons[i]->Draw();
		}
	}
}

CInventoryItem* CUIRadialMenuWeapon::GetSelectedItem(u32 it)
{
	CActor* owner = g_pGameLevel->CurrentViewEntity()->cast_actor();
	CInventory& inventory = owner->inventory();
	u16 activeSlot = inventory.GetActiveSlot();
	CInventoryItem* item = nullptr;
	CCustomDevice* dev = nullptr;
	if (inventory.ItemFromSlot(DEVICE_SLOT))
	{
		dev = inventory.ItemFromSlot(DEVICE_SLOT)->cast_custom_device();
	}

	u32 slotId = GetSlotIdInSector(it);
	shared_str backTexture = textureDefault;
	bool isSelected = selected_index == it;
	const auto chooseBackground = [&]()
	{
		if (isSelected)
		{
			return textureFocusedSelected;
		}
		else
		{
			return textureFocused;
		}
	};

	if (slotId == NO_ACTIVE_SLOT) // Empty hands
	{
		if (activeSlot == NO_ACTIVE_SLOT && (!dev || !dev->IsWorking()))
		{
			backTexture = chooseBackground();
		}
	}
	else
	{
		item = inventory.ItemFromSlot(slotId);
		if (item)
		{
			if (slotId == DEVICE_SLOT)
			{
				if (dev->IsWorking())
				{
					backTexture = chooseBackground();
				}
			}
			else if (slotId == activeSlot)
			{
				backTexture = chooseBackground();
			}
		}
	}
	bool isBlocked = false;
	if (slotId != NO_ACTIVE_SLOT)
	{
		CInventoryItem* iitem = inventory.ItemFromSlot(slotId);
		isBlocked = iitem && inventory.IsSlotBlocked(iitem);
	}
	if (backTexture == textureDefault && isSelected && !isBlocked)
	{
		backTexture = textureSelected;
	}
	slotBackgrounds[it]->InitTexture(backTexture.c_str());
	return item;
}

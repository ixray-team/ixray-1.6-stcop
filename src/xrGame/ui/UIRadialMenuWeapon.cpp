#include "StdAfx.h"
#include "UIRadialMenuWeapon.h"

#include "../Actor.h"
#include "../Inventory.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../CustomDevice.h"
#include "../player_hud.h"
#include "../../xrEngine/xr_input.h"
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
		inherited::Init(&uiXml);
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
				return true;
			}
			case kWPN_FUNC:
			{
				owner->inventory().Action(kWPN_FUNC, CMD_START);
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

		CInventory& inventory = owner->inventory();
		u16 activeSlot = inventory.GetActiveSlot();
		CCustomDevice* dev = inventory.ItemFromSlot(DEVICE_SLOT)->cast_custom_device();

		float current_angle = starting_angle;

		const u32 clrSlotIcon = 0xAAFFFFFF;
		const u32 clrSlotIconBlocked = 0x55FFFFFF;

		for (int i = 0; i < sectors_count; ++i) 
		{
			CInventoryItem *item = nullptr;
			u32 slotId = this->GetSlotIdInSector(i);
			u32 color1 = sector_inner_side_color;

			if (slotId == NO_ACTIVE_SLOT) // Empty hands
			{
				if (activeSlot == NO_ACTIVE_SLOT && (!dev || !dev->IsWorking()))
					color1 = selected_color;
			}
			else
			{
				item = inventory.ItemFromSlot(slotId);
				if (item)
				{
					if (slotId == DEVICE_SLOT)
					{
						if (dev->IsWorking())
							color1 = selected_color;
					}
					else if (slotId == activeSlot)
						color1 = selected_color;
				}
			}

			// Sector backpad
			draw_arc(*crosshair_shader, center_x, center_y, radius, inner_radius, current_angle, current_angle + sector, color1, sector_outer_side_color);
			current_angle += sector + gap;
			
			if (item)
			{
				Irect grect = item->GetInvGridRect();
				if (grect.y2 > 0 && grect.x2 > 0)
				{
					// Draw item icon
					TexturedRectDrawData trdd;
					trdd.side = radius / 4;

					UIRender->SetShader(*InventoryUtilities::GetEquipmentIconsShader(item->IconsTexture.c_str()));
					UIRender->StartPrimitive(6, IUIRender::ptTriList, IUIRender::ePointType::pttTL);//6*8

					const float angle = starting_angle + sector/2 + 2 * M_PI * i / float(sectors_count);

					const float r = inner_radius + screen_height / 12.f;
					
					trdd.x = center_x + cos(angle) * r;
					trdd.y = center_y + sin(angle) * r;

					if (!inventory.IsSlotBlocked(item) || slotId == DEVICE_SLOT)
						DrawItem(trdd, item, clrSlotIcon);
					else
						DrawItem(trdd, item, clrSlotIconBlocked);

					UIRender->FlushPrimitive();
				}
			}
		}

		// Draw current sector selector
		if (selected_index != -1)
		{
			float selected_angle = starting_angle + selected_index * (2 * M_PI / sectors_count);
			u32 slotId = this->GetSlotIdInSector(selected_index);

			u32 clr = deselected_color;
			if (slotId == NO_ACTIVE_SLOT) // Empty hands
			{
				clr = selected_color;
			}
			else
			{
				CInventoryItem *item = inventory.ItemFromSlot(slotId);
				if (item && !inventory.IsSlotBlocked(item))
					clr = selected_color;
			}

			draw_arc(*crosshair_shader, center_x, center_y, selected_radius, inner_radius, selected_angle, selected_angle + sector, clr, clr);
		}
	}
}



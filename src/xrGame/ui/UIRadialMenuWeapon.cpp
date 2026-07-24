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

		CInventory& inventory = owner->inventory();
		u16 activeSlot = inventory.GetActiveSlot();
		CCustomDevice* dev = nullptr;
		if (inventory.ItemFromSlot(DEVICE_SLOT))
			dev = inventory.ItemFromSlot(DEVICE_SLOT)->cast_custom_device();

		float current_angle = starting_angle;

		const u32 clrSlotIcon = 0xAAFFFFFF;
		const u32 clrSlotIconBlocked = 0x55FFFFFF;

		for (int i = 0; i < sectors_count; ++i)
		{
			int backIterator = sectors_count - i - 3;
			if (backIterator < 0)
			{
				backIterator += sectors_count;
			}
			float angle_back = PI_MUL_2 * (float(backIterator) / float(sectors_count));

			Fvector2 backSize{240.f, 240.f};
			slotBackgrounds[backIterator]->SetWndSize(backSize);

			Fvector2 pivot{slotBackgrounds[backIterator]->GetWidth() / 2.f, slotBackgrounds[backIterator]->GetHeight() * 1.5f};
			Fvector2 offset{UI_BASE_WIDTH, UI_BASE_HEIGHT};
			offset.mad(offset, pivot, -2.0f);
			offset.div(2.0f);

			slotBackgrounds[backIterator]->SetHeadingPivot(pivot, offset, false);
			slotBackgrounds[backIterator]->SetHeading(angle_back);

			CInventoryItem* item = nullptr;
			u32 slotId = GetSlotIdInSector(i);
			u32 color1 = sector_inner_side_color;

			shared_str backTexture = textureDefault;
			bool isSelected = selected_index == i;
			if (slotId == NO_ACTIVE_SLOT) // Empty hands
			{
				if (activeSlot == NO_ACTIVE_SLOT && (!dev || !dev->IsWorking()))
				{
					if (isSelected)
					{
						backTexture = textureFocusedSelected;
					}
					else
					{
						backTexture = textureFocused;
					}
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
							if (isSelected)
							{
								backTexture = textureFocusedSelected;
							}
							else
							{
								backTexture = textureFocused;
							}
						}
					}
					else if (slotId == activeSlot)
					{
						if (isSelected)
						{
							backTexture = textureFocusedSelected;
						}
						else
						{
							backTexture = textureFocused;
						}
					}
				}
			}
			bool isBlocked = false;
			if (slotId != NO_ACTIVE_SLOT)
			{
				CInventoryItem* item = inventory.ItemFromSlot(slotId);
				isBlocked = item && inventory.IsSlotBlocked(item);
			}
			if (backTexture == textureDefault && isSelected && !isBlocked)
			{
				backTexture = textureSelected;
			}
			slotBackgrounds[backIterator]->InitTexture(backTexture.c_str());
			slotBackgrounds[backIterator]->Draw();
			current_angle += sector + gap;
			
			if (item)
			{
				Irect grect = item->GetInvGridRect();
				if (grect.y2 > 0 && grect.x2 > 0)
				{
					// Draw item icon
					TexturedRectDrawData trdd;
					trdd.width = trdd.height = radius / 3;
					trdd.width *= UI().get_current_kx();

					slotIcons[i]->SetShader(InventoryUtilities::GetEquipmentIconsShader(item->IconsTexture.c_str()));

					Irect item_grid_rect = item->GetInvGridRect();
					float scaleIcon = item->ScaleIcon;
					Frect texture_rect = {};
					texture_rect.lt.set(item_grid_rect.x1 * INV_GRID_WIDTH(scaleIcon), item_grid_rect.y1 * INV_GRID_HEIGHT(scaleIcon));
					texture_rect.rb.set(item_grid_rect.x2 * INV_GRID_WIDTH(scaleIcon), item_grid_rect.y2 * INV_GRID_HEIGHT(scaleIcon));
					texture_rect.rb.add(texture_rect.lt);
					slotIcons[i]->SetTextureRect(texture_rect);

					if (item_grid_rect.x2 == 1)
					{
						trdd.width *= 0.6f;
						trdd.height *= 0.6f;
					}
					const float angle = starting_angle + sector / 2 + 2 * M_PI * i / float(sectors_count);

					const float r = inner_radius + UI_BASE_HEIGHT / 12.f;

					trdd.x = center_x + (cos(angle) * r * UI().get_current_kx());
					trdd.y = center_y + sin(angle) * r;

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

					if (!inventory.IsSlotBlocked(item) || slotId == DEVICE_SLOT)
					{
						DrawItem(slotIcons[i], trdd, clrSlotIcon);
					}
					else
					{
						DrawItem(slotIcons[i], trdd, clrSlotIconBlocked);
					}
				}
			}
			// separate icon for empty hands
			else if (slotId == NO_ACTIVE_SLOT)
			{
				// Draw item icon
				TexturedRectDrawData trdd;
				trdd.width = trdd.height = radius / 3;
				trdd.width *= UI().get_current_kx();

				slotIcons[i]->InitTexture(emptyIconName.c_str());

				const float angle = starting_angle + sector / 2 + 2 * M_PI * i / float(sectors_count);

				const float r = inner_radius + UI_BASE_HEIGHT / 12.f;

				trdd.x = center_x + (cos(angle) * r * UI().get_current_kx());
				trdd.y = center_y + sin(angle) * r;

				DrawItem(slotIcons[i], trdd, clrSlotIcon);
			}
		}
	}
}



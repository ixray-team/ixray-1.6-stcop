#include "stdafx.h"
#include "GamepadService.h"
#include "xr_input.h"

#ifdef IXR_WINDOWS
#	include <hidapi.h>
#endif

ENGINE_API CGamepadService* GGamepadService = nullptr;

CGamepadService::CGamepadService()
{
	InitHID();
}

void CGamepadService::InitHID()
{
#ifdef IXR_WINDOWS
	hid_init();

	hid_device_info* Devices = hid_enumerate(0, 0);

	for (hid_device_info* CurrentDevice = Devices; CurrentDevice; CurrentDevice = CurrentDevice->next)
	{
		if (CurrentDevice->vendor_id != 0x054C)
		{
			continue;
		}

		switch (CurrentDevice->product_id)
		{
			case 0x05C4: // DualShock 4 v1
			case 0x09CC: // DualShock 4 v2
			{
				HidDevice = hid_open_path(CurrentDevice->path);

				if (HidDevice)
				{
					Msg("~Opened DualShock 4 (%04X:%04X)", CurrentDevice->vendor_id, CurrentDevice->product_id);

					hid_free_enumeration(Devices);
					return;
				}

				break;
			}
		}
	}

	hid_free_enumeration(Devices);
#endif
}

CGamepadService::~CGamepadService()
{
#ifdef IXR_WINDOWS
	if (HidDevice != nullptr)
	{
		hid_close((hid_device*)HidDevice);
		HidDevice = nullptr;
	}

	hid_exit();
#endif
}

void CGamepadService::UpdateLEDByHP(float Health)
{
	if (pInput->pGamePad == nullptr || HidDevice == nullptr)
	{
		return;
	}

	Health = std::clamp(Health, 0.f, 1.f);

	u8 Red;
	u8 Green;
	u8 Blue = 0;

	if (Health >= 0.5f)
	{
		float ratio = (Health - 0.5f) / 0.5f; // 0..1
		Red = (u8)((1.0f - ratio) * 255);
		Green = 255;
	}
	else
	{
		float ratio = Health / 0.5f; // 0..1
		Red = 255;
		Green = (u8)(ratio * 255);
	}

#ifdef IXR_WINDOWS
	SetLED(Red, Green, Blue);
#else
	SDL_SetGamepadLED(pInput->pGamePad, Red, Green, Blue)
#endif
}

void CGamepadService::SetLED(u8 Red, u8 Green, u8 Blue)
{
#ifdef IXR_WINDOWS
	if (HidDevice == nullptr)
	{
		return;
	}

	u8 Report[32] = {};

	Report[0] = 0x05; // Output Report ID
	Report[1] = 0xFF; // Enable rumble + lightbar

	// Rumble
	Report[4] = 0;
	Report[5] = 0;

	// Light Bar
	Report[6] = Red;
	Report[7] = Green;
	Report[8] = Blue;

	const int Result = hid_write((hid_device*)HidDevice, Report, sizeof(Report));

	if (Result < 0)
	{
		Msg("hid_write failed: %ls", hid_error((hid_device*)HidDevice));
	}
#endif
}
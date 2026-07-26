#include "stdafx.h"
#include "GamepadService.h"
#include "xr_input.h"

#ifdef IXR_WINDOWS
#	include <hidapi.h>
#endif

ENGINE_API CGamepadService* GGamepadService = nullptr;

bool CGamepadService::GamepadFeedbackMode = false;

CGamepadService::CGamepadService()
{
	InitHID();
}

CGamepadService::~CGamepadService()
{
	ClearTriggerEffect(true);
	ClearTriggerEffect(false);
	SetLED(0, 0, 255);

	DestroyHID();

	if (GamePadDevice != nullptr)
	{
		SDL_CloseGamepad(GamePadDevice);
	}
}

void CGamepadService::InitHID()
{
#ifdef IXR_WINDOWS
	hid_init();
#endif
}

void CGamepadService::FindHIDDevice()
{
#ifdef IXR_WINDOWS
	hid_device_info* Devices = hid_enumerate(0, 0);

	for (hid_device_info* CurrentDevice = Devices; CurrentDevice; CurrentDevice = CurrentDevice->next)
	{
		if (CurrentDevice->vendor_id != 0x054C)
		{
			continue;
		}

		EGamepadType DetectedType = EGamepadType::Unknown;

		switch (CurrentDevice->product_id)
		{
			case 0x05C4: // DualShock 4 v1
			case 0x09CC: // DualShock 4 v2
				DetectedType = EGamepadType::DualShock4;
				break;

			case 0x0CE6: // DualSense
				DetectedType = EGamepadType::DualSense;
				break;

			default:
				continue;
		}

		HidDevice = hid_open_path(CurrentDevice->path);

		if (HidDevice)
		{
			Type = DetectedType;

			Msg("~Opened %s (%04X:%04X)",
				Type == EGamepadType::DualShock4 ? "DualShock 4" : "DualSense",
				CurrentDevice->vendor_id,
				CurrentDevice->product_id);

			hid_free_enumeration(Devices);
			return;
		}
	}

	hid_free_enumeration(Devices);
#endif
}

void CGamepadService::ResetHID()
{
#ifdef IXR_WINDOWS
	if (HidDevice != nullptr)
	{
		hid_close((hid_device*)HidDevice);
		HidDevice = nullptr;
	}
	Type = EGamepadType::Unknown;
#endif
}

void CGamepadService::DestroyHID()
{
#ifdef IXR_WINDOWS
	ResetHID();
	hid_exit();
#endif
}

void CGamepadService::UpdateLEDByHP(float Health)
{
	if (GamePadDevice == nullptr && HidDevice == nullptr)
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

	SetLED(Red, Green, Blue);
}

void CGamepadService::SetLED(u8 Red, u8 Green, u8 Blue)
{
#ifdef IXR_WINDOWS
	if (Type == EGamepadType::DualShock4)
	{
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
	}
	else
#endif
	if (GamePadDevice)
	{
		SDL_SetGamepadLED(GamePadDevice, Red, Green, Blue);
	}
}

void CGamepadService::SetTriggerResistance(bool RightTrigger, u8 StartPosition, u8 Force)
{
#ifdef IXR_WINDOWS
	if (Type != EGamepadType::DualSense || HidDevice == nullptr)
	{
		return;
	}

	StartPosition = std::min(StartPosition, (u8)9);
	Force = std::min(Force, (u8)8);

	u8 Report[48] = {};

	Report[0] = 0x02;

	// Enable trigger effect
	Report[1] = 0x04;

	u8* Trigger = RightTrigger ? &Report[11] : &Report[22];

	Trigger[0] = 0x01; // Resistance mode
	Trigger[1] = StartPosition;
	Trigger[2] = Force;

	hid_write((hid_device*)HidDevice, Report, sizeof(Report));
#endif
}

void CGamepadService::ClearTriggerEffect(bool RightTrigger)
{
#ifdef IXR_WINDOWS
	if (Type != EGamepadType::DualSense || HidDevice == nullptr)
	{
		return;
	}

	u8 Report[48] = {};

	Report[0] = 0x02;
	Report[1] = 0x04;

	u8* Trigger = RightTrigger ? &Report[11] : &Report[22];

	Trigger[0] = 0x00;

	hid_write((hid_device*)HidDevice, Report, sizeof(Report));
#endif
}

bool CGamepadService::Rumble(u16 LFRumble, u16 HFRumble, u16 DurationMS)
{
	if (GamePadDevice == nullptr)
	{
		return false;
	}

	if (CGamepadService::GamepadFeedbackMode)
	{
		return SDL_RumbleGamepad(GamePadDevice, LFRumble, HFRumble, DurationMS);
	}
	return false;
}

shared_str CGamepadService::GetGamepadPrefix() const
{
	shared_str Prefix;

	// FX: Support DS4Windows and DualSenceX wrappers
	if (Type == EGamepadType::DualShock4)
	{
		Prefix = "ps4";
		return Prefix;
	}
	else if (Type == EGamepadType::DualSense)
	{
		Prefix = "ps5";
		return Prefix;
	}

	switch (SDL_GetGamepadType(GamePadDevice))
	{
		case SDL_GAMEPAD_TYPE_PS3:
		case SDL_GAMEPAD_TYPE_PS4:
		{
			Prefix = "ps4";
			break;
		}
		case SDL_GAMEPAD_TYPE_PS5:
		{
			Prefix = "ps5";
			break;
		}
		case SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_PRO:
		case SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_LEFT:
		case SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT:
		case SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_PAIR:
		{
			Prefix = "switch";
			break;
		}
		default: // Use Xbox prefix as a fallback
		{
			Prefix = "xbox1";
			break;
		}
	}

	return Prefix;
}

void CGamepadService::SetTriggerResistance(bool RightTrigger, u8 StartPosition, u8 Force, float Time)
{
	SetTriggerResistance(RightTrigger, StartPosition, Force);
	if (RightTrigger)
	{
		triggerEffectTimeR = Time * 1000;
		triggerEffectTimeStampR = Device.dwTimeContinual;
	}
	else
	{
		triggerEffectTimeL = Time * 1000;
		triggerEffectTimeStampL = Device.dwTimeContinual;
	}
}

void CGamepadService::ShotTriggerEffect(bool RightTrigger, float Time)
{
	if (Type != EGamepadType::DualSense || HidDevice == nullptr)
	{
		return;
	}

	u8 Report[48] = {};

	Report[0] = 0x02;

	// Enable trigger effect
	Report[1] = 0x04;

	u8* Trigger = RightTrigger ? &Report[11] : &Report[22];

	Trigger[0] = 0x25; // Shot mode
	Trigger[1] = 0x20;
	Trigger[2] = 0x1;
	Trigger[3] = 0x7;

	hid_write((hid_device*)HidDevice, Report, sizeof(Report));

	if (RightTrigger)
	{
		triggerEffectTimeR = Time * 1000;
		triggerEffectTimeStampR = Device.dwTimeContinual;
	}
	else
	{
		triggerEffectTimeL = Time * 1000;
		triggerEffectTimeStampL = Device.dwTimeContinual;
	}
}

void CGamepadService::Update() 
{
	if (triggerEffectTimeStampL != 0 && triggerEffectTimeL != 0)
	{
		if (Device.dwTimeContinual > (triggerEffectTimeStampL + triggerEffectTimeL))
		{
			ClearTriggerEffect(false);
			triggerEffectTimeL = 0;
		}
	}

	if (triggerEffectTimeStampR != 0 && triggerEffectTimeR != 0)
	{
		if (Device.dwTimeContinual > (triggerEffectTimeStampR + triggerEffectTimeR))
		{
			ClearTriggerEffect(true);
			triggerEffectTimeR = 0;
		}
	}
}

#include "stdafx.h"
#include "GamepadService.h"
#include "xr_input.h"

ENGINE_API CGamepadService* GGamepadService = nullptr;

bool CGamepadService::FeedbackMode = false;
bool CGamepadService::GyroscopeEnabled = true;
float CGamepadService::GyroscopeDeadZone = 0.15f;
float CGamepadService::GyroscopeSensitivity = 1.f;

CGamepadService::CGamepadService()
{
    InitHID();
    RumbleTimerID = 0;
}

CGamepadService::~CGamepadService()
{
    // Останавливаем таймер
    if (RumbleTimerID != 0)
    {
        SDL_RemoveTimer(RumbleTimerID);
        RumbleTimerID = 0;
    }
    
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
    SDL_hid_init();
}

void CGamepadService::FindHIDDevice()
{
    SDL_hid_device_info* Devices = SDL_hid_enumerate(0, 0);

    for (SDL_hid_device_info* CurrentDevice = Devices; CurrentDevice; CurrentDevice = CurrentDevice->next)
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

        HidDevice = SDL_hid_open_path(CurrentDevice->path);

        if (HidDevice)
        {
            Type = DetectedType;

            Msg
            (
                "~Opened %s (%04X:%04X)",
                Type == EGamepadType::DualShock4 ? "DualShock 4" : "DualSense",
                CurrentDevice->vendor_id,
                CurrentDevice->product_id
            );
			isWireless = CurrentDevice->interface_number == -1;
            SDL_hid_free_enumeration(Devices);
            return;
        }
    }

    SDL_hid_free_enumeration(Devices);
}

void CGamepadService::ResetHID()
{
    if (HidDevice != nullptr)
    {
        u8 Report[32] = {};
        Report[0] = 0x05;
        Report[1] = 0xFF;
        Report[4] = 0;
        Report[5] = 0;
        Report[6] = 0;
        Report[7] = 0;
        Report[8] = 255;
        SDL_hid_write(HidDevice, Report, sizeof(Report));

        SDL_hid_close(HidDevice);
        HidDevice = nullptr;
    }

    Type = EGamepadType::Unknown;
}

void CGamepadService::DestroyHID()
{
    ResetHID();
    SDL_hid_exit();
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
    // Сохраняем текущие значения
	CurrentLEDRed.store(Red);
	CurrentLEDGreen.store(Green);
	CurrentLEDBlue.store(Blue);

    if (Type == EGamepadType::DualShock4)
    {
        if (HidDevice == nullptr)
        {
            return;
        }

        u8 Report[32] = {};

        Report[0] = 0x05; // Output Report ID
        Report[1] = 0xFF; // Enable rumble + lightbar

        Report[4] = (u8)(CurrentLFRumble.load() / 257);
        Report[5] = (u8)(CurrentHFRumble.load() / 257);

        Report[6] = Red;
        Report[7] = Green;
        Report[8] = Blue;

        const int Result = SDL_hid_write(HidDevice, Report, sizeof(Report));

        if (Result < 0)
        {
            Msg("SDL_hid_write failed: %s", SDL_GetError());
        }
    }
    else if (GamePadDevice)
    {
        SDL_SetGamepadLED(GamePadDevice, Red, Green, Blue);
    }
}

void CGamepadService::StopRumble()
{
    if (Type == EGamepadType::DualShock4 && HidDevice != nullptr)
    {
        CurrentLFRumble.store(0);
        CurrentHFRumble.store(0);
        
        u8 Report[32] = {};
        Report[0] = 0x05;
        Report[1] = 0xFF;
        Report[4] = 0;
        Report[5] = 0;
		Report[6] = CurrentLEDRed.load();
		Report[7] = CurrentLEDGreen.load();
		Report[8] = CurrentLEDBlue.load();

        SDL_hid_write(HidDevice, Report, sizeof(Report));
    }
    else if (GamePadDevice != nullptr)
    {
        SDL_RumbleGamepad(GamePadDevice, 0, 0, 0);
    }
    
    RumbleTimerID = 0;
}

void CGamepadService::SetTriggerResistance(bool RightTrigger, u8 StartPosition, u8 Force)
{
    if (Type != EGamepadType::DualSense || HidDevice == nullptr)
    {
        return;
    }

    StartPosition = std::min(StartPosition, (u8)9);
    Force = std::min(Force, (u8)8);

    xr_vector<u8> array = {};
	array.resize(GetPacketSize());
	u8* Report = BeginOutputPacket(array);

    // Enable trigger effect
    Report[1] = 0x04;

    u8* Trigger = RightTrigger ? &Report[11] : &Report[22];

    Trigger[0] = 0x01; // Resistance mode
    Trigger[1] = StartPosition;
    Trigger[2] = Force;

    EndOutputPacket(array);
}

void CGamepadService::ClearTriggerEffect(bool RightTrigger)
{
    if (Type != EGamepadType::DualSense || HidDevice == nullptr)
    {
        return;
    }

    xr_vector<u8> array = {};
	array.resize(GetPacketSize());
	u8* Report = BeginOutputPacket(array);

    Report[1] = 0x04;

    u8* Trigger = RightTrigger ? &Report[11] : &Report[22];

    Trigger[0] = 0x00;

    EndOutputPacket(array);
}

bool CGamepadService::Rumble(u16 LFRumble, u16 HFRumble, u16 DurationMS)
{
    if (GamePadDevice == nullptr && HidDevice == nullptr)
    {
        return false;
    }

    // Останавливаем предыдущий таймер
    if (RumbleTimerID != 0)
    {
        SDL_RemoveTimer(RumbleTimerID);
        RumbleTimerID = 0;
    }

    if (Type == EGamepadType::DualShock4 && HidDevice != nullptr)
    {
        CurrentLFRumble.store(LFRumble);
        CurrentHFRumble.store(HFRumble);

        u8 Report[32] = {};

        Report[0] = 0x05; // Output Report ID
        Report[1] = 0xFF; // Enable rumble + lightbar

        Report[4] = (u8)(LFRumble / 257);
        Report[5] = (u8)(HFRumble / 257);

        Report[6] = CurrentLEDRed.load();
		Report[7] = CurrentLEDGreen.load();
		Report[8] = CurrentLEDBlue.load();

        int Result = SDL_hid_write(HidDevice, Report, sizeof(Report));

        if (Result < 0)
        {
            Msg("SDL_hid_write (rumble) failed: %s", SDL_GetError());
            return false;
        }

        // Запускаем таймер на отключение вибрации
        if (DurationMS > 0)
        {
            RumbleTimerID = SDL_AddTimer(DurationMS, RumbleTimerCallback, this);
            if (RumbleTimerID == 0)
            {
                Msg("SDL_AddTimer failed: %s", SDL_GetError());
                return false;
            }
        }

        return true;
    }
    else if (CGamepadService::FeedbackMode && GamePadDevice != nullptr)
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
        TriggerEffectTimeR = Time * 1000;
        TriggerEffectTimeStampR = Device.dwTimeContinual;
    }
    else
    {
        TriggerEffectTimeL = Time * 1000;
        TriggerEffectTimeStampL = Device.dwTimeContinual;
    }
}

void CGamepadService::ShotTriggerEffect(bool RightTrigger, float Time)
{
    if (Type != EGamepadType::DualSense || HidDevice == nullptr)
    {
        return;
    }

    xr_vector<u8> array;
	array.resize(GetPacketSize());
	u8* Report = BeginOutputPacket(array);

    // Enable trigger effect
    Report[1] = 0x04;

    u8* Trigger = RightTrigger ? &Report[11] : &Report[22];

    Trigger[0] = 0x25; // Shot mode
    Trigger[1] = 0x20;
    Trigger[2] = 0x1;
    Trigger[3] = 0x7;

    EndOutputPacket(array);

    if (RightTrigger)
    {
        TriggerEffectTimeR = Time * 1000;
        TriggerEffectTimeStampR = Device.dwTimeContinual;
    }
    else
    {
        TriggerEffectTimeL = Time * 1000;
        TriggerEffectTimeStampL = Device.dwTimeContinual;
    }
}

void CGamepadService::GyroscopeUpdate()
{
    if (!GyroscopeEnabled || GGamepadService->GamePadDevice == nullptr || !SDL_GamepadHasSensor(GGamepadService->GamePadDevice, SDL_SENSOR_GYRO) || !pInput->GetControllerMode())
    {
        return;
    }

    float GyroscopeVal[3];
    SDL_GetGamepadSensorData(GGamepadService->GamePadDevice, SDL_SENSOR_GYRO, GyroscopeVal, 3);

    Fvector3 Gyroscope{GyroscopeVal[0], GyroscopeVal[1], GyroscopeVal[2]};
    if (Gyroscope.magnitude() < GyroscopeDeadZone)
    {
        return;
    }

    Gyroscope *= GyroscopeSensitivity;

    pInput->GamepadGyroscopeUpdate(Gyroscope);
}

void CGamepadService::Update()
{
    if (TriggerEffectTimeStampL != 0 && TriggerEffectTimeL != 0)
    {
        if (Device.dwTimeContinual > (TriggerEffectTimeStampL + TriggerEffectTimeL))
        {
            ClearTriggerEffect(false);
            TriggerEffectTimeL = 0;
        }
    }

    if (TriggerEffectTimeStampR != 0 && TriggerEffectTimeR != 0)
    {
        if (Device.dwTimeContinual > (TriggerEffectTimeStampR + TriggerEffectTimeR))
        {
            ClearTriggerEffect(true);
            TriggerEffectTimeR = 0;
        }
    }
}

Uint32 CGamepadService::RumbleTimerCallback(void*, SDL_TimerID, Uint32)
{
	if (GGamepadService != nullptr)
	{
		GGamepadService->StopRumble();
	}
	return 0;
}

u8 CGamepadService::GetPacketSize()
{
	u8 size = 32;

	if (Type == EGamepadType::DualSense)
	{
		size = IsGamepadWireless() ? 78 : 48;
	}
	return size;
}

bool CGamepadService::IsGamepadWireless()
{
	return isWireless;
}

u8* CGamepadService::BeginOutputPacket(xr_vector<u8>& array)
{
	if (IsGamepadWireless())
    {
		array[0] = 0x31;
		array[1] = 0x02;
		return &array[1];
    }
    else
    {
		array[0] = 0x02;
		return &array[0];
    }
}

void CGamepadService::EndOutputPacket(xr_vector<u8>& array)
{
#pragma todo("St4lker0k765 to ForserX: CRC calculation doesn't work for some reason")
    if (isWireless)
    {
		u32 crcChecksum = crc32(array.data(), 74);
		array[74] = crcChecksum & 0x000000FF;
		array[75] = (crcChecksum & 0x0000FF00) >> 8;
		array[76] = (crcChecksum & 0x00FF0000) >> 16;
		array[77] = (crcChecksum & 0xFF000000) >> 24;
    }
    if (SDL_hid_write(HidDevice, array.data(), GetPacketSize()) == -1)
    {
		Msg(make_string<const char*>("! SDL Error while writing HID packet: %s", SDL_GetError()));
    }
}

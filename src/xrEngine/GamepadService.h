#pragma once
#include <SDL3/SDL_hidapi.h>

enum class EGamepadType
{
	Unknown,
	DualShock3,
	DualShock4,
	DualSense
};

class ENGINE_API CGamepadService
{
public:
	static bool GamepadFeedbackMode;

public:
	CGamepadService();
	~CGamepadService();

	shared_str GetGamepadPrefix() const;

	void UpdateLEDByHP(float Health);
	void SetTriggerResistance(bool RightTrigger, u8 StartPosition, u8 Force);
	void ClearTriggerEffect(bool RightTrigger);
	void SetLED(u8 Red, u8 Green, u8 Blue);

	bool Rumble(u16 LFRumble, u16 HFRumble, u16 DurationMS);

	void ResetHID();
	void FindHIDDevice();

	void Update();
	void SetTriggerResistance(bool RightTrigger, u8 StartPosition, u8 Force, float Time);
	void ShotTriggerEffect(bool RightTrigger, float Time);

public:
	EGamepadType Type = EGamepadType::Unknown;
	SDL_Gamepad* GamePadDevice = nullptr;
	u8 TouchpadFingersCount = 0;

private:
	SDL_hid_device* HidDevice = nullptr;

	u32 TriggerEffectTimeStampL = 0;
	u32 TriggerEffectTimeL = 0;
	u32 TriggerEffectTimeStampR = 0;
	u32 TriggerEffectTimeR = 0;

private:
	void InitHID();
	void DestroyHID();

};

extern ENGINE_API CGamepadService* GGamepadService;
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
	static bool FeedbackMode;
	static bool GyroscopeEnabled;
	static float GyroscopeDeadZone;
	static float GyroscopeSensitivity;

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
	void GyroscopeUpdate();

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

	xr_atomic_u8 CurrentLEDRed = 0;
	xr_atomic_u8 CurrentLEDGreen = 0;
	xr_atomic_u8 CurrentLEDBlue = 255;

	std::atomic<u16> CurrentLFRumble = 0;
	std::atomic<u16> CurrentHFRumble = 0;

	SDL_TimerID RumbleTimerID = 0;

private:
	void InitHID();
	void DestroyHID();
	void StopRumble();

	static Uint32 RumbleTimerCallback(void*, SDL_TimerID, Uint32);
};

extern ENGINE_API CGamepadService* GGamepadService;
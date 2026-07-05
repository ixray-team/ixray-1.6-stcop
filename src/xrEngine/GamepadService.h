#pragma once

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
	CGamepadService();
	~CGamepadService();

	void UpdateLEDByHP(float Health);

public:
	EGamepadType Type = EGamepadType::Unknown;

private:
	void* HidDevice = nullptr;

private:
	void InitHID();
	void SetLED(u8 Red, u8 Green, u8 Blue);
};

extern ENGINE_API CGamepadService* GGamepadService;
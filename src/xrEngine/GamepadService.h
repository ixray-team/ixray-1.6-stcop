#pragma once

class ENGINE_API CGamepadService
{
public:
	CGamepadService();
	~CGamepadService();

	void UpdateLEDByHP(float Health);

private:
	void* HidDevice = nullptr;

private:
	void InitHID();
	void SetLED(u8 Red, u8 Green, u8 Blue);
};

extern ENGINE_API CGamepadService* GGamepadService;
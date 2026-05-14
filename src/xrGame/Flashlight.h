#pragma once
#include "CustomDevice.h"

class CFlashlight final : public CCustomDevice
{
	using inherited = CCustomDevice;

	bool m_bFlashlightStatus = false;
	Fvector2 m_fElectronicProblems = { 0.0f, 0.5f };

public:
	CFlashlight() = default;
	~CFlashlight() override;

	void Load(const char* section) override;
	void UpdateCL() override;
	void OnMotionMark(u8 state, const motion_marks& m) override;
	void OnH_B_Independent(bool just_before_destroy) override;
	void OnHiddenItem() override;
	void OnMoveToRuck(const SInvItemPlace& prev) override;

	CCustomDevice* cast_custom_device() override { return this; }
};
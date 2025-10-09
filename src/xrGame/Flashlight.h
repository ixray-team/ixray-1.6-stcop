#pragma once
#include "CustomDevice.h"

class CFlashlight final : public CCustomDevice
{
	using inherited = CCustomDevice;

	bool m_bFlashlightStatus = false;

	struct electronic_problems
	{
		float problems_level = 0.0f;
		float probability = 1.0f;
	} m_ElectronicProblems;

public:
	CFlashlight() = default;
	~CFlashlight() override;

	void Load(LPCSTR section) override;
	void UpdateCL() override;
	void OnMotionMark(u32 state, const motion_marks& m) override;
	void OnH_B_Independent(bool just_before_destroy) override;

	CCustomDevice* cast_custom_device() override { return this; }
};
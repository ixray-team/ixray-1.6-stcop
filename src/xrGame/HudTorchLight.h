#pragma once

//#include "HudItem.h"

class CHudItem;

class HudLightTorch
{
public:
	const char* Section = {};
	ref_light RenderLight = nullptr;
	ref_light OmniLight = nullptr;
	bool IsRenderLight = false;
	const char* LightBone = {};
	const char* LightDirBoneName = {};
	bool IsLightDirByBone = false;
	Fvector3 LightOffset;
	Fvector3 AimOffset;
	Fvector3 LightWorldOffset;
	Fvector3 OmniOffset;
	Fvector3 OmniWorldOffset;
	Fcolor LightColor;
	Fcolor OmniColor;
	bool IsTorchInstalled = false;
	RStringVec ConeBones{};

public:
	HudLightTorch() = default;
	virtual ~HudLightTorch();

public:
	virtual void NewTorchlight(const char* section);
	virtual void UpdateTorchFromObject(CHudItem* item) const;

	void SwitchTorchlight(bool isActive);
	IC void SetInstalled(bool install) { IsTorchInstalled = install; }

	IC bool GetTorchActive() const { return IsRenderLight; }
	IC bool GetTorchInstalled() const { return IsTorchInstalled; }
};

class HudLightLaser : public HudLightTorch
{
	float LaserLightDist = 15.0f;
	float LaserWorkDist = 7.5f;
	float LaserMaxDist = 15.0f;

public:
	Fvector2 LightSpotAngle = {2, 5};

	HudLightLaser();
	virtual ~HudLightLaser();
	virtual void NewTorchlight(const char* section);
	virtual void UpdateTorchFromObject(CHudItem* item) const;
};
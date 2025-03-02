#pragma once

class CHudItem;
class player_hud;
class CMotionDef;

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

public:
	HudLightTorch() = default;
	~HudLightTorch();

public:
	void NewTorchlight(const char* section);
	void SwitchTorchlight(bool isActive);
	void UpdateTorchFromObject(CHudItem* item) const;
	bool GetTorchActive() const { return IsRenderLight; }
	bool GetTorchInstalled() const { return IsTorchInstalled; }
};
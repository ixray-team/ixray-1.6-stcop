#pragma once

//#include "HudItem.h"

class CHudItem;
class CHudItemObject;

struct THudLightTorch
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

	struct breaking_params
	{
		float start_condition = 0.0f;     // при каком состоянии начнутся проблемы
		float end_condition = 0.0f;       // при каком состоянии отрубится вообще
		float start_probability = 0.0f;   // вероятность проблем в стартовом состоянии
		float levels_problem = 0.0f;
	} BreakingParams;

	virtual ~THudLightTorch() = default;

	virtual void BeginComponent(IECSOwner* O);
	void EndComponent();

	virtual void NewTorchlight(const char* section);
	virtual void UpdateTorchFromObject(CHudItem* item) const;

	void SwitchTorchlight(bool isActive);
	IC void SetInstalled(bool install) { IsTorchInstalled = install; }

	IC bool GetTorchActive() const { return IsRenderLight; }
	IC bool GetTorchInstalled() const { return IsTorchInstalled; }

	void UpdateTorch(CHudItemObject* item, bool& saved_status);
	void SwitchTorch(bool& saved_status, bool status, bool forced = false);

private:
	ECS_COMPONENT(THudLightTorch)
		ECS_STRING(Section, "Light Section");
		ECS_PTR(RenderLight, "Light Ptr");
		ECS_PTR(OmniLight, "Omni Ptr");
		ECS_VALUE(IsRenderLight, "Is Render Light");
		ECS_STRING(LightBone, "Light Bone");
		ECS_STRING(LightDirBoneName, "Light Dir Bone Name");
		ECS_VALUE(IsLightDirByBone, "Is Light Dir By Bone");
		ECS_VALUE(LightOffset.x, "Light Offset X");
		ECS_VALUE(LightOffset.y, "Light Offset Y");
		ECS_VALUE(LightOffset.z, "Light Offset Z");
		ECS_VALUE(AimOffset.x, "Aim Offset X");
		ECS_VALUE(AimOffset.y, "Aim Offset Y");
		ECS_VALUE(AimOffset.z, "Aim Offset Z");
		ECS_VALUE(LightWorldOffset.x, "Light World Offset X");
		ECS_VALUE(LightWorldOffset.y, "Light World Offset Y");
		ECS_VALUE(LightWorldOffset.z, "Light World Offset Z");
		ECS_VALUE(OmniOffset.x, "Omni Offset X");
		ECS_VALUE(OmniOffset.y, "Omni Offset Y");
		ECS_VALUE(OmniOffset.z, "Omni Offset Z");
		ECS_VALUE(OmniWorldOffset.x, "Omni World Offset X");
		ECS_VALUE(OmniWorldOffset.y, "Omni World Offset Y");
		ECS_VALUE(OmniWorldOffset.z, "Omni World Offset Z");
		ECS_VALUE(LightColor.r, "Light Color R");
		ECS_VALUE(LightColor.g, "Light Color G");
		ECS_VALUE(LightColor.b, "Light Color B");
		ECS_VALUE(LightColor.a, "Light Color A");
		ECS_VALUE(OmniColor.r, "Omni Color R");
		ECS_VALUE(OmniColor.g, "Omni Color G");
		ECS_VALUE(OmniColor.b, "Omni Color B");
		ECS_VALUE(OmniColor.a, "Omni Color A");
		ECS_VALUE(IsTorchInstalled, "Is Torch Installed");
		ECS_VALUE(BreakingParams.start_condition, "Breaking Params Start Condition");
		ECS_VALUE(BreakingParams.end_condition, "Breaking Params End Condition");
		ECS_VALUE(BreakingParams.start_probability, "Breaking Params Start Probability");
		ECS_VALUE(BreakingParams.levels_problem, "Breaking Params Levels Problem");
	ECS_END
};

struct THudLightLaser final : public THudLightTorch
{
	float LaserLightDist = 15.0f;
	float LaserWorkDist = 7.5f;
	float LaserMaxDist = 15.0f;
	Fvector2 LightSpotAngle = { 2.0f, 5.0f };
public:

	virtual void BeginComponent(IECSOwner* O) override;

	virtual void NewTorchlight(const char* section) override;
	virtual void UpdateTorchFromObject(CHudItem* item) const override;

	void UpdateLaser(CHudItemObject* item, bool& saved_status);
	void SwitchLaser(bool& saved_status, bool status, bool forced = false);

private:
	ECS_COMPONENT(THudLightLaser)
		ECS_STRING(Section, "Light Section");
		ECS_PTR(RenderLight, "Light Ptr");
		ECS_PTR(OmniLight, "Omni Ptr");
		ECS_VALUE(LaserLightDist, "Laser Light Distance")
		ECS_VALUE(LaserWorkDist, "Laser Work Distance")
		ECS_VALUE(LaserMaxDist, "Laser Max Distance")
		ECS_VALUE(IsRenderLight, "Is Render Light");
		ECS_VALUE(LightSpotAngle.x, "Light Spot Angle X");
		ECS_VALUE(LightSpotAngle.y, "Light Spot Angle Y");
		ECS_STRING(LightBone, "Light Bone");
		ECS_STRING(LightDirBoneName, "Light Dir Bone Name");
		ECS_VALUE(IsLightDirByBone, "Is Light Dir By Bone");
		ECS_VALUE(LightOffset.x, "Light Offset X");
		ECS_VALUE(LightOffset.y, "Light Offset Y");
		ECS_VALUE(LightOffset.z, "Light Offset Z");
		ECS_VALUE(AimOffset.x, "Aim Offset X");
		ECS_VALUE(AimOffset.y, "Aim Offset Y");
		ECS_VALUE(AimOffset.z, "Aim Offset Z");
		ECS_VALUE(LightWorldOffset.x, "Light World Offset X");
		ECS_VALUE(LightWorldOffset.y, "Light World Offset Y");
		ECS_VALUE(LightWorldOffset.z, "Light World Offset Z");
		ECS_VALUE(OmniOffset.x, "Omni Offset X");
		ECS_VALUE(OmniOffset.y, "Omni Offset Y");
		ECS_VALUE(OmniOffset.z, "Omni Offset Z");
		ECS_VALUE(OmniWorldOffset.x, "Omni World Offset X");
		ECS_VALUE(OmniWorldOffset.y, "Omni World Offset Y");
		ECS_VALUE(OmniWorldOffset.z, "Omni World Offset Z");
		ECS_VALUE(LightColor.r, "Light Color R");
		ECS_VALUE(LightColor.g, "Light Color G");
		ECS_VALUE(LightColor.b, "Light Color B");
		ECS_VALUE(LightColor.a, "Light Color A");
		ECS_VALUE(OmniColor.r, "Omni Color R");
		ECS_VALUE(OmniColor.g, "Omni Color G");
		ECS_VALUE(OmniColor.b, "Omni Color B");
		ECS_VALUE(OmniColor.a, "Omni Color A");
		ECS_VALUE(IsTorchInstalled, "Is Laser Installed");
		ECS_VALUE(BreakingParams.start_condition, "Breaking Params Start Condition");
		ECS_VALUE(BreakingParams.end_condition, "Breaking Params End Condition");
		ECS_VALUE(BreakingParams.start_probability, "Breaking Params Start Probability");
		ECS_VALUE(BreakingParams.levels_problem, "Breaking Params Levels Problem");
	ECS_END
};
#pragma once

enum class EEngineExternalUI {
	HQIcons,
	DisableCharacterInfo,
	DisableInventoryGrid,
	None
};

enum class EEngineExternalPhysical {
	DeadBodyRagdoll,
	None
};

enum class EEngineExternalGame
{
	EnableThirst,
	EnableSleepiness,
	EnableAiDieInAnomaly,
	EnableNPCLookAtActor,
	EnableBlockSprintInReload,
	EnableAutoreload,
	EnableMonstersInventory,
	EnableWeaponInertion,
	EnableWeaponCollision,
	EnableArtefactDegradation,
	EnableActorStepWallmarks,
	DisableSprintWhileOverweight
};

enum class EEngineExternalRender 
{
	DisableLoadScreenTips,
	None
};

enum class EEngineExternalEnvironment
{
	ReadSunConfig,
	None
};


class ENGINE_API CEngineExternal final
{
	CInifile* pOptions = nullptr;

public:
	CEngineExternal();
	~CEngineExternal();

	xr_string GetTitle() const;
	const char* GetPlayerHudOmfAdditional() const;
	const xr_vector<shared_str> StepWallmarksMaterials() const;
	const xr_string WallmarkLeft() const;
	const xr_string WallmarkRight() const;
	float GetWeaponIconScaling() const;
	const char* PlatformMode() const;
	bool ClearSkyMode() const;
	bool CallOfPripyatMode() const;

	u32 GetFontAltasSize() const;

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
	bool operator[](const EEngineExternalEnvironment& ID) const;
};

ENGINE_API CEngineExternal& EngineExternal();

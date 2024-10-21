#pragma once

enum class EEngineExternalUI {
	HQIcons,
	DisableCharacterInfo,
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
	Enable3DPDA,
	EnableArtefactDegradation
};

enum class EEngineExternalRender {
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

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
	bool operator[](const EEngineExternalEnvironment& ID) const;
};

ENGINE_API CEngineExternal& EngineExternal();

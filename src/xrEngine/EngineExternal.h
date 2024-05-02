#pragma once

enum class EEngineExternalUI {
	HQIcons,
	DiasbleCharacterInfo,
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
	EnableMonstersInventory
};

enum class EEngineExternalRender {
	LoadScreenTips,
	None
};

enum class EEngineExternalEnvironment
{
	ReadSunConfig,
	None
};

enum class EEngineExternalGunslinger
{
	EnableGunslingerMode,
	None
};

class ENGINE_API CEngineExternal final
{
	CInifile* pOptions = nullptr;

public:
	CEngineExternal();
	~CEngineExternal();

	xr_string GetTitle() const;

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
	bool operator[](const EEngineExternalEnvironment& ID) const;
	bool operator[](const EEngineExternalGunslinger& ID) const;
};

ENGINE_API CEngineExternal& EngineExternal();

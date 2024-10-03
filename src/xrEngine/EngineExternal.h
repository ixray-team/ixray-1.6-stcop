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
	EnableWeaponCollision
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

enum class EEngineExternalGunslinger
{
	EnableGunslingerMode,
	None
};

class ENGINE_API CEngineExternal final
{
	bool m_is_modification_gunslinger;
	CInifile* pOptions;
	const char* m_pTitle;
public:
	CEngineExternal();
	~CEngineExternal();

	const char* GetTitle() const;

	bool isModificationGunslinger(void) const;
	const char* GetPlayerHudOmfAdditional() const;

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
	bool operator[](const EEngineExternalEnvironment& ID) const;
	bool operator[](const EEngineExternalGunslinger& ID) const;
};

ENGINE_API CEngineExternal& EngineExternal();

#pragma once

#include <atomic>

enum class EEngineExternalUI
{
	//HQIcons,
	DisableCharacterInfo,
	DisableInventoryGrid,
	ShowLoadingStages,
	DisableMotionIcon,
	PdaRearrangeTabButtons,
	UseSavedGameStatic,
	DisableHudRenderingOnMaster,
	UseCompassBar, // Deprecated boot-time hint; prefer CUIMainIngameWnd::SetNavigationMode / Lua API
	None
};

enum class EEngineExternalPhysical
{
	DeadBodyRagdoll,
	DisableCameraMagicField,
	None
};

enum class EEngineExternalSound
{
	EnableWorldAmbientOcclusion,
	MaterialOCC,
	None
};

enum class EEngineExternalSpawnSupplies
{
	EnableLoadoutsSupplies,
	EnableSpawnFullRandomLoadout,
	EnableSpawnOnceRandomItemPerEachLoadouts,
	EnableSpawnOnceRandomitemByRandomLoadout,
	None
};

enum class EEngineExternalGame
{
	EnableThirst,
	EnableSleepiness,
	EnableMedIntoxication,
	EnableAiDieInAnomaly,
	EnableNPCLookAtActor,
	EnableBlockSprintInReload,
	EnableAutoreload,
	EnableMonstersInventory,
	EnableWeaponInertion,
	EnableActorStepWallmarks,
	DisableSprintWhileOverweight,
	EnableAlternateZoomFovCalc,
	EnableImproveWeaponMisfire,
	EnableDelayedWeaponActions,
	EnableLegacyUpgradeSystem,
	EnableEngineArtefactSpawn,
	Enable3DPDA,
	EnableTorchOnlyInOutfit,
	EnableBurerShieldPenetrationWithGauss,
	EnableWeaponAlternateRPMSystem,
	EnableInventoryVolume,
	EnableRealBulletPos,
	EnableWeaponAffectsOnMouseSensitivity,
	EnableMainMenuForDisabledControls,
	NewDemoRecordInputSchema,
	EnablePolterStaminaLooseOnHit,
	EnableWindEffectOnABullet,
	EnableSaveOnNewGame,
	EnableSuicideByController,
	Enable3DIcons,
	EnableRagdolledZombiePseudodeath,
	EnableStackWindow,
	EnableFirstPersonDie,
	ArtefactDegradation,
	None
};

enum class EEngineExternalMonstersLogic
{
	EnableMonsterFactionEnemySharingIsolation,
	EnableMonsterFriendlyEnemySharing,
	EnableMonsterDistributedTargeting,
	EnableMonsterEnemySharingCallSounds,
	EnableMonsterVisionIgnoreFoliage,
	None
};

enum class EEngineExternalRender 
{
	DisableLoadScreenTips,
	UseDynamicSnowMask,
	UseLegacyParticleLoader,
	None
};

enum class EEngineExternalEnvironment
{
	ReadSunConfig,
	None
};

enum class EEngineExternalPlatform : u8
{
	ShadowOfChernobyl,
	ClearSky,
	CallOfPripyat,
	EnumSize,
	Unknown = static_cast<u8>(-1)
};

enum class EEngineExternalSystem : u8
{
	AdvancedSerialization,
	AdvancedSerializationDebugLog,
	EngineScriptStoryID,
	EngineAmmoRepacker,
	CustomMessageInClipboardOnCrash,
	DisablePause,
	DisableConsole,
	None
};

constexpr const char* kPlatformNameCOP = "cop";
constexpr const char* kPlatformNameCS = "cs";
constexpr const char* kPlatformNameSOC = "soc";

constexpr const char* g_PlatformNames[] = {kPlatformNameSOC, kPlatformNameCS, kPlatformNameCOP};
constexpr EEngineExternalPlatform g_Platforms[] = {EEngineExternalPlatform::ShadowOfChernobyl, EEngineExternalPlatform::ClearSky, EEngineExternalPlatform::CallOfPripyat};

constexpr const char* kUIConfigField_InventoryVectorIcon = "inv_vector_icon";

static_assert(std::size(g_PlatformNames) == static_cast<u8>(EEngineExternalPlatform::EnumSize), "you must register names that will be equal to EEngineExternalPlatform, you forgot to add a new platform to g_PlatformNames");
static_assert(std::size(g_Platforms) == static_cast<u8>(EEngineExternalPlatform::EnumSize), "must be equal, probably you forgot to register a new platform in enum or you forgot to add platform to g_Platforms");
static_assert(std::size(g_Platforms) == std::size(g_PlatformNames), "must be equal!");

using ShaderExternalMap = xr_string_map<xr_string, xr_string>;

class XRCORE_API CEngineExternal final
{
private:
	std::atomic<int> MonsterLogicOverride{-1};
	float MonsterEnemySharingCloseRatio = .25f;
	u32 MonsterEnemySharingIntervalMin = 3000;
	u32 MonsterEnemySharingIntervalMax = 5000;
public:
	u32 GetMonsterEnemySharingIntervalMin() const { return MonsterEnemySharingIntervalMin; }
	u32 GetMonsterEnemySharingIntervalMax() const { return MonsterEnemySharingIntervalMax; }
	float GetMonsterEnemySharingCloseRatio() const { return MonsterEnemySharingCloseRatio; }
	void SetMonsterLogicOverride(int Value) { MonsterLogicOverride.store(Value, std::memory_order_relaxed); }
	int GetMonsterLogicOverride() const { return MonsterLogicOverride.load(std::memory_order_relaxed); }
	CEngineExternal();
	~CEngineExternal();

	xr_string GetTitle() const;
	const char* GetPlayerHudOmfAdditional() const;
	const char* GetPreferredFallbackLanguage() const;
	const xr_vector<shared_str> StepWallmarksMaterials() const;
	const xr_string WallmarkLeft() const;
	const xr_string WallmarkRight() const;
	float GetWeaponIconScaling() const;
	const char* PlatformMode() const;
	bool ClearSkyMode() const;
	bool CallOfPripyatMode() const;
	bool ShadowOfChernobylMode() const;
	Fvector4 GetTalkDof() const;
	float GetTalkFovScale() const;
	u32 GetFontAltasSize() const;
	float GetSprintFovFactor() const;
	const char* GetCurrentPlatformFullName();
	const char* GetCurrentPlatformName();
	EEngineExternalPlatform GetCurrentPlatform();
	shared_str GetInventoryItemCountPrefix();

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalSpawnSupplies& ID) const;
	bool operator[](const EEngineExternalMonstersLogic& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
	bool operator[](const EEngineExternalEnvironment& ID) const;
	bool operator[](const EEngineExternalSound& ID) const;
	bool operator[](const EEngineExternalSystem& ID) const;

	ICF bool operator[](const EEngineExternalPlatform& ID) const
	{
		if (static_cast<unsigned char>(ID) > static_cast<unsigned char>(std::size(g_Platforms)) - 1)
			return false;

		return g_Platforms[static_cast<unsigned char>(ID)] == m_platform_type;
	}

	ShaderExternalMap ShadersOptions;
	Ivector2 gamesaveSize;

	CInifile* GetIniFile();
	CInifile* GetMonsterLogicIniFile() const { return MonsterLogicOptions; }
private:
	void InitPlatform(const char* pPlatformName);

private:
	EEngineExternalPlatform m_platform_type;
	CInifile* pOptions;
	CInifile* MonsterLogicOptions = nullptr;
	bool MonsterLogicEnabled = false;
	bool MonsterLogicConfigAvailable = false;
};

const char* Translate_EEngineExternalPlatform(EEngineExternalPlatform);

XRCORE_API CEngineExternal& EngineExternal();

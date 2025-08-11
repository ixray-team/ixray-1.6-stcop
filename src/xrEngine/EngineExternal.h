#pragma once

enum class EEngineExternalUI {
	HQIcons,
	DisableCharacterInfo,
	DisableInventoryGrid,
	ShowLoadingStages,
	DisableMotionIcon,
	PdaRearrangeTabButtons,
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
	EnableActorStepWallmarks,
	DisableSprintWhileOverweight,
	EnableAlternateZoomFovCalc,
	EnableInventoryPistolSlot,
	EnableImproveWeaponMisfire,
};

enum class EEngineExternalRender 
{
	DisableLoadScreenTips,
	UseDynamicSnowMask,
	None
};

enum class EEngineExternalEnvironment
{
	ReadSunConfig,
	None
};

enum class EEngineExternalPlatform : unsigned char
{
	ShadowOfChernobyl,
	ClearSky,
	CallOfPripyat,
	EnumSize,
	Unknown = static_cast<unsigned char>(-1)
};

constexpr const char* kPlatformNameCOP = "cop";
constexpr const char* kPlatformNameCS = "cs";
constexpr const char* kPlatformNameSOC = "soc";

constexpr const char* g_PlatformNames[] = {kPlatformNameSOC, kPlatformNameCS, kPlatformNameCOP};
constexpr EEngineExternalPlatform g_Platforms[] = {EEngineExternalPlatform::ShadowOfChernobyl, EEngineExternalPlatform::ClearSky, EEngineExternalPlatform::CallOfPripyat};


static_assert((sizeof(g_PlatformNames) / sizeof(g_PlatformNames[0])) == static_cast<unsigned char>(EEngineExternalPlatform::EnumSize), "you must register names that will be equal to EEngineExternalPlatform, you forgot to add a new platform to g_PlatformNames");
static_assert((sizeof(g_Platforms) / sizeof(g_Platforms[0])) == static_cast<unsigned char>(EEngineExternalPlatform::EnumSize), "must be equal, probably you forgot to register a new platform in enum or you forgot to add platform to g_Platforms");
static_assert((sizeof(g_Platforms) / sizeof(g_Platforms[0])) == (sizeof(g_PlatformNames) / sizeof(g_PlatformNames[0])), "must be equal!");


class ENGINE_API CEngineExternal final
{
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
	bool ShadowOfChernobylMode() const;
	Fvector4 GetTalkDof() const;
	float GetTalkFovScale() const;
	u32 GetFontAltasSize() const;
	float GetSprintFovFactor() const;

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
	bool operator[](const EEngineExternalEnvironment& ID) const;
	bool operator[](const EEngineExternalPlatform& ID) const;

	xr_string_map<xr_string, xr_string> ShadersOptions;
	Ivector2 gamesaveSize;

private:
	void InitPlatform(const char* pPlatformName);

private:
	EEngineExternalPlatform m_platform_type;
	CInifile* pOptions;
};

const char* Translate_EEngineExternalPlatform(EEngineExternalPlatform);

ENGINE_API CEngineExternal& EngineExternal();

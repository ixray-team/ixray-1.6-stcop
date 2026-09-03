#pragma once

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
	EnableWeaponCollision,
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
	EnablePolterDrop,
	EnableWindEffectOnABullet,
	EnableSaveOnNewGame,
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

enum class EEngineExternalSound
{
	EnableWorldAmbientOcclusion,
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
	EngineScriptStoryID,
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
	template<XRay::Concepts::Enum Enum>
	bool CachedGetSettings(CInifile* File, xr_string_view Type, Enum ID) const
	{
		using ntype = std::underlying_type_t<Enum>;
		static xr_array<bool, (size_t)Enum::None> Cache = [File, &Type]()
		{
			xr_array<bool, (size_t)Enum::None> Cache = {};
			for (ntype i = 0; i < (ntype)Enum::None; i++)
			{
				Cache[i] = READ_IF_EXISTS(File, r_bool, Type.data(), magic_enum::enum_name((Enum)i).data(), false);
			}
			return Cache;
		}();
		return Cache[(ntype)ID];
	}
	
public:
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

	ICF bool operator[](const EEngineExternalUI& ID) const
	{
		static xr_stack_string16 Type = "ui";
		return CachedGetSettings<EEngineExternalUI>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalPhysical& ID) const
	{
		static xr_stack_string16 Type = "physics";
		return CachedGetSettings<EEngineExternalPhysical>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalGame& ID) const
	{
		static xr_stack_string16 Type = "gameplay";
		return CachedGetSettings<EEngineExternalGame>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalSpawnSupplies& ID) const
	{
		static xr_stack_string16 Type = "spawn_supplies";
		return CachedGetSettings<EEngineExternalSpawnSupplies>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalRender& ID) const
	{
		static xr_stack_string16 Type = "render";
		return CachedGetSettings<EEngineExternalRender>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalEnvironment& ID) const {
		static xr_stack_string16 Type = "environment";
		return CachedGetSettings<EEngineExternalEnvironment>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalSound& ID) const
	{
		static xr_stack_string16 Type = "sound";
		return CachedGetSettings<EEngineExternalSound>(pOptions, Type, ID);
	}

	ICF bool operator[](const EEngineExternalPlatform& ID) const
	{
		if (static_cast<unsigned char>(ID) > static_cast<unsigned char>(std::size(g_Platforms)) - 1)
			return false;

		return g_Platforms[static_cast<unsigned char>(ID)] == m_platform_type;
	}
	
	ICF bool operator[](const EEngineExternalSystem& ID) const
	{
		static xr_stack_string16 Type = "system";
		return CachedGetSettings<EEngineExternalSystem>(pOptions, Type, ID);
	}

	ShaderExternalMap ShadersOptions;
	Ivector2 gamesaveSize;

	CInifile* GetIniFile();
private:
	void InitPlatform(const char* pPlatformName);

private:
	EEngineExternalPlatform m_platform_type;
	CInifile* pOptions;
};

const char* Translate_EEngineExternalPlatform(EEngineExternalPlatform);

XRCORE_API CEngineExternal& EngineExternal();

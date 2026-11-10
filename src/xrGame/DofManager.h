#pragma once

#include "Descs/DescRegistry.h"
#include "alife_space.h"
#include "../xrEngine/IGame_Persistent.h"

struct SDofOptions
{
	bool Dynamic = false;
	bool ConstantZoom = false;
	bool Reload = true;
	bool HudAnimator = false;
	bool Talk = true;
	bool Inventory = true;
};

extern SDofOptions g_DofOptions;

// Owns the applied DOF-only configuration view; it never replaces system_ini().
class DofSettings final
{
public:
	DofSettings() = delete;
	~DofSettings() = default;
	static const CInifile& Get();
	static void Apply(xr_unique_ptr<CInifile> Config);
	static void Clear();
	static u64 GetRevision();
	static bool Validate(const CInifile& Config, xr_string& Error);
	static bool IsDofKey(xr_string_view Key);
	static shared_str AnimationAlias(xr_string_view Key);

private:
	static xr_unique_ptr<CInifile> Applied;
	static u64 Revision;
};

enum class EDofEvent : u32
{
	Talk,
	Zone,
	Pickable,
	Inventory,
	Pda,
	HudAnimator,
	Reload,
	WeaponAction,
	Aim,
	AimAlt,
	AimGrenade,
	AimLens,
	Effector,
	Count
};

struct SDofCameraDesc
{
	using Registry = TDescRegistry<SDofCameraDesc>;
	SDofCameraSettings Camera;
	shared_str Section;
	Flags8 Fields = {};
	bool Enabled = true;
	void Load(const shared_str& Section);
	void LoadFrom(const CInifile& Config, const shared_str& Section);
	void Apply(SDofCameraSettings& Settings) const;
};

enum EDofFields : u8
{
	DofEnabled = 1 << 0,
	DofExplicitEnable = 1 << 1,
	DofNear = 1 << 2,
	DofFocus = 1 << 3,
	DofFar = 1 << 4,
	DofSpeedIn = 1 << 5,
	DofSpeedOut = 1 << 6,
	DofTimeOffset = 1 << 7
};

struct SHudDofRequest
{
	Fvector Values = {0.0f, 0.5f, 5.0f};
	float SpeedIn = 3.0f;
	float SpeedOut = 1.0f;
};

struct SHudDofActionDesc
{
	SHudDofRequest Request;
	Fvector Overrides = {};
	float TimeOffset = -0.4f;
	Flags8 Fields = {};
};

struct SHudDofDefaults
{
	using Registry = TDescRegistry<SHudDofDefaults>;
	Fvector Zoom = {0.5f, 0.8f, 100.0f};
	SHudDofActionDesc Action;
	SHudDofRequest Inventory;
	float Speed = 5.0f;
	float PickNear = -70.0f;
	float PickFar = 70.0f;
	const SDofCameraDesc* Cameras[static_cast<u32>(EDofEvent::Count)] = {};
	void Load(const shared_str& Section);
	void LoadFrom(const CInifile& Config, const shared_str& Section, bool LoadCameras = false);
};

struct SHudDofDesc
{
	using Registry = TDescRegistry<SHudDofDesc>;
	xr_hash_map<shared_str, SHudDofActionDesc> Actions;
	xr_hash_map<shared_str, const SDofCameraDesc*> CameraActions;
	const SDofCameraDesc* Cameras[static_cast<u32>(EDofEvent::Count)] = {};
	SHudDofActionDesc Usable;
	SHudDofActionDesc Reload;
	SHudDofRequest Zoom;
	SHudDofRequest AlterZoom;
	SHudDofRequest Grenade;
	SHudDofRequest Lens;
	SHudDofRequest Pda;
	Fvector LensOverrides = {};
	Flags8 LensFields = {};
	bool DisableZoom = false;
	void Load(const shared_str& Section);
	void LoadFrom(const CInifile& Config, const shared_str& Section, const SHudDofDefaults& Defaults, bool LoadCameras = false);
};

// Passive animation state; all policy and transitions belong to CDofManager.
struct SHudDofSource
{
	shared_str HudSection;
	shared_str ItemSection;
	shared_str ScopeSection;
	const SHudDofDesc* Hud = nullptr;
	const SHudDofDesc* Item = nullptr;
	const SHudDofDesc* Scope = nullptr;
	SHudDofActionDesc Action;
	shared_str Motion;
	u64 Revision = 0;
	bool HasAction = false;
	const SDofCameraDesc* CameraAction = nullptr;
	shared_str CameraActionSection;
	u32 StartTime = 0;
	u32 Duration = 0;
	float ReleaseAfter = 0.0f;
	float CameraReleaseAfter = 0.0f;
	bool ReloadAction = false;
	bool UsableAction = false;
	bool Usable = false;
	bool Playing = false;
};

class CDofManager
{
public:
	void Initialize();
	void SetBase(const Fvector& Values);
	void SetEffector(const Fvector& Values);
	void RestoreEffector();
	void SetPickable(bool Active);
	void SetZoneActive(ALife::_OBJECT_ID Id, bool Active);
	void Update(float Delta);
	void Reset();
	void GetCurrent(Fvector& Result) const;
	void SetCinema(const Fvector4* Values);
	float GetKernel(float Default) const { return HasCinema ? Cinema.w : KernelFrom >= 0.0f ? KernelFrom + (Default - KernelFrom) * Progress : Default; }
	void SetForced(bool Value) { IsForced = Value; }
	bool IsDofForced() const { return IsForced; }
	const Fvector& GetBase() const { return Base; }
	void GetCameraSettings(SDofCameraSettings& Settings) const;
	float GetOverrideWeight() const { return OverrideWeight; }
	void SetPreview(const SDofCameraDesc& Profile);
	void SetPreview(const SHudDofRequest& Profile);
	void StopPreview();
	bool IsPreviewing() const { return IsCameraPreview || IsStaticPreview; }
	EDofEvent GetActiveEvent() const { return ActiveEvent; }
	const shared_str& GetActiveProfile() const { return ActiveProfile; }
	const shared_str& GetActiveBinding() const { return ActiveBinding; }
	static const char* EventName(EDofEvent Event);

	static void LoadHud(SHudDofSource& Source, const shared_str& Section, bool Usable = false);
	static void LoadItem(SHudDofSource& Source, const shared_str& Section);
	static void LoadScope(SHudDofSource& Source, const shared_str& Section);
	static void StartAction(SHudDofSource& Source, const shared_str& Motion, u32 Duration);
	static void StopAction(SHudDofSource& Source);

private:
	static void CacheAction(SHudDofSource& Source);
	static void RefreshSource(SHudDofSource& Source);
	bool SelectRequest(SHudDofRequest& Request) const;
	const SDofCameraDesc* SelectCameraProfile() const;
	const SDofCameraDesc* GetCameraProfile(EDofEvent Event, const SHudDofSource* Source = nullptr, bool UseAction = false) const;
	bool GetAction(const SHudDofSource& Source, bool Animator, SHudDofRequest& Request) const;
	bool GetTimedAction(const SHudDofSource& Source, const SHudDofActionDesc& Action, SHudDofRequest& Request) const;
	void Blend(const Fvector& Values, float Weight, float Rate, float Delta);
	Fvector PickFocus() const;

	const SHudDofDefaults* Defaults = nullptr;
	const SDofCameraDesc* CameraProfile = nullptr;
	SDofCameraDesc PreviewCamera;
	SHudDofRequest PreviewStatic;
	bool IsCameraPreview = false;
	bool IsStaticPreview = false;
	u32 PreviewOriginalMode = 0;
	mutable EDofEvent ActiveEvent = EDofEvent::Count;
	mutable shared_str ActiveProfile;
	mutable shared_str ActiveBinding;
	Fvector4 Talk = {-1.0f, -1.0f, -1.0f, -1.0f};
	xr_hash_set<ALife::_OBJECT_ID> Zones;
	Fvector Base = {-1.25f, 1.4f, 600.0f};
	Fvector Effector = Base;
	Fvector Current = Base;
	Fvector From = Base;
	Fvector Target = Base;
	float Progress = 0.0f;
	float OverrideWeight = 0.0f;
	float FromWeight = 0.0f;
	float TargetWeight = 0.0f;
	float Speed = 5.0f;
	float ReturnSpeed = 1.0f;
	bool HasEffector = false;
	bool Pickable = false;
	bool HadRequest = false;
	Fvector4 Cinema = {};
	float KernelFrom = -1.0f;
	bool IsForced = false;
	bool HasCinema = false;
};

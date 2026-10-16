#pragma once

#include "Descs/DescRegistry.h"

struct SDofOptions
{
	bool Dynamic = false;
	bool ConstantZoom = false;
	bool Reload = true;
	bool HudAnimator = false;
	bool Talk = true;
};

extern SDofOptions g_DofOptions;

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
	float Speed = 5.0f;
	float PickNear = -70.0f;
	float PickFar = 70.0f;
	void Load(const shared_str& Section);
};

struct SHudDofDesc
{
	using Registry = TDescRegistry<SHudDofDesc>;
	xr_hash_map<shared_str, SHudDofActionDesc> Actions;
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
	const SHudDofActionDesc* Action = nullptr;
	u32 StartTime = 0;
	u32 Duration = 0;
	float ReleaseAfter = 0.0f;
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
	void SetZoneActive(u16 Id, bool Active);
	void Update(float Delta);
	void Reset();
	void GetCurrent(Fvector& Result) const;

	static void LoadHud(SHudDofSource& Source, const shared_str& Section, bool Usable = false);
	static void LoadItem(SHudDofSource& Source, const shared_str& Section);
	static void LoadScope(SHudDofSource& Source, const shared_str& Section);
	static void StartAction(SHudDofSource& Source, const shared_str& Motion, u32 Duration);
	static void StopAction(SHudDofSource& Source);

private:
	bool SelectRequest(SHudDofRequest& Request) const;
	bool GetAction(const SHudDofSource& Source, bool Animator, SHudDofRequest& Request) const;
	bool GetTimedAction(const SHudDofSource& Source, const SHudDofActionDesc& Action, SHudDofRequest& Request) const;
	void Blend(const Fvector& Values, float Rate, float Delta);
	Fvector PickFocus() const;

	const SHudDofDefaults* Defaults = nullptr;
	Fvector4 Talk = {-1.0f, -1.0f, -1.0f, -1.0f};
	xr_hash_set<u16> Zones;
	Fvector Base = {-1.25f, 1.4f, 600.0f};
	Fvector Effector = Base;
	Fvector Current = Base;
	Fvector From = Base;
	Fvector Target = Base;
	float Progress = 0.0f;
	float Speed = 5.0f;
	float ReturnSpeed = 1.0f;
	bool HasEffector = false;
	bool Pickable = false;
	bool HadRequest = false;
};

#include "StdAfx.h"
#include "DofManager.h"
#include "Actor.h"
#include "Inventory.h"
#include "Weapon.h"
#include "HudAnimatorManager.h"
#include "HUDManager.h"
#include "Level.h"

SDofOptions g_DofOptions;

namespace DofConfig
{
static float ReadFloat(const shared_str& Section, const char* Key, float Default)
{
	const float Value = pSettings->read_if_exists<float>(Section.c_str(), Key, Default);
	return _valid(Value) ? Value : Default;
}

static float ReadSpeed(const shared_str& Section, const char* Key, float Default)
{
	return clampr(ReadFloat(Section, Key, Default), 0.0f, 1000.0f);
}

static Fvector ReadVector(const shared_str& Section, const char* Prefix, const Fvector& Default, Flags8* Fields = nullptr, Fvector* Overrides = nullptr)
{
	Fvector Result;
	const char* Suffixes[] = {"near", "focus", "far"};
	const u8 Bits[] = {DofNear, DofFocus, DofFar};
	for (u32 Index = 0; Index < 3; ++Index)
	{
		const shared_str Key = shared_str().printf("%s_%s", Prefix, Suffixes[Index]);
		Result[Index] = ReadFloat(Section, Key.c_str(), Default[Index]);
		if (Fields)
		{
			Fields->set(Bits[Index], pSettings->line_exist(Section.c_str(), Key.c_str()));
		}
	}
	if (Overrides)
	{
		*Overrides = Result;
	}
	return Result.x <= Result.y && Result.y <= Result.z ? Result : Default;
}

static bool DefaultAction(xr_string_view Name)
{
	return Name.starts_with("anm_reload") || Name.starts_with("anm_open") ||
		   Name.starts_with("anm_close") || Name.starts_with("anm_add_cartridge");
}

static SHudDofActionDesc ReadAction(const shared_str& Section, const shared_str& Motion, const SHudDofActionDesc& Default)
{
	SHudDofActionDesc Result = Default;
	Result.Fields.zero();
	const shared_str EnableKey = shared_str().printf("use_dof_%s", Motion.c_str());
	const bool HasEnable = pSettings->line_exist(Section.c_str(), EnableKey.c_str());
	Result.Fields.set(DofExplicitEnable, HasEnable);
	Result.Fields.set(DofEnabled, pSettings->read_if_exists<bool>(Section.c_str(), EnableKey.c_str(), Default.Fields.test(DofEnabled)));
	Result.Request.Values = ReadVector(Section, shared_str().printf("dof_%s", Motion.c_str()).c_str(), Default.Request.Values, &Result.Fields, &Result.Overrides);
	const shared_str SpeedInKey = shared_str().printf("dof_speed_in_%s", Motion.c_str());
	const shared_str SpeedOutKey = shared_str().printf("dof_speed_out_%s", Motion.c_str());
	const shared_str TimeKey = shared_str().printf("dof_time_offset_%s", Motion.c_str());
	Result.Fields.set(DofSpeedIn, pSettings->line_exist(Section.c_str(), SpeedInKey.c_str()));
	Result.Fields.set(DofSpeedOut, pSettings->line_exist(Section.c_str(), SpeedOutKey.c_str()));
	Result.Fields.set(DofTimeOffset, pSettings->line_exist(Section.c_str(), TimeKey.c_str()));
	Result.Request.SpeedIn = ReadSpeed(Section, SpeedInKey.c_str(), Default.Request.SpeedIn);
	Result.Request.SpeedOut = ReadSpeed(Section, SpeedOutKey.c_str(), Default.Request.SpeedOut);
	Result.TimeOffset = ReadFloat(Section, TimeKey.c_str(), Default.TimeOffset);
	return Result;
}

static void MergeAction(SHudDofActionDesc& Target, const SHudDofActionDesc& Source)
{
	if (Source.Fields.test(DofExplicitEnable))
	{
		Target.Fields.set(DofEnabled, Source.Fields.test(DofEnabled));
	}
	const u8 Bits[] = {DofNear, DofFocus, DofFar};
	for (u32 Index = 0; Index < 3; ++Index)
	{
		if (Source.Fields.test(Bits[Index]))
		{
			Target.Request.Values[Index] = Source.Overrides[Index];
		}
	}
	if (Source.Fields.test(DofSpeedIn))
	{
		Target.Request.SpeedIn = Source.Request.SpeedIn;
	}
	if (Source.Fields.test(DofSpeedOut))
	{
		Target.Request.SpeedOut = Source.Request.SpeedOut;
	}
	if (Source.Fields.test(DofTimeOffset))
	{
		Target.TimeOffset = Source.TimeOffset;
	}
}

static bool ReadLegacy(const shared_str& Section, const char* Key, SHudDofActionDesc& Action)
{
	if (!pSettings->line_exist(Section.c_str(), Key))
	{
		return false;
	}
	const Fvector4 Value = pSettings->r_fvector4(Section.c_str(), Key);
	if (!_valid(Value.x) || !_valid(Value.y) || !_valid(Value.z) || !_valid(Value.w) ||
		Value.w < 0.0f || Value.x > Value.y || Value.y > Value.z)
	{
		return false;
	}
	Action.Request.Values.set(Value.x, Value.y, Value.z);
	Action.TimeOffset = Value.w;
	Action.Fields.set(DofEnabled, true);
	return true;
}

static shared_str MotionFromKey(xr_string_view Key)
{
	if (Key.starts_with("anm_"))
	{
		return shared_str(xr_string(Key.data(), static_cast<u32>(Key.size())).c_str());
	}
	const xr_string_view Prefixes[] = {"use_dof_", "dof_speed_in_", "dof_speed_out_", "dof_time_offset_"};
	for (const xr_string_view Prefix : Prefixes)
	{
		if (Key.starts_with(Prefix))
		{
			return shared_str(xr_string(Key.data() + Prefix.size(), static_cast<u32>(Key.size() - Prefix.size())).c_str());
		}
	}
	if (Key.starts_with("dof_"))
	{
		const xr_string_view Suffixes[] = {"_near", "_focus", "_far"};
		for (const xr_string_view Suffix : Suffixes)
		{
			if (Key.size() > 4 + Suffix.size() && Key.ends_with(Suffix))
			{
				return shared_str(xr_string(Key.data() + 4, static_cast<u32>(Key.size() - 4 - Suffix.size())).c_str());
			}
		}
	}
	return {};
}

static float ReleaseTime(const SHudDofActionDesc& Action, u32 Duration)
{
	const float Length = Duration / 1000.0f;
	float Release = flt_max;
	if (Action.TimeOffset > 0.0f)
	{
		Release = Action.TimeOffset;
	}
	else if (Duration != 0)
	{
		Release = std::max(0.0f, Length + Action.TimeOffset);
	}
	return Duration != 0 ? std::min(Release, Length) : Release;
}
} // namespace DofConfig

void SHudDofDefaults::Load(const shared_str& Section)
{
	Zoom = DofConfig::ReadVector(Section, "default_zoom_dof", Zoom);
	Action.Request.Values = DofConfig::ReadVector(Section, "default_action_dof", Action.Request.Values);
	Speed = DofConfig::ReadSpeed(Section, "default_dof_speed", Speed);
	Action.Request.SpeedIn = DofConfig::ReadSpeed(Section, "default_dof_speed_in", Action.Request.SpeedIn);
	Action.Request.SpeedOut = DofConfig::ReadSpeed(Section, "default_dof_speed_out", Action.Request.SpeedOut);
	Action.TimeOffset = DofConfig::ReadFloat(Section, "default_dof_time_offset", Action.TimeOffset);
	PickNear = std::min(DofConfig::ReadFloat("zone_pick_dof", "near", PickNear), 0.0f);
	PickFar = std::max(DofConfig::ReadFloat("zone_pick_dof", "far", PickFar), 0.0f);
}

void SHudDofDesc::Load(const shared_str& Section)
{
	const SHudDofDefaults& Defaults = SHudDofDefaults::Registry::Get("gunslinger_base");
	Zoom = {DofConfig::ReadVector(Section, "zoom_dof", Defaults.Zoom), DofConfig::ReadSpeed(Section, "zoom_in_dof_speed", Defaults.Action.Request.SpeedIn), DofConfig::ReadSpeed(Section, "zoom_out_dof_speed", Defaults.Action.Request.SpeedOut)};
	AlterZoom = {DofConfig::ReadVector(Section, "alter_zoom_dof", Zoom.Values), DofConfig::ReadSpeed(Section, "alter_zoom_in_dof_speed", Defaults.Action.Request.SpeedIn), Zoom.SpeedOut};
	Grenade = {DofConfig::ReadVector(Section, "gl_dof", Defaults.Zoom), DofConfig::ReadSpeed(Section, "gl_in_dof_speed", Defaults.Action.Request.SpeedIn), Zoom.SpeedOut};
	Lens = {DofConfig::ReadVector(Section, "lens_dof", Defaults.Zoom, &LensFields, &LensOverrides), Zoom.SpeedIn, Zoom.SpeedOut};
	Pda = {DofConfig::ReadVector(Section, "zoom_dof", Defaults.Action.Request.Values), Zoom.SpeedIn, Zoom.SpeedOut};
	DisableZoom = pSettings->read_if_exists<bool>(Section.c_str(), "disable_zoom_dof", false);
	Reload = Defaults.Action;
	DofConfig::ReadLegacy(Section, "reload_dof", Reload);

	if (Section.size() && pSettings->section_exist(Section.c_str()))
	{
		xr_hash_set<shared_str> Motions;
		for (const CInifile::Item& Entry : pSettings->r_section(Section).Data)
		{
			const shared_str Motion = DofConfig::MotionFromKey(Entry.first.c_str());
			if (Motion.size())
			{
				Motions.insert(Motion);
			}
		}
		for (const shared_str& Motion : Motions)
		{
			SHudDofActionDesc Default = Defaults.Action;
			Default.Fields.set(DofEnabled, DofConfig::DefaultAction(Motion.c_str()));
			Actions.emplace(Motion, DofConfig::ReadAction(Section, Motion, Default));
		}
	}
	Usable = Defaults.Action;
	DofConfig::ReadLegacy(Section, "dof", Usable);
	Usable.Fields.set(DofEnabled, true);
	const Fvector UsableFallback = Usable.Request.Values;
	// Resolve native and original aliases from cached fields, without reading them again.
	for (const shared_str& Motion : {shared_str("anm_show"), shared_str("anm_use")})
	{
		const auto It = Actions.find(Motion);
		if (It != Actions.end())
		{
			DofConfig::MergeAction(Usable, It->second);
		}
	}
	if (Usable.Request.Values.x > Usable.Request.Values.y || Usable.Request.Values.y > Usable.Request.Values.z)
	{
		Usable.Request.Values = UsableFallback;
	}
}

void CDofManager::Initialize()
{
	Defaults = &SHudDofDefaults::Registry::Get("gunslinger_base");
	Talk = EngineExternal().GetTalkDof();
	Reset();
}

void CDofManager::LoadHud(SHudDofSource& Source, const shared_str& Section, bool Usable)
{
	if (Source.HudSection == Section && Source.Usable == Usable)
	{
		return;
	}
	StopAction(Source);
	Source.HudSection = Section;
	Source.Hud = Section.size() ? &SHudDofDesc::Registry::Get(Section) : nullptr;
	Source.Usable = Usable;
}

void CDofManager::LoadItem(SHudDofSource& Source, const shared_str& Section)
{
	if (Source.ItemSection == Section)
	{
		return;
	}
	Source.ItemSection = Section;
	Source.Item = Section.size() ? &SHudDofDesc::Registry::Get(Section) : nullptr;
}

void CDofManager::LoadScope(SHudDofSource& Source, const shared_str& Section)
{
	if (Source.ScopeSection == Section)
	{
		return;
	}
	Source.ScopeSection = Section;
	Source.Scope = Section.size() ? &SHudDofDesc::Registry::Get(Section) : nullptr;
}

void CDofManager::StartAction(SHudDofSource& Source, const shared_str& Motion, u32 Duration)
{
	StopAction(Source);
	Source.StartTime = Device.dwTimeGlobal;
	Source.Duration = Duration;
	Source.Playing = true;
	if (!Source.Hud)
	{
		return;
	}
	if (Source.Usable && Motion == "anm_show")
	{
		Source.Action = &Source.Hud->Usable;
	}
	else
	{
		const auto It = Source.Hud->Actions.find(Motion);
		if (It != Source.Hud->Actions.end())
		{
			Source.Action = &It->second;
		}
	}
	if (Source.Action)
	{
		Source.ReleaseAfter = DofConfig::ReleaseTime(*Source.Action, Duration);
	}
}

void CDofManager::StopAction(SHudDofSource& Source)
{
	Source.Action = nullptr;
	Source.Duration = 0;
	Source.Playing = false;
}

bool CDofManager::GetTimedAction(const SHudDofSource& Source, const SHudDofActionDesc& Action, SHudDofRequest& Request) const
{
	if (!Source.Playing || !Action.Fields.test(DofEnabled) || (Device.dwTimeGlobal - Source.StartTime) / 1000.0f >= DofConfig::ReleaseTime(Action, Source.Duration))
	{
		return false;
	}
	Request = Action.Request;
	return true;
}

bool CDofManager::GetAction(const SHudDofSource& Source, bool Animator, SHudDofRequest& Request) const
{
	if (!(Animator ? g_DofOptions.HudAnimator : g_DofOptions.Dynamic) || !Source.Action ||
		!Source.Action->Fields.test(DofEnabled) || (Device.dwTimeGlobal - Source.StartTime) / 1000.0f >= Source.ReleaseAfter)
	{
		return false;
	}
	Request = Source.Action->Request;
	return true;
}

Fvector CDofManager::PickFocus() const
{
	const float Focus = HUD().GetCurrentRayQuery().range;
	return Fvector().set(Focus + Defaults->PickNear, Focus, Focus + Defaults->PickFar);
}

bool CDofManager::SelectRequest(SHudDofRequest& Request) const
{
	CActor* Actor = g_pGameLevel && Level().bReady && Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!Actor || !Actor->g_Alive() || !Actor->HUDview())
	{
		return false;
	}
	if (Actor->IsTalking())
	{
		CInventoryOwner* Partner = Actor->GetTalkPartner();
		if (g_DofOptions.Talk && Talk.w >= 0.0f && Partner && Partner->GetFocusingOnNpc())
		{
			Fvector Position;
			Partner->cast_game_object()->Center(Position);
			Position.sub(Device.vCameraPosition);
			const float Focus = std::max(Position.dotproduct(Device.vCameraDirection), EPS_L);
			Request = {Fvector().set(Focus + Talk.x - Talk.y, Focus, Focus + Talk.z - Talk.y), Defaults->Speed, Defaults->Speed};
			return true;
		}
		return false;
	}
	if (Pickable || !Zones.empty())
	{
		Request = {PickFocus(), Defaults->Speed, Defaults->Speed};
		return true;
	}
	CHudAnimatorManager* AnimatorManager = Actor->HudAnimator();
	if (CHudAnimatorBase* Animator = AnimatorManager ? AnimatorManager->CurrentAnimator() : nullptr)
	{
		const SHudDofSource& Source = Animator->DofSource;
		if (GetAction(Source, true, Request))
		{
			return true;
		}
		if (Animator->cast_pda_animator() && g_DofOptions.HudAnimator && Source.Hud && !Source.Hud->DisableZoom)
		{
			Request = Source.Hud->Pda;
			return true;
		}
		return false;
	}
	CInventoryItem* Item = Actor->inventory().ActiveItem();
	CHudItem* HudItem = Item ? Item->cast_hud_item() : nullptr;
	if (!HudItem || HudItem->IsHidden() || !HudItem->HudItemData())
	{
		return false;
	}
	const SHudDofSource& Source = HudItem->DofSource;
	if (GetAction(Source, false, Request))
	{
		return true;
	}
	CWeapon* Weapon = Item->cast_weapon();
	if (!Weapon)
	{
		return false;
	}
	if (!g_DofOptions.Dynamic && g_DofOptions.Reload && Weapon->GetState() == CWeapon::eReload && Source.Item &&
		GetTimedAction(Source, Source.Item->Reload, Request))
	{
		return true;
	}
	if (!Weapon->IsZoomed() || !Source.Hud || Source.Hud->DisableZoom || (Source.Item && Source.Item->DisableZoom))
	{
		return false;
	}
	const SHudDofDesc& Desc = *Source.Hud;
	if (Weapon->IsLensedScopeInstalled() && Weapon->Allow3DScopes() && !Weapon->IsAltZoomed() && !Weapon->IsGrenadeMode())
	{
		Request = Desc.Lens;
		if (Source.Scope)
		{
			const SHudDofDesc& Scope = *Source.Scope;
			if (Scope.LensFields.test(DofNear))
			{
				Request.Values.x = Scope.LensOverrides.x;
			}
			if (Scope.LensFields.test(DofFocus))
			{
				Request.Values.y = Scope.LensOverrides.y;
			}
			if (Scope.LensFields.test(DofFar))
			{
				Request.Values.z = Scope.LensOverrides.z;
			}
			if (Request.Values.x > Request.Values.y || Request.Values.y > Request.Values.z)
			{
				Request.Values = Desc.Lens.Values;
			}
		}
		return true;
	}
	if (g_DofOptions.ConstantZoom)
	{
		if (Weapon->IsGrenadeMode())
		{
			Request = Desc.Grenade;
		}
		else if (Weapon->IsAltZoomed())
		{
			Request = Desc.AlterZoom;
		}
		else if (!Weapon->IsScopeAttached())
		{
			Request = Desc.Zoom;
		}
		else
		{
			return false;
		}
		return true;
	}
	if (!Weapon->IsAltZoomed() && (Weapon->IsCollimatorInstalled() || Weapon->IsLensedScopeInstalled()))
	{
		return false;
	}
	Request = {PickFocus(), Defaults->Speed, Desc.Zoom.SpeedOut};
	return true;
}

void CDofManager::SetBase(const Fvector& Values)
{
	Base = Values;
}

void CDofManager::SetEffector(const Fvector& Values)
{
	Effector = Values;
	HasEffector = true;
}

void CDofManager::RestoreEffector()
{
	HasEffector = false;
}

void CDofManager::SetPickable(bool Active)
{
	Pickable = Active;
}

void CDofManager::SetZoneActive(u16 Id, bool Active)
{
	if (Active)
	{
		Zones.insert(Id);
	}
	else
	{
		Zones.erase(Id);
	}
}

void CDofManager::Blend(const Fvector& Values, float Rate, float Delta)
{
	if (!Target.similar(Values) || Speed != Rate)
	{
		From = Current;
		Target = Values;
		Speed = Rate;
		Progress = 0.0f;
	}
	Progress = clampr(Progress + std::max(Delta, 0.0f) * Speed, 0.0f, 1.0f);
	Current.lerp(From, Target, Progress);
}

void CDofManager::Update(float Delta)
{
	if (!Defaults || g_dedicated_server)
	{
		return;
	}
	SHudDofRequest Request;
	if (SelectRequest(Request))
	{
		ReturnSpeed = Request.SpeedOut;
		HadRequest = true;
		Blend(Request.Values, Request.SpeedIn, Delta);
	}
	else
	{
		const Fvector& Values = HasEffector ? Effector : Base;
		Blend(Values, HadRequest ? ReturnSpeed : Defaults->Speed, Delta);
		if (Current.similar(Values))
		{
			HadRequest = false;
		}
	}
}

void CDofManager::Reset()
{
	Zones.clear();
	Pickable = false;
	HasEffector = false;
	HadRequest = false;
	Current = From = Target = Base;
	Progress = 0.0f;
	Speed = Defaults ? Defaults->Speed : 5.0f;
	ReturnSpeed = 1.0f;
}

void CDofManager::GetCurrent(Fvector& Result) const
{
	Result = Current;
}

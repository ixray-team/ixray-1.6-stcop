#include "StdAfx.h"
#include "WeaponBoneVisibility.h"
#include "Components/WeaponAmmoBones.h"
#include "../../Include/xrRender/Kinematics.h"

namespace WeaponBoneVisibility
{
LPCSTR ReadString(LPCSTR SectionName, LPCSTR Field)
{
	// Empty LTX values are stored as null shared strings, even when the field exists.
	LPCSTR Value = pSettings->read_if_exists<str_c>(SectionName, Field, nullptr);
	return Value ? Value : "";
}

void SetBone(IKinematics& Model, const shared_str& Bone, bool IsVisible, bool IsRecursive)
{
	const u16 ID = Model.LL_BoneID(Bone);
	if (ID != BI_NONE)
	{
		Model.LL_SetBoneVisible(ID, IsVisible, IsRecursive);
	}
}

static void SetBones(IKinematics& Model, xr_span<const shared_str> Bones, bool IsVisible)
{
	for (const auto& Bone : Bones)
	{
		SetBone(Model, Bone, IsVisible);
	}
}

void ApplyAddons(IKinematics& Model, const AddonState& State)
{
	SetBone(Model, "wpn_scope", State.HasScope, true);
	SetBone(Model, "wpn_silencer", State.HasSilencer, true);
	SetBone(Model, "wpn_launcher", State.HasLauncher, true);
	SetBones(Model, State.DefaultHide, false);
	SetBones(Model, State.DefaultShow, true);
	SetBones(Model, State.UpgradeHide, false);
	SetBones(Model, State.UpgradeShow, true);
	SetBones(Model, State.InstalledHide, false);
	SetBones(Model, State.InstalledShow, true);
	SetBones(Model, State.OverrideHide, false);
	if (State.HasSilencer)
	{
		SetBones(Model, State.SilencerHide, false);
		SetBones(Model, State.DefaultSilencerHide, false);
	}
	if (State.HasScope)
	{
		SetBones(Model, State.ScopeHide, false);
		SetBones(Model, State.DefaultScopeHide, false);
	}
	if (State.HasLauncher)
	{
		SetBones(Model, State.LauncherHide, false);
		SetBones(Model, State.DefaultLauncherHide, false);
	}
}

static void AppendBones(LPCSTR SectionName, LPCSTR Field, xr_vector<shared_str>& Bones)
{
	VisitBoneList(SectionName, Field, false, false, [&](const shared_str& Bone, bool, bool)
				  { Bones.push_back(Bone); });
}

static void CollectUpgradeBonesRecursive(LPCSTR SectionName, xr_vector<shared_str>& Bones, xr_hash_set<shared_str>& Visited)
{
	if (!Visited.emplace(SectionName).second)
	{
		return;
	}
	LPCSTR Elements = ReadString(SectionName, "elements");
	for (int Index = 0, Count = _GetItemCount(Elements); Index < Count; ++Index)
	{
		string256 Element;
		_GetItem(Elements, Index, Element);
		LPCSTR Effects = ReadString(Element, "effects");
		for (int EffectIndex = 0, EffectCount = _GetItemCount(Effects); EffectIndex < EffectCount; ++EffectIndex)
		{
			string256 Effect;
			_GetItem(Effects, EffectIndex, Effect);
			CollectUpgradeBonesRecursive(Effect, Bones, Visited);
		}
		LPCSTR Upgrade = pSettings->read_if_exists<str_c>(Element, "section", nullptr);
		if (Upgrade && *Upgrade)
		{
			AppendBones(Upgrade, "show_bones", Bones);
		}
	}
}

void CollectUpgradeBones(LPCSTR SectionName, xr_vector<shared_str>& Bones)
{
	xr_hash_set<shared_str> Visited;
	CollectUpgradeBonesRecursive(SectionName, Bones, Visited);
}

void ConfigureDefault(IKinematics& Model, LPCSTR SectionName)
{
	xr_vector<shared_str> InstalledSections;
	LPCSTR Installed = ReadString(SectionName, "installed_upgrades");
	for (int Index = 0, Count = _GetItemCount(Installed); Index < Count; ++Index)
	{
		string256 Upgrade;
		_GetItem(Installed, Index, Upgrade);
		LPCSTR Parameters = pSettings->read_if_exists<str_c>(Upgrade, "section", nullptr);
		if (Parameters && *Parameters)
		{
			InstalledSections.emplace_back(Parameters);
		}
	}
	const auto ReadStatus = [&](LPCSTR Field)
	{
		u32 Status = pSettings->read_if_exists<u32>(SectionName, Field, 0);
		for (const auto& Parameters : InstalledSections)
		{
			Status = pSettings->read_if_exists<u32>(Parameters, Field, Status);
		}
		return Status;
	};
	const auto ReadDefaultBones = [&](LPCSTR Field)
	{
		xr_vector<shared_str> Bones;
		AppendBones(SectionName, Field, Bones);
		return Bones;
	};
	const auto ReadInstalledBones = [&](LPCSTR Field)
	{
		xr_vector<shared_str> Bones;
		for (const auto& Parameters : InstalledSections)
		{
			AppendBones(Parameters.c_str(), Field, Bones);
		}
		return Bones;
	};
	const auto DefaultHide = ReadDefaultBones("def_hide_bones");
	const auto DefaultShow = ReadDefaultBones("def_show_bones");
	const auto InstalledHide = ReadInstalledBones("hide_bones");
	const auto InstalledShow = ReadInstalledBones("show_bones");
	const auto OverrideHide = ReadInstalledBones("hide_bones_override");
	const auto SilencerHide = ReadInstalledBones("hide_bones_override_when_silencer_attached");
	const auto DefaultSilencerHide = ReadDefaultBones("def_hide_bones_override_when_silencer_attached");
	const auto ScopeHide = ReadInstalledBones("hide_bones_override_when_scope_attached");
	const auto DefaultScopeHide = ReadDefaultBones("def_hide_bones_override_when_scope_attached");
	const auto LauncherHide = ReadInstalledBones("hide_bones_override_when_gl_attached");
	const auto DefaultLauncherHide = ReadDefaultBones("def_hide_bones_override_when_gl_attached");
	xr_vector<shared_str> UpgradeHide;
	LPCSTR Upgrades = ReadString(SectionName, "upgrades");
	for (int Index = 0, Count = _GetItemCount(Upgrades); Index < Count; ++Index)
	{
		string256 Upgrade;
		_GetItem(Upgrades, Index, Upgrade);
		CollectUpgradeBones(Upgrade, UpgradeHide);
	}
	const u32 ScopeStatus = ReadStatus("scope_status");
	ApplyAddons(Model, {.HasScope = ScopeStatus == 1, .HasSilencer = ReadStatus("silencer_status") == 1, .HasLauncher = ReadStatus("grenade_launcher_status") == 1, .DefaultHide = DefaultHide, .DefaultShow = DefaultShow, .UpgradeHide = UpgradeHide, .UpgradeShow = InstalledShow, .InstalledHide = InstalledHide, .InstalledShow = InstalledShow, .OverrideHide = OverrideHide, .SilencerHide = SilencerHide, .DefaultSilencerHide = DefaultSilencerHide, .ScopeHide = ScopeHide, .DefaultScopeHide = DefaultScopeHide, .LauncherHide = LauncherHide, .DefaultLauncherHide = DefaultLauncherHide});

	xr_vector<shared_str> Scopes;
	const auto AppendScopes = [&](LPCSTR Parameters, u32 Status)
	{
		if (Status != 2 && (Parameters == SectionName || Status != 1))
		{
			return;
		}
		LPCSTR Field = pSettings->line_exist(Parameters, "scopes") ? "scopes" : "scopes_sect";
		LPCSTR List = ReadString(Parameters, Field);
		if (!*List || xr_strcmp(List, "none") == 0)
		{
			Field = "scopes_sect";
			List = ReadString(Parameters, Field);
		}
		if (*List)
		{
			AppendBones(Parameters, Field, Scopes);
		}
		else
		{
			Scopes.emplace_back(Parameters);
		}
	};
	AppendScopes(SectionName, pSettings->read_if_exists<u32>(SectionName, "scope_status", 0));
	for (const auto& Parameters : InstalledSections)
	{
		if (pSettings->line_exist(Parameters, "scope_status"))
		{
			AppendScopes(Parameters.c_str(), pSettings->r_u32(Parameters, "scope_status"));
		}
	}
	const auto NoScopeHide = ReadDefaultBones("no_scope_overriding_hide_bones");
	const auto NoScopeShow = ReadDefaultBones("no_scope_overriding_show_bones");
	VisitScopeBones(Scopes, nullptr, NoScopeHide, NoScopeShow, [&](const shared_str& Bone, bool IsVisible, bool IsRecursive)
					{ SetBone(Model, Bone, IsVisible, IsRecursive); });
	TAmmoBones::ApplyDefaultIconBones(Model, SectionName, InstalledSections);
	Model.CalculateBones_Invalidate();
}
} // namespace WeaponBoneVisibility

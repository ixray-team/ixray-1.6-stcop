#pragma once

class IKinematics;

namespace WeaponBoneVisibility
{
class AddonState final
{
public:
	bool HasScope = false;
	bool HasSilencer = false;
	bool HasLauncher = false;
	xr_span<const shared_str> DefaultHide;
	xr_span<const shared_str> DefaultShow;
	xr_span<const shared_str> UpgradeHide;
	xr_span<const shared_str> UpgradeShow;
	xr_span<const shared_str> InstalledHide;
	xr_span<const shared_str> InstalledShow;
	xr_span<const shared_str> OverrideHide;
	xr_span<const shared_str> SilencerHide;
	xr_span<const shared_str> DefaultSilencerHide;
	xr_span<const shared_str> ScopeHide;
	xr_span<const shared_str> DefaultScopeHide;
	xr_span<const shared_str> LauncherHide;
	xr_span<const shared_str> DefaultLauncherHide;
};

void SetBone(IKinematics& Model, const shared_str& Bone, bool IsVisible, bool IsRecursive = false);
const char* ReadString(const char* SectionName, const char* Field);
void ApplyAddons(IKinematics& Model, const AddonState& State);
void CollectUpgradeBones(const char* SectionName, xr_vector<shared_str>& Bones);
void ConfigureDefault(IKinematics& Model, const char* SectionName);

template <typename TVisitor>
void VisitBoneList(const char* SectionName, const char* Field, bool IsVisible, bool IsRecursive, const TVisitor& Visitor)
{
	const char* List = ReadString(SectionName, Field);
	for (int Index = 0, Count = _GetItemCount(List); Index < Count; ++Index)
	{
		string256 Bone;
		_GetItem(List, Index, Bone);
		Visitor(shared_str(Bone), IsVisible, IsRecursive);
	}
}

template <typename TVisitor>
void VisitScopeBones(xr_span<const shared_str> Scopes, const char* AttachedSection, xr_span<const shared_str> NoScopeHide, xr_span<const shared_str> NoScopeShow, const TVisitor& Visitor)
{
	for (const auto& Scope : Scopes)
	{
		const bool IsAttached = AttachedSection &&
								(Scope == AttachedSection || xr_strcmp(ReadString(Scope.c_str(), "scope_name"), AttachedSection) == 0);
		VisitBoneList(Scope.c_str(), "bones", IsAttached, false, Visitor);
		VisitBoneList(Scope.c_str(), "hide_bones", !IsAttached, false, Visitor);
		if (IsAttached)
		{
			VisitBoneList(Scope.c_str(), "overriding_hide_bones", false, false, Visitor);
			VisitBoneList(Scope.c_str(), "overriding_show_bones", true, false, Visitor);
		}
	}
	if (!AttachedSection)
	{
		for (const auto& Bone : NoScopeHide)
		{
			Visitor(Bone, false, true);
		}
		for (const auto& Bone : NoScopeShow)
		{
			Visitor(Bone, true, true);
		}
	}
}
} // namespace WeaponBoneVisibility

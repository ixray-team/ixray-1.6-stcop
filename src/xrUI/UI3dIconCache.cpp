#include "stdafx.h"
#include "UI3dIconCache.h"
#include "../xrEngine/Render.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/KinematicsAnimated.h"

namespace UI3dIconNames
{
static xr_string Normalize(const shared_str& Name)
{
	xr_string Key = Name ? Name.c_str() : "";
	xr_strlwr(Key.data());
	if (Key.ends_with(".ogf"))
	{
		Key.resize(Key.size() - 4);
	}
	return Key;
}
} // namespace UI3dIconNames

UI3dIconCache& UI3dIconCache::Instance()
{
	static UI3dIconCache Cache;
	return Cache;
}

IRenderVisual* UI3dIconCache::Acquire(const shared_str& Name)
{
	xrCriticalSectionGuard Guard(Lock);
	if (!Name || !*Name.c_str())
	{
		return nullptr;
	}
	const xr_string Key = UI3dIconNames::Normalize(Name);
	auto It = Models.find(Key);
	if (It == Models.end())
	{
		const xr_string Path = Key + ".ogf";
		if (!FS.exist(_game_meshes_, Path.c_str()))
		{
			return nullptr;
		}
		auto* Prototype = ::Render->model_Create(Key.c_str());
		if (!Prototype)
		{
			return nullptr;
		}
		if (!Prototype->dcast_PKinematics())
		{
			::Render->model_Delete(Prototype);
			return nullptr;
		}
		It = Models.emplace(Key, ModelEntry{Prototype, 0}).first;
	}
	// Skinning and visibility caches belong to a skeleton instance, not a bone mask.
	auto* Visual = ::Render->model_Duplicate(It->second.Visual);
	if (!Visual)
	{
		if (It->second.References == 0)
		{
			::Render->model_Delete(It->second.Visual);
			Models.erase(It);
		}
		return nullptr;
	}
	++It->second.References;
	Instances.emplace(Visual, Key);
	if (auto* Animated = Visual->dcast_PKinematicsAnimated())
	{
		auto Motion = Animated->ID_Cycle_Safe("idle");
		if (!Motion)
		{
			Motion.set(0, 0);
		}
		Animated->PlayCycle(Motion, false);
	}
	return Visual;
}

void UI3dIconCache::Release(IRenderVisual* Visual)
{
	xrCriticalSectionGuard Guard(Lock);
	const auto Instance = Instances.find(Visual);
	if (Instance == Instances.end())
	{
		return;
	}
	const auto It = Models.find(Instance->second);
	Instances.erase(Instance);
	::Render->model_Delete(Visual, true);
	if (--It->second.References == 0)
	{
		::Render->model_Delete(It->second.Visual);
		Models.erase(It);
	}
}

bool UI3dIconCache::Matches(IRenderVisual* Visual, const shared_str& Name) const
{
	xrCriticalSectionGuard Guard(Lock);
	const auto It = Instances.find(Visual);
	return It != Instances.end() && It->second == UI3dIconNames::Normalize(Name);
}

size_t UI3dIconCache::GetModelCount() const
{
	xrCriticalSectionGuard Guard(Lock);
	return Models.size();
}

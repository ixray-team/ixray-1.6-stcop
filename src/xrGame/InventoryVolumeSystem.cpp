//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////// Desert Cliff ////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "InventoryVolumeSystem.h"

#include "ActorBackpack.h"
#include "attachable_item.h"
#include "attachment_owner.h"
#include "CustomOutfit.h"
#include "GameObject.h"
#include "inventory_item.h"
#include "Inventory.h"
#include "InventoryOwner.h"
#include "PhysicsShellHolder.h"

namespace
{
constexpr const char* kVolumeSystemFile = "volume_system.ltx";
constexpr u32 kMaxChildVolumeDepth = 8;

constexpr const char* kSectionSystem = "volume_system";
constexpr const char* kSectionPenalties = "soft_penalties";
constexpr const char* kSectionItemVolumes = "item_volumes";
constexpr const char* kSectionContainerProfiles = "container_profiles";
constexpr const char* kSectionContainerSections = "container_sections";
constexpr const char* kSectionContainerOverrides = "container_capacity_overrides";
constexpr const char* kSectionOutfitSections = "outfit_sections";
constexpr const char* kSectionOutfitOverrides = "outfit_capacity_overrides";

void LoadFloatSection(const CInifile& ini, const char* section, xr_map<shared_str, float>& outValues)
{
    if (!ini.section_exist(section))
    {
        return;
    }

    const CInifile::Sect& data = ini.r_section(section);
    for (const CInifile::Item& line : data.Data)
    {
        outValues[line.first] = line.second.size() ? static_cast<float>(atof(line.second.c_str())) : 0.0f;
    }
}

void LoadStringSection(const CInifile& ini, const char* section, xr_map<shared_str, shared_str>& outValues)
{
    if (!ini.section_exist(section))
    {
        return;
    }

    const CInifile::Sect& data = ini.r_section(section);
    for (const CInifile::Item& line : data.Data)
    {
        outValues[line.first] = line.second;
    }
}

bool IsActor(const CInventoryOwner& owner)
{
    return const_cast<CInventoryOwner&>(owner).cast_actor() != nullptr;
}
} // namespace

CInventoryVolumeSystem& CInventoryVolumeSystem::Get()
{
    static CInventoryVolumeSystem volumeSystem;
    return volumeSystem;
}

bool CInventoryVolumeSystem::IsEnabled() const
{
    if (_scriptOverride >= 0)
    {
        return _scriptOverride != 0;
    }

    return EngineExternal()[EEngineExternalGame::EnableInventoryVolume];
}

void CInventoryVolumeSystem::SetScriptEnabled(bool enabled)
{
    const s8 newOverride = enabled ? s8(1) : s8(0);
    if (_scriptOverride == newOverride)
    {
        return;
    }

    _scriptOverride = newOverride;
    _loaded = false;

    if (enabled)
    {
        Load();
    }
}

void CInventoryVolumeSystem::Load()
{
    Reset();
    _loaded = true;

    if (!IsEnabled())
    {
        return;
    }

    LoadFile(kVolumeSystemFile);
}

float CInventoryVolumeSystem::CalcRuckVolume(const CInventoryOwner& owner) const
{
    if (!IsEnabled())
    {
        return 0.0f;
    }

    EnsureLoaded();

    float volume = 0.0f;
    for (const PIItem item : owner.inventory().m_ruck)
    {
        if (item != nullptr)
        {
            volume += GetItemVolume(*item);
        }
    }
    return volume;
}

float CInventoryVolumeSystem::GetCapacity(const CInventoryOwner& owner) const
{
    return GetCapacity(owner, nullptr);
}

float CInventoryVolumeSystem::GetOverloadFactor(const CInventoryOwner& owner) const
{
    if (!IsEnabled())
    {
        return 1.0f;
    }

    const float capacity = GetCapacity(owner);
    if (capacity <= EPS_S)
    {
        return 1.0f;
    }

    return CalcRuckVolume(owner) / capacity;
}

bool CInventoryVolumeSystem::CanAddToRuck(const CInventoryOwner& owner, const CInventoryItem& item) const
{
    if (!IsEnabled() || !_blockPickupAtHardLimit || owner.inventory().InRuck(&item))
    {
        return true;
    }

    if (const_cast<CInventoryItem&>(item).cast_weapon_ammo() != nullptr)
    {
        return true;
    }

    if (!IsActor(owner))
    {
        return true;
    }

    const float capacity = GetCapacity(owner, &item);
    if (capacity <= EPS_S)
    {
        return true;
    }

    const float projectedVolume = CalcRuckVolume(owner) + GetItemVolume(item);
    return projectedVolume <= capacity * _hardOverloadLimit + EPS_S;
}

float CInventoryVolumeSystem::GetItemVolume(const CInventoryItem& item) const
{
    if (!IsEnabled())
    {
        return 0.0f;
    }

    EnsureLoaded();

    const auto explicitVolume = _itemVolumes.find(item.m_section_id);
    if (explicitVolume != _itemVolumes.end())
    {
        return explicitVolume->second;
    }

    float volume = std::max(0.0f, item.Weight() * _defaultWeightVolumeMultiplier);
    if (_recursiveContainerVolume)
    {
        volume += CalcChildItemsVolume(item, 0);
    }
    return volume;
}

SInventoryVolumePenalty CInventoryVolumeSystem::GetPenalty(const CInventoryOwner& owner) const
{
    SInventoryVolumePenalty penalty;
    if (!IsEnabled() || !IsActor(owner))
    {
        return penalty;
    }

    penalty.overloadFactor = GetOverloadFactor(owner);
    if (penalty.overloadFactor <= 1.0f)
    {
        return penalty;
    }

    penalty.curve = SmoothStep(1.0f, _hardOverloadLimit, penalty.overloadFactor);
    penalty.staminaPowerPenalty = _staminaPowerPenalty * penalty.curve;
    penalty.maxWalkWeightPenalty = _maxWalkWeightPenalty * penalty.curve;
    penalty.aimSwayPenalty = _aimSwayPenalty * penalty.curve;
    penalty.blockSprint = penalty.overloadFactor >= _sprintBlockFactor;
    penalty.blockPickup = _blockPickupAtHardLimit && penalty.overloadFactor >= _hardOverloadLimit;
    return penalty;
}

void CInventoryVolumeSystem::EnsureLoaded() const
{
    if (!_loaded)
    {
        const_cast<CInventoryVolumeSystem*>(this)->Load();
    }
}

void CInventoryVolumeSystem::Reset()
{
    _blockPickupAtHardLimit = true;
    _recursiveContainerVolume = false;
    _baseActorVolume = 0.0f;
    _defaultWeightVolumeMultiplier = 1.0f;
    _hardOverloadLimit = 1.4f;
    _staminaPowerPenalty = 0.25f;
    _maxWalkWeightPenalty = 0.0f;
    _aimSwayPenalty = 0.0f;
    _sprintBlockFactor = 1.2f;

    _itemVolumes.clear();
    _containerSections.clear();
    _containerCapacityOverrides.clear();
    _outfitSections.clear();
    _outfitCapacityOverrides.clear();
    _containerProfiles.clear();
}

void CInventoryVolumeSystem::LoadFile(const char* relativePath)
{
    if (!FS.exist(_game_config_, relativePath))
    {
        return;
    }

    string_path path = {};
    FS.update_path(path, _game_config_, relativePath);
    const CInifile ini(path);
    ApplyConfig(ini);
}

void CInventoryVolumeSystem::ApplyConfig(const CInifile& ini)
{
    _baseActorVolume = ini.read_if_exists<float>(kSectionSystem, "base_actor_volume", _baseActorVolume);
    _defaultWeightVolumeMultiplier = ini.read_if_exists<float>(kSectionSystem, "default_weight_volume_multiplier", _defaultWeightVolumeMultiplier);
    _hardOverloadLimit = ini.read_if_exists<float>(kSectionSystem, "hard_overload_limit", _hardOverloadLimit);
    _blockPickupAtHardLimit = ini.read_if_exists<bool>(kSectionSystem, "block_pickup_at_hard_limit", _blockPickupAtHardLimit);
    _recursiveContainerVolume = ini.read_if_exists<bool>(kSectionSystem, "recursive_container_volume", _recursiveContainerVolume);

    _staminaPowerPenalty = ini.read_if_exists<float>(kSectionPenalties, "stamina_power_penalty", _staminaPowerPenalty);
    _maxWalkWeightPenalty = ini.read_if_exists<float>(kSectionPenalties, "max_walk_weight_penalty", _maxWalkWeightPenalty);
    _aimSwayPenalty = ini.read_if_exists<float>(kSectionPenalties, "aim_sway_penalty", _aimSwayPenalty);
    _sprintBlockFactor = ini.read_if_exists<float>(kSectionPenalties, "sprint_block_factor", _sprintBlockFactor);

    LoadFloatSection(ini, kSectionItemVolumes, _itemVolumes);
    LoadFloatSection(ini, kSectionContainerProfiles, _containerProfiles);
    LoadFloatSection(ini, kSectionContainerOverrides, _containerCapacityOverrides);
    LoadFloatSection(ini, kSectionOutfitOverrides, _outfitCapacityOverrides);
    LoadStringSection(ini, kSectionContainerSections, _containerSections);
    LoadStringSection(ini, kSectionOutfitSections, _outfitSections);
}

float CInventoryVolumeSystem::GetCapacity(const CInventoryOwner& owner, const CInventoryItem* ignoredContainer) const
{
    if (!IsEnabled())
    {
        return 0.0f;
    }

    EnsureLoaded();

    float capacity = _baseActorVolume;
    if (capacity <= EPS_S && pSettings->line_exist("inventory", "max_ruck"))
    {
        capacity = pSettings->r_float("inventory", "max_ruck");
    }

    const CCustomOutfit* outfit = owner.GetOutfit();
    if (outfit != nullptr && outfit != ignoredContainer)
    {
        capacity = ApplyContainerCapacity(capacity, outfit->m_section_id, _outfitSections, _outfitCapacityOverrides);
    }

    const CBackpack* backpack = owner.GetBackpack();
    if (backpack != nullptr && backpack != ignoredContainer)
    {
        capacity = ApplyContainerCapacity(capacity, backpack->m_section_id, _containerSections, _containerCapacityOverrides);
    }

    return std::max(0.0f, capacity);
}

float CInventoryVolumeSystem::ApplyContainerCapacity(
    float capacity,
    const shared_str& section,
    const xr_map<shared_str, shared_str>& sectionProfiles,
    const xr_map<shared_str, float>& capacityOverrides) const
{
    const auto profileMapping = sectionProfiles.find(section);
    if (profileMapping != sectionProfiles.end())
    {
        const auto profile = _containerProfiles.find(profileMapping->second);
        if (profile != _containerProfiles.end())
        {
            capacity = profile->second;
        }
    }

    const auto capacityOverride = capacityOverrides.find(section);
    if (capacityOverride != capacityOverrides.end())
    {
        capacity = capacityOverride->second;
    }

    return capacity;
}

float CInventoryVolumeSystem::CalcChildItemsVolume(const CInventoryItem& item, u32 depth) const
{
    if (depth >= kMaxChildVolumeDepth)
    {
        return 0.0f;
    }

    CPhysicsShellHolder& holder = item.object();
    CAttachmentOwner* attachmentOwner = holder.cast_attachment_owner();
    if (attachmentOwner == nullptr)
    {
        return 0.0f;
    }

    float volume = 0.0f;
    for (CAttachableItem* child : attachmentOwner->attached_objects())
    {
        if (child == nullptr)
        {
            continue;
        }

        CInventoryItem& childItem = child->item();
        if (&childItem == &item)
        {
            continue;
        }

        const auto explicitVolume = _itemVolumes.find(childItem.m_section_id);
        if (explicitVolume != _itemVolumes.end())
        {
            volume += explicitVolume->second;
            continue;
        }

        volume += std::max(0.0f, childItem.Weight() * _defaultWeightVolumeMultiplier);
        volume += CalcChildItemsVolume(childItem, depth + 1);
    }
    return volume;
}

float CInventoryVolumeSystem::SmoothStep(float edge0, float edge1, float value) const
{
    if (edge1 <= edge0)
    {
        return value >= edge1 ? 1.0f : 0.0f;
    }

    float x = (value - edge0) / (edge1 - edge0);
    clamp(x, 0.0f, 1.0f);
    return x * x * (3.0f - 2.0f * x);
}

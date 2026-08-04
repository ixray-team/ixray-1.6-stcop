//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////// Desert Cliff ////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////

#pragma once

class CInventoryItem;
class CInventoryOwner;
class CInifile;

struct SInventoryVolumePenalty final
{
    float overloadFactor = 1.0f;
    float curve = 0.0f;
    float staminaPowerPenalty = 0.0f;
    float maxWalkWeightPenalty = 0.0f;
    float aimSwayPenalty = 0.0f;
    bool blockSprint = false;
    bool blockPickup = false;
};

class CInventoryVolumeSystem final
{
public:
    static CInventoryVolumeSystem& Get();

    CInventoryVolumeSystem() = default;
    ~CInventoryVolumeSystem() = default;

    CInventoryVolumeSystem(const CInventoryVolumeSystem&) = delete;
    CInventoryVolumeSystem& operator=(const CInventoryVolumeSystem&) = delete;

    void Load();
    bool IsEnabled() const;
    void SetScriptEnabled(bool enabled);

    float CalcRuckVolume(const CInventoryOwner& owner) const;
    float GetCapacity(const CInventoryOwner& owner) const;
    float GetOverloadFactor(const CInventoryOwner& owner) const;
    bool CanAddToRuck(const CInventoryOwner& owner, const CInventoryItem& item) const;

    float GetItemVolume(const CInventoryItem& item) const;
    SInventoryVolumePenalty GetPenalty(const CInventoryOwner& owner) const;

private:
    void EnsureLoaded() const;
    void Reset();
    void LoadFile(const char* relativePath);
    void ApplyConfig(const CInifile& ini);

    float GetCapacity(const CInventoryOwner& owner, const CInventoryItem* ignoredContainer) const;
    float ApplyContainerCapacity(
        float capacity,
        const shared_str& section,
        const xr_map<shared_str, shared_str>& sectionProfiles,
        const xr_map<shared_str, float>& capacityOverrides) const;
    float CalcChildItemsVolume(const CInventoryItem& item, u32 depth) const;
    float SmoothStep(float edge0, float edge1, float value) const;

private:
    bool _loaded = false;
    // -1: inherit EnableInventoryVolume from engine_external; 0/1: Lua/IXR Options override
    s8 _scriptOverride = -1;
    bool _blockPickupAtHardLimit = true;
    bool _recursiveContainerVolume = false;

    float _baseActorVolume = 0.0f;
    float _defaultWeightVolumeMultiplier = 1.0f;
    float _hardOverloadLimit = 1.4f;

    float _staminaPowerPenalty = 0.25f;
    float _maxWalkWeightPenalty = 0.0f;
    float _aimSwayPenalty = 0.0f;
    float _sprintBlockFactor = 1.2f;

    xr_map<shared_str, float> _itemVolumes;
    xr_map<shared_str, shared_str> _containerSections;
    xr_map<shared_str, float> _containerCapacityOverrides;
    xr_map<shared_str, shared_str> _outfitSections;
    xr_map<shared_str, float> _outfitCapacityOverrides;
    xr_map<shared_str, float> _containerProfiles;
};

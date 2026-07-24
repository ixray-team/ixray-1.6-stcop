#pragma once

#include "inventory_space.h"

class CInventoryItem;
class CInventory;

enum class EInventorySortCategory : u8
{
    All = 0,
    Weapons,
    Ammo,
    Armor,
    Devices,
    Consumables,
    Artefacts,
    Attachments,
    CustomStart = 8
};

enum class EInventorySortSystem : u8
{
    Categories,
    Ordering
};

enum class EInventoryOrderMode : u8
{
    General,
    ByType,
    ByWeight,
    ByCondition,
    ByCost,
    ByImportance,
    ByNovelty
};

struct SInventorySortCategoryInfo
{
    shared_str _id;
    shared_str _name;
    shared_str _hint;
    shared_str _iconTexture;
    bool _hasIcon = false;
    bool _hasText = false;
    xr_set<shared_str> _itemSections;
    xr_set<CLASS_ID> _itemClsids;
    bool _isCustom = false;
};

struct SInventoryOrderModeInfo
{
    shared_str _id;
    shared_str _name;
    shared_str _hint;
    bool _hasText = false;
};

struct SInventoryOrderOptions
{
    bool weightDesc = true;
    bool conditionDesc = true;
    bool costDesc = true;
    bool noveltyDesc = true;
    // 0 = all types grouped; 1..N = focus/filter by type cycle entry
    u8 typeCycle = 0;
};

class CInventorySorter final
{
public:
    CInventorySorter();
    ~CInventorySorter() = default;
    void Initialize();
    void LoadCustomCategories();

    EInventorySortSystem GetSystem() const { return _system; }
    void SetSystem(EInventorySortSystem system);
    bool IsWeightDescending() const { return _weightDesc; }
    bool IsConditionDescending() const { return _conditionDesc; }
    bool IsCostDescending() const { return _costDesc; }
    bool IsNoveltyDescending() const { return _noveltyDesc; }

    EInventorySortCategory GetItemCategory(PIItem item) const;
    bool ItemMatchesCategory(PIItem item, EInventorySortCategory category) const;

    u32 GetCategoriesCount() const { return _categories.size(); }
    EInventorySortCategory GetCategoryByIndex(u32 index) const;
    const SInventorySortCategoryInfo* GetCategoryInfo(EInventorySortCategory category) const;
    EInventorySortCategory GetCategoryById(const shared_str& id) const;
    const SInventorySortCategoryInfo* GetCategoryInfoById(const shared_str& id) const;

    EInventoryOrderMode GetOrderModeById(const shared_str& id) const;
    const SInventoryOrderModeInfo* GetOrderModeInfo(EInventoryOrderMode mode) const;

    u8 GetTypeCycleCount() const;
    EInventorySortCategory GetTypeCycleCategory(u8 cycleIndex) const;
    const SInventorySortCategoryInfo* GetTypeCycleInfo(u8 cycleIndex) const;

    void AddCustomCategory(const shared_str& id, const shared_str& name, const shared_str& hint);
    void AddItemToCustomCategory(const shared_str& categoryId, const shared_str& itemSection);
    void AddClsidToCustomCategory(const shared_str& categoryId, CLASS_ID clsid);

    void SortItems(TIItemContainer& items, EInventorySortCategory category) const;
    void SortItemsById(TIItemContainer& items, const shared_str& categoryId) const;
    void ApplyBagListOrder(TIItemContainer& items, EInventoryOrderMode mode) const;
    void ApplyBagListOrder(TIItemContainer& items, EInventoryOrderMode mode, const SInventoryOrderOptions& options) const;

private:
    void LoadSystemSettings();
    void InitializeDefaultCategories();
    void InitializeDefaultOrderModes();
    void BuildTypeCycleList();
    void LoadCategoryFromXml(const shared_str& categoryId, EInventorySortCategory category);
    void LoadOrderModeFromLtx(const shared_str& orderModeId, EInventoryOrderMode mode);

    u8 GetCategoryPriority(EInventorySortCategory category) const;
    u8 GetCategoryPriority(EInventorySortCategory category, EInventorySortCategory pivot) const;
    bool CompareByType(PIItem item1, PIItem item2, EInventorySortCategory pivot) const;
    bool CompareByWeight(PIItem item1, PIItem item2, bool weightDesc) const;
    bool CompareByCondition(PIItem item1, PIItem item2, bool conditionDesc) const;
    bool CompareByCost(PIItem item1, PIItem item2, bool costDesc) const;
    bool CompareByImportance(PIItem item1, PIItem item2) const;
    bool CompareByNovelty(PIItem item1, PIItem item2, bool noveltyDesc) const;

    bool IsWeapon(PIItem item) const;
    bool IsAmmo(PIItem item) const;
    bool IsArmor(PIItem item) const;
    bool IsDevice(PIItem item) const;
    bool IsConsumable(PIItem item) const;
    bool IsArtefact(PIItem item) const;
    bool IsAttachment(PIItem item) const;
    bool MatchesCustomCategory(PIItem item, const shared_str& categoryId) const;

    EInventorySortSystem _system = EInventorySortSystem::Categories;
    bool _weightDesc = true;
    bool _conditionDesc = true;
    bool _costDesc = true;
    bool _noveltyDesc = true;

    xr_map<EInventorySortCategory, SInventorySortCategoryInfo> _categories;
    xr_map<shared_str, EInventorySortCategory> _idToCategory;
    xr_map<shared_str, EInventorySortCategory> _customCategoryMap;
    u32 _customCategoryCounter = 0;

    xr_map<EInventoryOrderMode, SInventoryOrderModeInfo> _orderModes;
    xr_map<shared_str, EInventoryOrderMode> _idToOrderMode;
    xr_vector<EInventorySortCategory> _typeCycleCategories;
};

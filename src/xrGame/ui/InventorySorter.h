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

class CInventorySorter final
{
public:
    CInventorySorter();
    ~CInventorySorter() = default;
    void Initialize();
    void LoadCustomCategories();
    
    EInventorySortCategory GetItemCategory(PIItem item) const;
    bool ItemMatchesCategory(PIItem item, EInventorySortCategory category) const;
    
    u32 GetCategoriesCount() const { return _categories.size(); }
    EInventorySortCategory GetCategoryByIndex(u32 index) const;
    const SInventorySortCategoryInfo* GetCategoryInfo(EInventorySortCategory category) const;
    EInventorySortCategory GetCategoryById(const shared_str& id) const;
    const SInventorySortCategoryInfo* GetCategoryInfoById(const shared_str& id) const;
    
    void AddCustomCategory(const shared_str& id, const shared_str& name, const shared_str& hint);
    void AddItemToCustomCategory(const shared_str& categoryId, const shared_str& itemSection);
    void AddClsidToCustomCategory(const shared_str& categoryId, CLASS_ID clsid);
    
    void SortItems(TIItemContainer& items, EInventorySortCategory category) const;
    void SortItemsById(TIItemContainer& items, const shared_str& categoryId) const;
    
private:
    void InitializeDefaultCategories();
    void LoadCategoryFromXml(const shared_str& categoryId, EInventorySortCategory category);
    
    bool IsWeapon(PIItem item) const;
    bool IsAmmo(PIItem item) const;
    bool IsArmor(PIItem item) const;
    bool IsDevice(PIItem item) const;
    bool IsConsumable(PIItem item) const;
    bool IsArtefact(PIItem item) const;
    bool IsAttachment(PIItem item) const;
    bool MatchesCustomCategory(PIItem item, const shared_str& categoryId) const;
    xr_map<EInventorySortCategory, SInventorySortCategoryInfo> _categories;
    xr_map<shared_str, EInventorySortCategory> _idToCategory;
    xr_map<shared_str, EInventorySortCategory> _customCategoryMap;
    u32 _customCategoryCounter = 0;
};

#include "stdafx.h"
#include "InventorySorter.h"
#include "UIInventoryUtilities.h"
#include "../inventory_item.h"
#include "../Weapon.h"
#include "../WeaponAmmo.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../Artefact.h"
#include "../CustomDetector.h"
#include "../Torch.h"
#include "../PDA.h"
#include "../CustomDevice.h"
#include "../eatable_item.h"
#include "../medkit.h"
#include "../Scope.h"
#include "../Silencer.h"
#include "../GrenadeLauncher.h"
#include "../Grenade.h"
#include "../Inventory.h"
#include "../../xrServerEntities/object_factory.h"
#include "../../xrCore/xr_ini.h"
#include "../../xrEngine/string_table.h"

CInventorySorter::CInventorySorter()
{
    Initialize();
}

void CInventorySorter::Initialize()
{
    _categories.clear();
    _idToCategory.clear();
    _customCategoryMap.clear();
    _customCategoryCounter = 0;
    _orderModes.clear();
    _idToOrderMode.clear();
    _typeCycleCategories.clear();

    LoadSystemSettings();
    InitializeDefaultCategories();
    LoadCustomCategories();
    InitializeDefaultOrderModes();
    BuildTypeCycleList();
}

void CInventorySorter::SetSystem(EInventorySortSystem system)
{
    _system = system;
}

void CInventorySorter::LoadSystemSettings()
{
    _system = EInventorySortSystem::Categories;
    _weightDesc = true;
    _conditionDesc = true;
    _costDesc = true;
    _noveltyDesc = true;

    if (pSettings->section_exist("inventory_sort"))
    {
        shared_str systemStr = pSettings->read_if_exists<str_c>("inventory_sort", "system", "categories");
        if (systemStr.size() && xr_strcmp(systemStr.c_str(), "ordering") == 0)
        {
            _system = EInventorySortSystem::Ordering;
        }
    }

    if (pSettings->section_exist("inventory_sort:ordering"))
    {
        _weightDesc = pSettings->read_if_exists<bool>("inventory_sort:ordering", "weight_desc", true);
        _conditionDesc = pSettings->read_if_exists<bool>("inventory_sort:ordering", "condition_desc", true);
        _costDesc = pSettings->read_if_exists<bool>("inventory_sort:ordering", "cost_desc", true);
        _noveltyDesc = pSettings->read_if_exists<bool>("inventory_sort:ordering", "novelty_desc", true);
    }
}

void CInventorySorter::InitializeDefaultOrderModes()
{
    SInventoryOrderModeInfo infoGeneral;
    infoGeneral._id = "general";
    infoGeneral._name = "st_inv_sort_order_general";
    infoGeneral._hint = "st_inv_sort_order_general_hint";
    infoGeneral._hasText = true;
    _orderModes[EInventoryOrderMode::General] = infoGeneral;
    _idToOrderMode[infoGeneral._id] = EInventoryOrderMode::General;

    SInventoryOrderModeInfo infoByType;
    infoByType._id = "by_type";
    infoByType._name = "st_inv_sort_order_by_type";
    infoByType._hint = "st_inv_sort_order_by_type_hint";
    infoByType._hasText = true;
    _orderModes[EInventoryOrderMode::ByType] = infoByType;
    _idToOrderMode[infoByType._id] = EInventoryOrderMode::ByType;

    SInventoryOrderModeInfo infoByWeight;
    infoByWeight._id = "by_weight";
    infoByWeight._name = "st_inv_sort_order_by_weight";
    infoByWeight._hint = "st_inv_sort_order_by_weight_hint";
    infoByWeight._hasText = true;
    _orderModes[EInventoryOrderMode::ByWeight] = infoByWeight;
    _idToOrderMode[infoByWeight._id] = EInventoryOrderMode::ByWeight;

    SInventoryOrderModeInfo infoByCondition;
    infoByCondition._id = "by_condition";
    infoByCondition._name = "st_inv_sort_order_by_condition";
    infoByCondition._hint = "st_inv_sort_order_by_condition_hint";
    infoByCondition._hasText = true;
    _orderModes[EInventoryOrderMode::ByCondition] = infoByCondition;
    _idToOrderMode[infoByCondition._id] = EInventoryOrderMode::ByCondition;

    SInventoryOrderModeInfo infoByCost;
    infoByCost._id = "by_cost";
    infoByCost._name = "st_inv_sort_order_by_cost";
    infoByCost._hint = "st_inv_sort_order_by_cost_hint";
    infoByCost._hasText = true;
    _orderModes[EInventoryOrderMode::ByCost] = infoByCost;
    _idToOrderMode[infoByCost._id] = EInventoryOrderMode::ByCost;

    SInventoryOrderModeInfo infoByImportance;
    infoByImportance._id = "by_importance";
    infoByImportance._name = "st_inv_sort_order_by_importance";
    infoByImportance._hint = "st_inv_sort_order_by_importance_hint";
    infoByImportance._hasText = true;
    _orderModes[EInventoryOrderMode::ByImportance] = infoByImportance;
    _idToOrderMode[infoByImportance._id] = EInventoryOrderMode::ByImportance;

    SInventoryOrderModeInfo infoByNovelty;
    infoByNovelty._id = "by_novelty";
    infoByNovelty._name = "st_inv_sort_order_by_novelty";
    infoByNovelty._hint = "st_inv_sort_order_by_novelty_hint";
    infoByNovelty._hasText = true;
    _orderModes[EInventoryOrderMode::ByNovelty] = infoByNovelty;
    _idToOrderMode[infoByNovelty._id] = EInventoryOrderMode::ByNovelty;

    for (auto& [mode, info] : _orderModes)
    {
        LoadOrderModeFromLtx(info._id, mode);
    }
}

void CInventorySorter::LoadOrderModeFromLtx(const shared_str& orderModeId, EInventoryOrderMode mode)
{
    if (!pSettings->section_exist("inventory_sort_order"))
    {
        return;
    }

    string256 path;
    xr_sprintf(path, "inventory_sort_order:%s", orderModeId.c_str());

    if (!pSettings->line_exist("inventory_sort_order", orderModeId.c_str()))
    {
        return;
    }

    auto it = _orderModes.find(mode);
    if (it == _orderModes.end())
    {
        return;
    }

    SInventoryOrderModeInfo& info = it->second;
    if (pSettings->section_exist(path))
    {
        info._name = pSettings->read_if_exists<str_c>(path, "name", info._name.c_str());
        info._hint = pSettings->read_if_exists<str_c>(path, "hint", info._hint.c_str());
        info._hasText = pSettings->read_if_exists<bool>(path, "show_text", info._hasText);
    }
}

void CInventorySorter::InitializeDefaultCategories()
{
    SInventorySortCategoryInfo infoAll;
    infoAll._id = "all";
    infoAll._name = "st_inv_sort_all";
    infoAll._hint = "st_inv_sort_all_hint";
    infoAll._hasIcon = false;
    infoAll._hasText = true;
    _categories[EInventorySortCategory::All] = infoAll;
    _idToCategory[infoAll._id] = EInventorySortCategory::All;

    SInventorySortCategoryInfo infoWeapons;
    infoWeapons._id = "weapons";
    infoWeapons._name = "st_inv_sort_weapons";
    infoWeapons._hint = "st_inv_sort_weapons_hint";
    infoWeapons._hasIcon = false;
    infoWeapons._hasText = true;
    _categories[EInventorySortCategory::Weapons] = infoWeapons;
    _idToCategory[infoWeapons._id] = EInventorySortCategory::Weapons;

    SInventorySortCategoryInfo infoAmmo;
    infoAmmo._id = "ammo";
    infoAmmo._name = "st_inv_sort_ammo";
    infoAmmo._hint = "st_inv_sort_ammo_hint";
    infoAmmo._hasIcon = false;
    infoAmmo._hasText = true;
    _categories[EInventorySortCategory::Ammo] = infoAmmo;
    _idToCategory[infoAmmo._id] = EInventorySortCategory::Ammo;

    SInventorySortCategoryInfo infoArmor;
    infoArmor._id = "armor";
    infoArmor._name = "st_inv_sort_armor";
    infoArmor._hint = "st_inv_sort_armor_hint";
    infoArmor._hasIcon = false;
    infoArmor._hasText = true;
    _categories[EInventorySortCategory::Armor] = infoArmor;
    _idToCategory[infoArmor._id] = EInventorySortCategory::Armor;

    SInventorySortCategoryInfo infoDevices;
    infoDevices._id = "devices";
    infoDevices._name = "st_inv_sort_devices";
    infoDevices._hint = "st_inv_sort_devices_hint";
    infoDevices._hasIcon = false;
    infoDevices._hasText = true;
    _categories[EInventorySortCategory::Devices] = infoDevices;
    _idToCategory[infoDevices._id] = EInventorySortCategory::Devices;

    SInventorySortCategoryInfo infoConsumables;
    infoConsumables._id = "consumables";
    infoConsumables._name = "st_inv_sort_consumables";
    infoConsumables._hint = "st_inv_sort_consumables_hint";
    infoConsumables._hasIcon = false;
    infoConsumables._hasText = true;
    _categories[EInventorySortCategory::Consumables] = infoConsumables;
    _idToCategory[infoConsumables._id] = EInventorySortCategory::Consumables;

    SInventorySortCategoryInfo infoArtefacts;
    infoArtefacts._id = "artefacts";
    infoArtefacts._name = "st_inv_sort_artefacts";
    infoArtefacts._hint = "st_inv_sort_artefacts_hint";
    infoArtefacts._hasIcon = false;
    infoArtefacts._hasText = true;
    _categories[EInventorySortCategory::Artefacts] = infoArtefacts;
    _idToCategory[infoArtefacts._id] = EInventorySortCategory::Artefacts;

    SInventorySortCategoryInfo infoAttachments;
    infoAttachments._id = "attachments";
    infoAttachments._name = "st_inv_sort_attachments";
    infoAttachments._hint = "st_inv_sort_attachments_hint";
    infoAttachments._hasIcon = false;
    infoAttachments._hasText = true;
    _categories[EInventorySortCategory::Attachments] = infoAttachments;
    _idToCategory[infoAttachments._id] = EInventorySortCategory::Attachments;

    for (auto& [category, info] : _categories)
    {
        LoadCategoryFromXml(info._id, category);
    }
}

void CInventorySorter::LoadCategoryFromXml(const shared_str& categoryId, EInventorySortCategory category)
{
    if (!pSettings->section_exist("inventory_sort_categories"))
    {
        return;
    }

    string256 path;
    xr_sprintf(path, "inventory_sort_categories:%s", categoryId.c_str());

    if (!pSettings->line_exist("inventory_sort_categories", categoryId.c_str()))
    {
        return;
    }

    auto it = _categories.find(category);
    if (it == _categories.end())
    {
        return;
    }

    SInventorySortCategoryInfo& info = it->second;
    if (!pSettings->section_exist(path))
    {
        return;
    }

    info._name = pSettings->read_if_exists<str_c>(path, "name", info._name.c_str());
    info._hint = pSettings->read_if_exists<str_c>(path, "hint", info._hint.c_str());

    shared_str iconTexture = pSettings->read_if_exists<str_c>(path, "icon", nullptr);
    if (iconTexture && iconTexture.size() > 0)
    {
        info._iconTexture = iconTexture;
        info._hasIcon = true;
    }

    info._hasText = pSettings->read_if_exists<bool>(path, "show_text", info._hasText);
}

void CInventorySorter::LoadCustomCategories()
{
    if (!pSettings->section_exist("inventory_sort_custom"))
    {
        return;
    }

    u32 customCount = pSettings->line_count("inventory_sort_custom");
    for (u32 i = 0; i < customCount; ++i)
    {
        str_c lineName = nullptr;
        str_c lineValue = nullptr;
        if (!pSettings->r_line("inventory_sort_custom", i, lineName, lineValue))
        {
            continue;
        }
        if (!lineName || !xr_strlen(lineName))
        {
            continue;
        }

        string256 path;
        xr_sprintf(path, "inventory_sort_custom:%s", lineName);

        shared_str name = pSettings->r_string(path, "name");
        shared_str hint = pSettings->read_if_exists<str_c>(path, "hint", "");
        
        AddCustomCategory(lineName, name, hint);

        u32 itemCount = pSettings->line_count(path);
        for (u32 j = 0; j < itemCount; ++j)
        {
            str_c itemName = nullptr;
            str_c itemValue = nullptr;
            if (!pSettings->r_line(path, j, itemName, itemValue))
            {
                continue;
            }
            if (!itemName || !xr_strlen(itemName))
            {
                continue;
            }
            str_c itemLine = itemName;

            if (xr_strcmp(itemLine, "name") == 0 || xr_strcmp(itemLine, "hint") == 0 || 
                xr_strcmp(itemLine, "icon") == 0 || xr_strcmp(itemLine, "show_text") == 0)
            {
                continue;
            }

            shared_str itemSection = pSettings->r_string(path, itemLine);
            AddItemToCustomCategory(lineName, itemSection);
        }
    }
}

EInventorySortCategory CInventorySorter::GetItemCategory(PIItem item) const
{
    if (!item)
    {
        return EInventorySortCategory::All;
    }

    if (smart_cast<CGrenade*>(item) != nullptr)
    {
        return EInventorySortCategory::Ammo;
    }

    if (IsWeapon(item))
    {
        return EInventorySortCategory::Weapons;
    }

    if (IsAmmo(item))
    {
        return EInventorySortCategory::Ammo;
    }

    if (IsArmor(item))
    {
        return EInventorySortCategory::Armor;
    }

    if (IsDevice(item))
    {
        return EInventorySortCategory::Devices;
    }

    if (IsConsumable(item))
    {
        return EInventorySortCategory::Consumables;
    }

    if (IsArtefact(item))
    {
        return EInventorySortCategory::Artefacts;
    }

    if (IsAttachment(item))
    {
        return EInventorySortCategory::Attachments;
    }

    for (const auto& [categoryId, category] : _customCategoryMap)
    {
        if (MatchesCustomCategory(item, categoryId))
        {
            return category;
        }
    }

    return EInventorySortCategory::All;
}

bool CInventorySorter::ItemMatchesCategory(PIItem item, EInventorySortCategory category) const
{
    if (category == EInventorySortCategory::All)
    {
        return true;
    }

    if (!item)
    {
        return false;
    }

    switch (category)
    {
        case EInventorySortCategory::Weapons:
            if (smart_cast<CGrenade*>(item) != nullptr)
            {
                return false;
            }
            return IsWeapon(item);

        case EInventorySortCategory::Ammo:
            return IsAmmo(item);

        case EInventorySortCategory::Armor:
            if (smart_cast<CGrenade*>(item) != nullptr)
            {
                return false;
            }
            return IsArmor(item);

        case EInventorySortCategory::Devices:
            if (smart_cast<CGrenade*>(item) != nullptr)
            {
                return false;
            }
            return IsDevice(item);

        case EInventorySortCategory::Consumables:
            if (smart_cast<CGrenade*>(item) != nullptr)
            {
                return false;
            }
            return IsConsumable(item);

        case EInventorySortCategory::Artefacts:
            if (smart_cast<CGrenade*>(item) != nullptr)
            {
                return false;
            }
            return IsArtefact(item);

        case EInventorySortCategory::Attachments:
            if (smart_cast<CGrenade*>(item) != nullptr)
            {
                return false;
            }
            return IsAttachment(item);

        default:
        {
            if (category >= EInventorySortCategory::CustomStart)
            {
                auto it = _categories.find(category);
                if (it != _categories.end() && it->second._isCustom)
                {
                    return MatchesCustomCategory(item, it->second._id);
                }
            }
            return false;
        }
    }
}

bool CInventorySorter::IsWeapon(PIItem item) const
{
    return smart_cast<CWeapon*>(item) != nullptr;
}

bool CInventorySorter::IsAmmo(PIItem item) const
{
    if (!item)
    {
        return false;
    }
    
    if (smart_cast<CGrenade*>(item) != nullptr)
    {
        return true;
    }
    
    if (smart_cast<CWeaponAmmo*>(item) != nullptr)
    {
        return true;
    }
    
    return false;
}

bool CInventorySorter::IsArmor(PIItem item) const
{
    return smart_cast<CCustomOutfit*>(item) != nullptr || 
           smart_cast<CHelmet*>(item) != nullptr;
}

bool CInventorySorter::IsDevice(PIItem item) const
{
    return smart_cast<CCustomDevice*>(item) != nullptr ||
           smart_cast<CTorch*>(item) != nullptr ||
           smart_cast<CPda*>(item) != nullptr ||
           smart_cast<CCustomDetector*>(item) != nullptr;
}

bool CInventorySorter::IsConsumable(PIItem item) const
{
    return smart_cast<CEatableItem*>(item) != nullptr ||
           smart_cast<CMedkit*>(item) != nullptr;
}

bool CInventorySorter::IsArtefact(PIItem item) const
{
    return smart_cast<CArtefact*>(item) != nullptr;
}

bool CInventorySorter::IsAttachment(PIItem item) const
{
    return smart_cast<CScope*>(item) != nullptr ||
           smart_cast<CSilencer*>(item) != nullptr ||
           smart_cast<CGrenadeLauncher*>(item) != nullptr;
}

bool CInventorySorter::MatchesCustomCategory(PIItem item, const shared_str& categoryId) const
{
    auto idIt = _idToCategory.find(categoryId);
    if (idIt == _idToCategory.end())
    {
        return false;
    }

    auto categoryIt = _categories.find(idIt->second);
    if (categoryIt == _categories.end())
    {
        return false;
    }

    const SInventorySortCategoryInfo& info = categoryIt->second;

    if (!info._isCustom)
    {
        return false;
    }

    if (item)
    {
        shared_str itemSection = item->object().cNameSect();
        if (info._itemSections.find(itemSection) != info._itemSections.end())
        {
            return true;
        }

        CLASS_ID itemClsid = item->object().CLS_ID;
        if (info._itemClsids.find(itemClsid) != info._itemClsids.end())
        {
            return true;
        }
    }

    return false;
}

EInventorySortCategory CInventorySorter::GetCategoryByIndex(u32 index) const
{
    u32 currentIndex = 0;
    for (const auto& [category, info] : _categories)
    {
        if (currentIndex == index)
        {
            return category;
        }
        ++currentIndex;
    }
    return EInventorySortCategory::All;
}

const SInventorySortCategoryInfo* CInventorySorter::GetCategoryInfo(EInventorySortCategory category) const
{
    auto it = _categories.find(category);
    if (it != _categories.end())
    {
        return &it->second;
    }
    return nullptr;
}

EInventorySortCategory CInventorySorter::GetCategoryById(const shared_str& id) const
{
    auto it = _idToCategory.find(id);
    if (it != _idToCategory.end())
    {
        return it->second;
    }

    return EInventorySortCategory::All;
}

const SInventorySortCategoryInfo* CInventorySorter::GetCategoryInfoById(const shared_str& id) const
{
    EInventorySortCategory category = GetCategoryById(id);
    return GetCategoryInfo(category);
}

void CInventorySorter::AddCustomCategory(const shared_str& id, const shared_str& name, const shared_str& hint)
{
    if (_customCategoryMap.find(id) != _customCategoryMap.end())
    {
        return;
    }

    // EInventorySortCategory is u8; keep custom ids inside the remaining range.
    constexpr u8 maxCustomCategories = static_cast<u8>(255 - static_cast<u8>(EInventorySortCategory::CustomStart));
    if (_customCategoryCounter >= maxCustomCategories)
    {
        Msg("! CInventorySorter: custom category limit reached, skip [%s]", id.c_str());
        return;
    }

    EInventorySortCategory newCategory = (EInventorySortCategory)((u8)EInventorySortCategory::CustomStart + _customCategoryCounter);
    ++_customCategoryCounter;

    SInventorySortCategoryInfo info;
    info._id = id;
    info._name = name;
    info._hint = hint;
    info._hasIcon = false;
    info._hasText = true;
    info._isCustom = true;

    _categories[newCategory] = info;
    _idToCategory[id] = newCategory;
    _customCategoryMap[id] = newCategory;
}

void CInventorySorter::AddItemToCustomCategory(const shared_str& categoryId, const shared_str& itemSection)
{
    auto it = _customCategoryMap.find(categoryId);
    if (it == _customCategoryMap.end())
    {
        return;
    }

    EInventorySortCategory category = it->second;
    auto categoryIt = _categories.find(category);
    if (categoryIt == _categories.end())
    {
        return;
    }

    categoryIt->second._itemSections.insert(itemSection);
}

void CInventorySorter::AddClsidToCustomCategory(const shared_str& categoryId, CLASS_ID clsid)
{
    auto it = _customCategoryMap.find(categoryId);
    if (it == _customCategoryMap.end())
    {
        return;
    }

    EInventorySortCategory category = it->second;
    auto categoryIt = _categories.find(category);
    if (categoryIt == _categories.end())
    {
        return;
    }

    categoryIt->second._itemClsids.insert(clsid);
}

void CInventorySorter::SortItems(TIItemContainer& items, EInventorySortCategory category) const
{
    if (category == EInventorySortCategory::All)
    {
        return;
    }

    TIItemContainer filtered;
    for (PIItem item : items)
    {
        if (ItemMatchesCategory(item, category))
        {
            filtered.push_back(item);
        }
    }

    items = filtered;
}

void CInventorySorter::SortItemsById(TIItemContainer& items, const shared_str& categoryId) const
{
    EInventorySortCategory category = GetCategoryById(categoryId);
    SortItems(items, category);
}

EInventoryOrderMode CInventorySorter::GetOrderModeById(const shared_str& id) const
{
    auto it = _idToOrderMode.find(id);
    if (it != _idToOrderMode.end())
    {
        return it->second;
    }

    return EInventoryOrderMode::General;
}

const SInventoryOrderModeInfo* CInventorySorter::GetOrderModeInfo(EInventoryOrderMode mode) const
{
    auto it = _orderModes.find(mode);
    if (it != _orderModes.end())
    {
        return &it->second;
    }

    return nullptr;
}

u8 CInventorySorter::GetCategoryPriority(EInventorySortCategory category) const
{
    switch (category)
    {
    case EInventorySortCategory::Weapons:
        return 0;
    case EInventorySortCategory::Ammo:
        return 1;
    case EInventorySortCategory::Armor:
        return 2;
    case EInventorySortCategory::Devices:
        return 3;
    case EInventorySortCategory::Consumables:
        return 4;
    case EInventorySortCategory::Artefacts:
        return 5;
    case EInventorySortCategory::Attachments:
        return 6;
    case EInventorySortCategory::All:
        return 250;
    default:
        if (category >= EInventorySortCategory::CustomStart)
        {
            return static_cast<u8>(category);
        }
        return 251;
    }
}

u8 CInventorySorter::GetCategoryPriority(EInventorySortCategory category, EInventorySortCategory pivot) const
{
    if (pivot == EInventorySortCategory::All)
    {
        return GetCategoryPriority(category);
    }

    if (category == pivot)
    {
        return 0;
    }

    const u8 basePriority = GetCategoryPriority(category);
    const u8 pivotPriority = GetCategoryPriority(pivot);
    if (basePriority < pivotPriority)
    {
        return static_cast<u8>(basePriority + 1);
    }

    return basePriority;
}

void CInventorySorter::BuildTypeCycleList()
{
    _typeCycleCategories.clear();
    _typeCycleCategories.push_back(EInventorySortCategory::Weapons);
    _typeCycleCategories.push_back(EInventorySortCategory::Ammo);
    _typeCycleCategories.push_back(EInventorySortCategory::Armor);
    _typeCycleCategories.push_back(EInventorySortCategory::Devices);
    _typeCycleCategories.push_back(EInventorySortCategory::Consumables);
    _typeCycleCategories.push_back(EInventorySortCategory::Artefacts);
    _typeCycleCategories.push_back(EInventorySortCategory::Attachments);

    for (const auto& [category, info] : _categories)
    {
        if (info._isCustom)
        {
            _typeCycleCategories.push_back(category);
        }
    }
}

u8 CInventorySorter::GetTypeCycleCount() const
{
    // +1 for "all types grouped"
    return static_cast<u8>(_typeCycleCategories.size() + 1);
}

EInventorySortCategory CInventorySorter::GetTypeCycleCategory(u8 cycleIndex) const
{
    if (cycleIndex == 0 || _typeCycleCategories.empty())
    {
        return EInventorySortCategory::All;
    }

    const u8 typeIndex = static_cast<u8>(cycleIndex - 1);
    if (typeIndex >= _typeCycleCategories.size())
    {
        return EInventorySortCategory::All;
    }

    return _typeCycleCategories[typeIndex];
}

const SInventorySortCategoryInfo* CInventorySorter::GetTypeCycleInfo(u8 cycleIndex) const
{
    return GetCategoryInfo(GetTypeCycleCategory(cycleIndex));
}

bool CInventorySorter::CompareByType(PIItem item1, PIItem item2, EInventorySortCategory pivot) const
{
    if (!item1 || !item2)
    {
        return item1 != nullptr;
    }

    const u8 priority1 = GetCategoryPriority(GetItemCategory(item1), pivot);
    const u8 priority2 = GetCategoryPriority(GetItemCategory(item2), pivot);
    if (priority1 != priority2)
    {
        return priority1 < priority2;
    }

    return InventoryUtilities::GreaterRoomInRuck(item1, item2);
}

bool CInventorySorter::CompareByWeight(PIItem item1, PIItem item2, bool weightDesc) const
{
    if (!item1 || !item2)
    {
        return item1 != nullptr;
    }

    const float weight1 = item1->Weight();
    const float weight2 = item2->Weight();
    if (!fis_zero(weight1 - weight2, EPS))
    {
        return weightDesc ? weight1 > weight2 : weight1 < weight2;
    }

    if (item1->object().cNameSect() == item2->object().cNameSect())
    {
        return item1->object().ID() > item2->object().ID();
    }

    return item1->object().cNameSect() > item2->object().cNameSect();
}

bool CInventorySorter::CompareByCondition(PIItem item1, PIItem item2, bool conditionDesc) const
{
    if (!item1 || !item2)
    {
        return item1 != nullptr;
    }

    const bool hasCondition1 = item1->IsUsingCondition();
    const bool hasCondition2 = item2->IsUsingCondition();

    if (!hasCondition1 && !hasCondition2)
    {
        return InventoryUtilities::GreaterRoomInRuck(item1, item2);
    }

    if (!hasCondition1)
    {
        return false;
    }

    if (!hasCondition2)
    {
        return true;
    }

    const float condition1 = item1->GetCondition();
    const float condition2 = item2->GetCondition();
    if (!fis_zero(condition1 - condition2, EPS))
    {
        return conditionDesc ? condition1 > condition2 : condition1 < condition2;
    }

    return InventoryUtilities::GreaterRoomInRuck(item1, item2);
}

bool CInventorySorter::CompareByCost(PIItem item1, PIItem item2, bool costDesc) const
{
    if (!item1 || !item2)
    {
        return item1 != nullptr;
    }

    const u32 cost1 = item1->Cost();
    const u32 cost2 = item2->Cost();
    if (cost1 != cost2)
    {
        return costDesc ? cost1 > cost2 : cost1 < cost2;
    }

    if (item1->object().cNameSect() == item2->object().cNameSect())
    {
        return item1->object().ID() > item2->object().ID();
    }

    return item1->object().cNameSect() > item2->object().cNameSect();
}

bool CInventorySorter::CompareByImportance(PIItem item1, PIItem item2) const
{
    if (!item1 || !item2)
    {
        return item1 != nullptr;
    }

    const bool isQuest1 = item1->IsQuestItem();
    const bool isQuest2 = item2->IsQuestItem();
    if (isQuest1 != isQuest2)
    {
        return isQuest1;
    }

    return InventoryUtilities::GreaterRoomInRuck(item1, item2);
}

bool CInventorySorter::CompareByNovelty(PIItem item1, PIItem item2, bool noveltyDesc) const
{
	// Novelty no longer stores take-time on the item (that broke client_data layout).
	// Keep the mode as a stable ID/section order so the tab still does something useful.
	if (!item1 || !item2)
	{
		return item1 != nullptr;
	}

	if (item1->object().cNameSect() == item2->object().cNameSect())
	{
		const bool byId = item1->object().ID() > item2->object().ID();
		return noveltyDesc ? byId : !byId;
	}

	const bool bySect = item1->object().cNameSect() > item2->object().cNameSect();
	return noveltyDesc ? bySect : !bySect;
}

void CInventorySorter::ApplyBagListOrder(TIItemContainer& items, EInventoryOrderMode mode) const
{
    SInventoryOrderOptions options;
    options.weightDesc = _weightDesc;
    options.conditionDesc = _conditionDesc;
    options.costDesc = _costDesc;
    options.noveltyDesc = _noveltyDesc;
    options.typeCycle = 0;
    ApplyBagListOrder(items, mode, options);
}

void CInventorySorter::ApplyBagListOrder(TIItemContainer& items, EInventoryOrderMode mode, const SInventoryOrderOptions& options) const
{
    switch (mode)
    {
    case EInventoryOrderMode::General:
    {
        std::stable_sort(items.begin(), items.end(), InventoryUtilities::GreaterRoomInRuck);
        break;
    }
    case EInventoryOrderMode::ByType:
    {
        const EInventorySortCategory pivot = GetTypeCycleCategory(options.typeCycle);
        std::stable_sort(items.begin(), items.end(), [this, pivot](PIItem item1, PIItem item2)
        {
            return CompareByType(item1, item2, pivot);
        });
        break;
    }
    case EInventoryOrderMode::ByWeight:
    {
        const bool weightDesc = options.weightDesc;
        std::stable_sort(items.begin(), items.end(), [this, weightDesc](PIItem item1, PIItem item2)
        {
            return CompareByWeight(item1, item2, weightDesc);
        });
        break;
    }
    case EInventoryOrderMode::ByCondition:
    {
        const bool conditionDesc = options.conditionDesc;
        std::stable_sort(items.begin(), items.end(), [this, conditionDesc](PIItem item1, PIItem item2)
        {
            return CompareByCondition(item1, item2, conditionDesc);
        });
        break;
    }
    case EInventoryOrderMode::ByCost:
    {
        const bool costDesc = options.costDesc;
        std::stable_sort(items.begin(), items.end(), [this, costDesc](PIItem item1, PIItem item2)
        {
            return CompareByCost(item1, item2, costDesc);
        });
        break;
    }
    case EInventoryOrderMode::ByImportance:
    {
        std::stable_sort(items.begin(), items.end(), [this](PIItem item1, PIItem item2)
        {
            return CompareByImportance(item1, item2);
        });
        break;
    }
    case EInventoryOrderMode::ByNovelty:
    {
        const bool noveltyDesc = options.noveltyDesc;
        std::stable_sort(items.begin(), items.end(), [this, noveltyDesc](PIItem item1, PIItem item2)
        {
            return CompareByNovelty(item1, item2, noveltyDesc);
        });
        break;
    }
    default:
    {
        std::stable_sort(items.begin(), items.end(), InventoryUtilities::GreaterRoomInRuck);
        break;
    }
    }
}

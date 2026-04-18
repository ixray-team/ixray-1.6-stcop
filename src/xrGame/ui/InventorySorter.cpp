#include "StdAfx.h"
#include "InventorySorter.h"
#include "../inventory_item.h"
#include "../Weapon.h"
#include "../WeaponAmmo.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../Artefact.h"
#include "../CustomDetector.h"
#include "../Torch.h"
#include "../Pda.h"
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
    InitializeDefaultCategories();
    LoadCustomCategories();
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
    string256 namePath;
    string256 hintPath;
    string256 iconPath;
    string256 showTextPath;
    
    xr_sprintf(path, "inventory_sort_categories:%s", categoryId.c_str());
    xr_sprintf(namePath, "%s:name", path);
    xr_sprintf(hintPath, "%s:hint", path);
    xr_sprintf(iconPath, "%s:icon", path);
    xr_sprintf(showTextPath, "%s:show_text", path);

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

    info._name = READ_IF_EXISTS(pSettings, r_string, "inventory_sort_categories", namePath, info._name.c_str());
    info._hint = READ_IF_EXISTS(pSettings, r_string, "inventory_sort_categories", hintPath, info._hint.c_str());
    
    shared_str iconTexture = READ_IF_EXISTS(pSettings, r_string, "inventory_sort_categories", iconPath, nullptr);
    if (iconTexture && iconTexture.size() > 0)
    {
        info._iconTexture = iconTexture;
        info._hasIcon = true;
    }

    bool showText = READ_IF_EXISTS(pSettings, r_bool, "inventory_sort_categories", showTextPath, true);
    info._hasText = showText;
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
        const char* lineName = nullptr;
        const char* lineValue = nullptr;
        if (!pSettings->r_line("inventory_sort_custom", i, &lineName, &lineValue))
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
        shared_str hint = READ_IF_EXISTS(pSettings, r_string, path, "hint", "");
        
        AddCustomCategory(lineName, name, hint);

        u32 itemCount = pSettings->line_count(path);
        for (u32 j = 0; j < itemCount; ++j)
        {
            const char* itemName = nullptr;
            const char* itemValue = nullptr;
            if (!pSettings->r_line(path, j, &itemName, &itemValue))
            {
                continue;
            }
            if (!itemName || !xr_strlen(itemName))
            {
                continue;
            }
            const char* itemLine = itemName;

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

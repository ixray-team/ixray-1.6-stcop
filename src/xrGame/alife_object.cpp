////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_object.cpp
//	Created 	: 27.10.2005
//  Modified 	: 27.10.2005
//	Author		: Dmitriy Iassenev
//	Description : ALife object class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "../xrServerEntities/xrServer_Objects_ALife.h"
#include "alife_simulator.h"
#include "../xrServerEntities/xrServer_Objects_ALife_Items.h"

void CSE_ALifeObject::spawn_supplies()
{
    spawn_supplies(*m_ini_string);
}


float CSE_ALifeObject::parseFloatParameterValue(LPCSTR spawnArgs, LPCSTR parameterName, float defaultValue)
{
    float value = defaultValue;

    if (spawnArgs == nullptr || !xr_strlen(spawnArgs) || !xr_strlen(parameterName)) {
        return value;
    }

    if (nullptr != strstr(spawnArgs, parameterName)) {
        value = (float)atof(strstr(spawnArgs, parameterName) + xr_strlen(parameterName));
    }

    return value;
}

int CSE_ALifeObject::parseIntParameterValue(LPCSTR spawnArgs, LPCSTR parameterName, int defaultValue)
{
    int value = defaultValue;

    if (spawnArgs == nullptr || !xr_strlen(spawnArgs) || !xr_strlen(parameterName)) {
        return value;
    }

    if (nullptr != strstr(spawnArgs, parameterName)) {
        value = (int)atoi(strstr(spawnArgs, parameterName) + xr_strlen(parameterName));
    }

    return value;
}

bool CSE_ALifeObject::parseBoolParameterValue(LPCSTR spawnArgs, LPCSTR parameterName)
{
    if (spawnArgs == nullptr || !xr_strlen(spawnArgs) || !xr_strlen(parameterName)) {
        return false;
    }

    return (nullptr != strstr(spawnArgs, parameterName));
}

u32 CSE_ALifeObject::getCountValueToSpawn(LPCSTR spawnArgs)
{
    int spawn_count = 1;

    if (spawnArgs == nullptr || !xr_strlen(spawnArgs)) {
        return spawn_count;
    }

    if (spawnArgs && xr_strlen(spawnArgs))
    {
        if (_GetItemCount(spawnArgs) > 0) {
            string64 tmp;
            spawn_count = atoi(_GetItem(spawnArgs, 0, tmp));
            if (!spawn_count)
            {
                spawn_count = 1;
            }
        }
    }

    return spawn_count;
}

CSE_Abstract* CSE_ALifeObject::setAddonFlagsIsWeapon(CSE_Abstract* E, LPCSTR spawnArgs)
{
    if (E == nullptr) {
        return E;
    }

    CSE_ALifeItemWeapon* ALIWeapon = smart_cast<CSE_ALifeItemWeapon*>(E);

    if (ALIWeapon == nullptr)
    {
        return E;
    }

    bool bScope = parseBoolParameterValue(spawnArgs, "scope");
    int scope_index = parseIntParameterValue(spawnArgs, "scope=", 0);
    float fScopeProb = parseFloatParameterValue(spawnArgs, "scope_prob=", 0.0f);
    if ( bScope && (fScopeProb > 0.0f) && (fScopeProb < 1.0f) && (randF(1.f) >= fScopeProb) ) {
        bScope = false;
    }

    bool bSilencer = parseBoolParameterValue(spawnArgs, "silencer");
    float fSilencerProb = parseFloatParameterValue(spawnArgs, "silencer_prob=", 0.0f);
    if (bSilencer && (fSilencerProb > 0.0f) && (fSilencerProb < 1.0f) && (randF(1.f) >= fSilencerProb)) {
        bSilencer = false;
    }

    bool bLauncher = parseBoolParameterValue(spawnArgs, "launcher");
    float fLauncherProb = parseFloatParameterValue(spawnArgs, "launcher_prob=", 0.0f);
    if (bLauncher && (fLauncherProb > 0.0f) && (fLauncherProb < 1.0f) && (randF(1.f) >= fLauncherProb)) {
        bLauncher = false;
    }

    if (ALIWeapon->m_scope_status == ALife::eAddonAttachable)
    {
        ALIWeapon->m_addon_flags.set(CSE_ALifeItemWeapon::eWeaponAddonScope, bScope);
        ALIWeapon->cur_scope = scope_index;
    }

    if (ALIWeapon->m_silencer_status == ALife::eAddonAttachable)
    {
        ALIWeapon->m_addon_flags.set(CSE_ALifeItemWeapon::eWeaponAddonSilencer, bSilencer);
    }

    if (ALIWeapon->m_grenade_launcher_status == ALife::eAddonAttachable)
    {
        ALIWeapon->m_addon_flags.set(CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher, bLauncher);
    }

    return E;
}

void CSE_ALifeObject::spawnAmmoForWeapon(LPCSTR wpnSection, CSE_Abstract* E, int i_ammo_type, u32 countAmmoBoxesToSpawn)
{
    if (wpnSection == nullptr || !xr_strlen(wpnSection)) {
        return;
    }

    if (smart_cast<CSE_ALifeItemWeapon*>(E) != nullptr)
    {
        if (pSettings->line_exist(wpnSection, "ammo_class"))
        {
            LPCSTR ammoSec = "";
            LPCSTR ammo_class = pSettings->r_string(wpnSection, "ammo_class");

            for (int i = 0, n = _GetItemCount(ammo_class); i < n; ++i)
            {
                string128 tmp;
                ammoSec = _GetItem(ammo_class, i, tmp);

                if (i == i_ammo_type) 
                {
                    break;
                }
            }

            if (xr_strlen(ammoSec) && pSettings->section_exist(ammoSec))
            {
                for (u32 i = 1; i <= countAmmoBoxesToSpawn; ++i)
                {
                    alife().spawn_item(ammoSec, o_Position, m_tNodeID, m_tGraphID, ID);
                }
            }
        }
    }
}

void CSE_ALifeObject::setItemCondition(CSE_Abstract* E, float condition)
{
    if (E == nullptr)
    {
        return;
    }

    if (CSE_ALifeInventoryItem* IItem = smart_cast<CSE_ALifeInventoryItem*>(E)) {
        IItem->m_fCondition = condition;
    }
}


void CSE_ALifeObject::spawn_supplies(LPCSTR ini_string)
{
    if (!ini_string)
        return;

    if (!xr_strlen(ini_string))
        return;

#pragma warning(push)
#pragma warning(disable:4238)
	IReader temp(
		(void*) (ini_string),
		xr_strlen(ini_string)
	);

	CInifile ini(&temp,
		FS.get_path("$game_config$")->m_Path
	);
#pragma warning(pop)

    const static bool isEnableLoadoutsForSpawnSupplies = EngineExternal()[EEngineExternalGame::EnableLoadoutsForSpawnSupplies];
    const static bool isEnableCocStyleForLoadoutsSpawnSupplies = EngineExternal()[EEngineExternalGame::EnableCocStyleForLoadoutsSpawnSupplies];

    if (isEnableLoadoutsForSpawnSupplies) {
        if (isEnableCocStyleForLoadoutsSpawnSupplies) 
        {
            processingSpawnOnceRandomItemInRandomLoadout(ini);
        }
        else 
        {
            processingSpawnOnceFullRandomLoadout(ini);
        }
    }

    processingVanillaSpawn(ini);
}

xr_vector <CInifile::Sect*> CSE_ALifeObject::parseLoadouts(CInifile& ini)
{
    xr_vector <CInifile::Sect*> m_loadouts;
    LPCSTR loadoutSpawnSectionName = "";
    auto sections = ini.sections();
    m_loadouts.clear();

    for (size_t i = 0; i < sections.size(); i++) {
        CInifile::Sect* sect = sections[i];
        if (!sect) {
            continue;
        }

        loadoutSpawnSectionName = sect->Name.c_str();
        if (nullptr != strstr(loadoutSpawnSectionName, "spawn_loadout")) {
            m_loadouts.push_back(&ini.r_section(loadoutSpawnSectionName));
        }
    }

    return m_loadouts;
}

// Спавнит один полный лодаут среди случайных в рамках инклуда в профиле нпц в разделе supplies
void CSE_ALifeObject::processingSpawnOnceFullRandomLoadout(CInifile& ini)
{
    xr_vector <CInifile::Sect*> m_loadouts = parseLoadouts(ini);
    LPCSTR itemSection = "";
    LPCSTR spawnArgs = "";

    if (m_loadouts.empty()) {
        return;
    }

    CInifile::Sect* randomLoadout = m_loadouts[::Random.randI(0, m_loadouts.size())];

    for (size_t i = 0; i < randomLoadout->Data.size(); i++) {
        itemSection = randomLoadout->Data[i].first.c_str();
        spawnArgs = randomLoadout->Data[i].second.c_str();

        if (!pSettings->section_exist(itemSection))
        {
            Msg("! ERROR missing loadout spawn section:[%s] for npc:[%s]", itemSection, name());
            continue;
        }

        float spawnItemChance = parseFloatParameterValue(spawnArgs, "prob=", 1.0f);
        if (spawnItemChance > 0.0f && spawnItemChance < 1.0f) {
            if (randF(1.f) >= spawnItemChance)
            {
                continue;
            }
        }

        CSE_Abstract* CSEItem = setAddonFlagsIsWeapon(
            alife().spawn_item(itemSection, o_Position, m_tNodeID, m_tGraphID, ID),
            spawnArgs
        );

        spawnAmmoForWeapon(
            itemSection,
            CSEItem,
            parseIntParameterValue(spawnArgs, "ammo_type=", 0),
            getCountValueToSpawn(spawnArgs)
        );

        setItemCondition(
            CSEItem,
            parseFloatParameterValue(spawnArgs, "cond=", 1.0f)
        );
    }
}

// Спавнит один случайный предмет среди случайно выбранного лодаута в рамках инклуда в профиле нпц в разделе supplies
void CSE_ALifeObject::processingSpawnOnceRandomItemInRandomLoadout(CInifile& ini)
{
    xr_vector <CInifile::Sect*> m_loadouts = parseLoadouts(ini);
    if (m_loadouts.empty()) {
        return;
    }

    CInifile::Sect* randomLoadout = m_loadouts[::Random.randI(0, m_loadouts.size())];
    size_t randomLoadoutItemIndex = ::Random.randI(0, randomLoadout->Data.size());
    LPCSTR itemSection = randomLoadout->Data[randomLoadoutItemIndex].first.c_str();
    LPCSTR spawnArgs = randomLoadout->Data[randomLoadoutItemIndex].second.c_str();

    if (!pSettings->section_exist(itemSection))
    {
        Msg("! ERROR missing loadout spawn section:[%s] for npc:[%s]", itemSection, name());
        return;
    }

    CSE_Abstract* CSEItem = setAddonFlagsIsWeapon(
        alife().spawn_item(itemSection, o_Position, m_tNodeID, m_tGraphID, ID),
        spawnArgs
    );

    spawnAmmoForWeapon(
        itemSection,
        CSEItem,
        parseIntParameterValue(spawnArgs, "ammo_type=", 0),
        getCountValueToSpawn(spawnArgs)
    );

    setItemCondition(
        CSEItem,
        parseFloatParameterValue(spawnArgs, "cond=", 1.0f)
    );
}

// Ванильный спавн
void CSE_ALifeObject::processingVanillaSpawn(CInifile& ini)
{
    LPCSTR itemSection = "";
    LPCSTR spawnArgs = "";

    if (ini.section_exist("spawn"))
    {
        CInifile::Sect spawnChunk = ini.r_section("spawn");

        for (size_t i = 0; i < spawnChunk.Data.size(); i++) {
            itemSection = spawnChunk.Data[i].first.c_str();
            spawnArgs = spawnChunk.Data[i].second.c_str();

            if (!pSettings->section_exist(itemSection))
            {
                Msg("! ERROR missing spawn section:[%s] for npc:[%s]", itemSection, name());
                continue;
            }

            if (pSettings->section_exist(itemSection))
            {
                float f_cond = parseFloatParameterValue(spawnArgs, "cond=", 1.0f);
                int countToSpawn = getCountValueToSpawn(spawnArgs);
                float spawnChance = parseFloatParameterValue(spawnArgs, "prob=", 1.0f);

                for (u32 i = 0; i < countToSpawn; ++i)
                {
                    if (randF(1.f) < spawnChance)
                    {
                        CSE_Abstract* CSEItem = alife().spawn_item(itemSection, o_Position, m_tNodeID, m_tGraphID, ID);
                        setAddonFlagsIsWeapon(CSEItem, spawnArgs);
                        setItemCondition(CSEItem, f_cond);
                    }
                }
            }
        }
    }
}

bool CSE_ALifeObject::keep_saved_data_anyway() const
{
    return (false);
}
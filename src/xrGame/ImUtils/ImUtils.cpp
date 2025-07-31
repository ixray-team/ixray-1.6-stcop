#include "stdafx.h"

#include "ImUtils.h"

clsid_manager* g_pClsidManager;
CImGuiGameSearchManager imgui_search_manager;

void RegisterImGuiInGame()
{
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Subscribe("Time Manager", CImGuiManager::ERenderPriority::eMedium, RenderTimeManagerWindow);
		CImGuiManager::Instance().Subscribe("Spawn Manager", CImGuiManager::ERenderPriority::eMedium, RenderSpawnManagerWindow);
		CImGuiManager::Instance().Subscribe("Weapon Manager", CImGuiManager::ERenderPriority::eMedium, RenderWeaponManagerWindow);
		CImGuiManager::Instance().Subscribe("Search Manager", CImGuiManager::ERenderPriority::eMedium, RenderSearchManagerWindow);
		CImGuiManager::Instance().Subscribe("OMF Editor", CImGuiManager::ERenderPriority::eMedium, RenderToolsOMFEditorWindow);

		InitImGuiCLSIDInGame();
		InitImGuiSearchInGame();
		InitSections();
	}
}

void DestroyImGuiInGame()
{
	DestroySpawnManagerWindow();
}

eSelectedType CImGuiGameSearchManager::convertCLSIDToType(CLASS_ID id) {
	eSelectedType result = eSelectedType::kSelectedType_Count;

	if (class_to_type.find(id) != class_to_type.end())
		result = class_to_type.at(id);

	return result;
}

const char* CImGuiGameSearchManager::convertTypeToString(int type) {
	switch (static_cast<eSelectedType>(type))
	{
	case eSelectedType::kSelectedType_All:
	{
		return "All";
	}
	case eSelectedType::kSelectedType_Monster_All:
	{
		return "Monster - All";
	}
	case eSelectedType::kSelectedType_Weapon_All:
	{
		return "Weapon - All";
	}
	}

	return nullptr;
}

bool CImGuiGameSearchManager::valid(CLASS_ID id) {

	bool result{};

	if (selected_type == eSelectedType::kSelectedType_All)
	{
		result = true;
		return result;
	}

	if (selected_type == eSelectedType::kSelectedType_Monster_All)
	{
		if (g_pClsidManager && g_pClsidManager->is_monster(id))
		{
			result = true;
			return result;
		}
	}

	if (selected_type == eSelectedType::kSelectedType_Weapon_All)
	{
		if (g_pClsidManager && g_pClsidManager->is_weapon(id))
		{
			result = true;
			return result;
		}
	}

	if (class_to_type.find(id) != class_to_type.end())
	{
		result = selected_type == class_to_type.at(id);
	}

	return result;
}

void CImGuiGameSearchManager::count(CLASS_ID id) {
	counts[(eSelectedType::kSelectedType_All)] += 1;

	if (g_pClsidManager == nullptr)
	{
		return;
	}

	if (g_pClsidManager->is_monster(id))
	{
		counts[eSelectedType::kSelectedType_Monster_All] += 1;

		if (class_to_type.find(id) != class_to_type.end())
			counts[class_to_type.at(id)] += 1;

	}
	else if (g_pClsidManager->is_weapon(id))
	{
		counts[eSelectedType::kSelectedType_Weapon_All] += 1;

		if (class_to_type.find(id) != class_to_type.end())
			counts[class_to_type.at(id)] += 1;
	}
	else
	{
		if (class_to_type.find(id) != class_to_type.end())
		{
			counts[class_to_type.at(id)] += 1;
		}
	}
}

void CImGuiGameSearchManager::init()
{
	if (g_pClsidManager == nullptr)
		return;

	type_to_class[eSelectedType::kSelectedType_SmartTerrain] = g_pClsidManager->smart_terrain;
	type_to_class[eSelectedType::kSelectedType_SmartCover] = g_pClsidManager->smart_cover;
	type_to_class[eSelectedType::kSelectedType_LevelChanger] = g_pClsidManager->level_changer;
	type_to_class[eSelectedType::kSelectedType_Artefact] = g_pClsidManager->artefact;
	type_to_class[eSelectedType::kSelectedType_Stalker] = g_pClsidManager->stalker;
	type_to_class[eSelectedType::kSelectedType_Car] = g_pClsidManager->car;

	type_to_class[eSelectedType::kSelectedType_Monster_BloodSucker] = g_pClsidManager->monster_bloodsucker;
	type_to_class[eSelectedType::kSelectedType_Monster_Boar] = g_pClsidManager->monster_boar;
	type_to_class[eSelectedType::kSelectedType_Monster_Dog] = g_pClsidManager->monster_dog;
	type_to_class[eSelectedType::kSelectedType_Monster_Flesh] = g_pClsidManager->monster_flesh;
	type_to_class[eSelectedType::kSelectedType_Monster_PseudoDog] = g_pClsidManager->monster_pseudodog;
	type_to_class[eSelectedType::kSelectedType_Monster_Burer] = g_pClsidManager->monster_burer;
	type_to_class[eSelectedType::kSelectedType_Monster_Cat] = g_pClsidManager->monster_cat;
	type_to_class[eSelectedType::kSelectedType_Monster_Chimera] = g_pClsidManager->monster_chimera;
	type_to_class[eSelectedType::kSelectedType_Monster_Controller] = g_pClsidManager->monster_controller;
	type_to_class[eSelectedType::kSelectedType_Monster_Izlom] = g_pClsidManager->monster_izlom;
	type_to_class[eSelectedType::kSelectedType_Monster_Poltergeist] = g_pClsidManager->monster_poltergeist;
	type_to_class[eSelectedType::kSelectedType_Monster_PseudoGigant] = g_pClsidManager->monster_pseudogigant;
	type_to_class[eSelectedType::kSelectedType_Monster_Zombie] = g_pClsidManager->monster_zombie;
	type_to_class[eSelectedType::kSelectedType_Monster_Snork] = g_pClsidManager->monster_snork;
	type_to_class[eSelectedType::kSelectedType_Monster_Tushkano] = g_pClsidManager->monster_tushkano;
	type_to_class[eSelectedType::kSelectedType_Monster_PsyDog] = g_pClsidManager->monster_psydog;
	type_to_class[eSelectedType::kSelectedType_Monster_PsyDogPhantom] = g_pClsidManager->monster_psydogphantom;

	type_to_class[eSelectedType::kSelectedType_Weapon_Binocular] = g_pClsidManager->weapon_binocular;
	type_to_class[eSelectedType::kSelectedType_Weapon_Knife] = g_pClsidManager->weapon_knife;
	type_to_class[eSelectedType::kSelectedType_Weapon_BM16] = g_pClsidManager->weapon_bm16;
	type_to_class[eSelectedType::kSelectedType_Weapon_Groza] = g_pClsidManager->weapon_groza;
	type_to_class[eSelectedType::kSelectedType_Weapon_SVD] = g_pClsidManager->weapon_svd;
	type_to_class[eSelectedType::kSelectedType_Weapon_AK74] = g_pClsidManager->weapon_ak74;
	type_to_class[eSelectedType::kSelectedType_Weapon_LR300] = g_pClsidManager->weapon_lr300;
	type_to_class[eSelectedType::kSelectedType_Weapon_HPSA] = g_pClsidManager->weapon_hpsa;
	type_to_class[eSelectedType::kSelectedType_Weapon_PM] = g_pClsidManager->weapon_pm;
	type_to_class[eSelectedType::kSelectedType_Weapon_RG6] = g_pClsidManager->weapon_rg6;
	type_to_class[eSelectedType::kSelectedType_Weapon_RPG7] = g_pClsidManager->weapon_rpg7;
	type_to_class[eSelectedType::kSelectedType_Weapon_Shotgun] = g_pClsidManager->weapon_shotgun;
	type_to_class[eSelectedType::kSelectedType_Weapon_AutoShotgun] = g_pClsidManager->weapon_autoshotgun;
	type_to_class[eSelectedType::kSelectedType_Weapon_SVU] = g_pClsidManager->weapon_svu;
	type_to_class[eSelectedType::kSelectedType_Weapon_USP45] = g_pClsidManager->weapon_usp45;
	type_to_class[eSelectedType::kSelectedType_Weapon_VAL] = g_pClsidManager->weapon_val;
	type_to_class[eSelectedType::kSelectedType_Weapon_VINTOREZ] = g_pClsidManager->weapon_vintorez;
	type_to_class[eSelectedType::kSelectedType_Weapon_WALTHER] = g_pClsidManager->weapon_walther;
	type_to_class[eSelectedType::kSelectedType_Weapon_Magazine] = g_pClsidManager->weapon_magazine;
	type_to_class[eSelectedType::kSelectedType_Weapon_StationaryMachineGun] = g_pClsidManager->weapon_stationary_machine_gun;

	for (const std::pair<eSelectedType, CLASS_ID>& pair : type_to_class)
	{
		class_to_type[pair.second] = pair.first;
	}

	for (int i = 0; i < (eSelectedType::kSelectedType_Count); ++i)
	{
		char* pPtr = &category_names[i][0];
		const char* pStr = convertTypeToString(i);
		char result[32]{};

		if (pStr == nullptr && type_to_class.find(eSelectedType(i)) != type_to_class.end())
		{
			char name[16]{};
			CLASS_ID id = type_to_class.at(eSelectedType(i));
			CLSID2TEXT(id, name);

			for (int i = 0; i < 16; ++i)
			{
				if (name[i] == 32)
				{
					name[i] = '\0';
				}
			}
			const char* pTranslatedName = g_pStringTable ? g_pStringTable->translate(name).c_str() : name;

			if (g_pClsidManager && g_pClsidManager->is_monster(id))
			{
				memcpy_s(result, sizeof(result), "Monster - ", sizeof("Monster - "));
				memcpy_s(&result[0] + sizeof("Monster -"), sizeof(result) - sizeof("Monster -"), pTranslatedName, strlen(pTranslatedName));
			}
			else if (g_pClsidManager && g_pClsidManager->is_weapon(id))
			{
				memcpy_s(result, sizeof(result), "Weapon - ", sizeof("Weapon - "));
				memcpy_s(&result[0] + sizeof("Weapon -"), sizeof(result) - sizeof("Weapon -"), pTranslatedName, strlen(pTranslatedName));
			}
			else
			{
				memcpy_s(result, sizeof(result), pTranslatedName, strlen(pTranslatedName));
			}

			pStr = result;
		}
		else
		{
			// unable to obtain the pointer of string, so we just mark it as warning to developers
			if (pStr == nullptr)
				pStr = "FAILED_TO_TRANSLATE";
		}

		memcpy_s(pPtr, sizeof(category_names[i]), pStr, strlen(pStr));

		combo_items[i] = pPtr;
	}

	initTranslatedLabels();


	is_initialized = true;
}

// pre-caching naming for fast accessing and reducing requests to StringTable manager, it is slow...

void CImGuiGameSearchManager::initTranslatedLabels()
{
	if (g_pClsidManager == nullptr)
		return;

	// if we unable to get info from StringTable manager we get a persistent pointer from .text section of dll so it is just string defines on "stack", see getDefaultNameOfSelectedType

	pTranslatedLabel_Artefact = getTranslatedString(eSelectedType::kSelectedType_Artefact);
	pTranslatedLabel_Car = getTranslatedString(eSelectedType::kSelectedType_Car);
	pTranslatedLabel_LevelChanger = getTranslatedString(eSelectedType::kSelectedType_LevelChanger);
	pTranslatedLabel_SmartCover = getTranslatedString(eSelectedType::kSelectedType_SmartCover);
	pTranslatedLabel_SmartTerrain = getTranslatedString(eSelectedType::kSelectedType_SmartTerrain);
	pTranslatedLabel_Stalker = getTranslatedString(eSelectedType::kSelectedType_Stalker);
}

const char* CImGuiGameSearchManager::getDefaultNameOfSelectedType(eSelectedType type)
{
	switch (type)
	{
	case eSelectedType::kSelectedType_SmartCover:
		return "default_Smart Cover";
	case eSelectedType::kSelectedType_SmartTerrain:
		return "default_Smart Terrain";
	case eSelectedType::kSelectedType_Stalker:
		return "default_Stalker";
	case eSelectedType::kSelectedType_Car:
		return "default_Car";
	case eSelectedType::kSelectedType_LevelChanger:
		return "default_LevelChanger";
	case eSelectedType::kSelectedType_Artefact:
		return "default_Artefact";
	default:
		return "DEFAULT_NAME_FAILED_TO_TRANSLATE";
	}
}
const char* CImGuiGameSearchManager::getTranslatedString(eSelectedType type)
{
	char name[16]{};
	CLASS_ID id = type_to_class.at(type);
	CLSID2TEXT(id, name);

	for (int i = 0; i < 16; ++i)
	{
		if (name[i] == 32)
		{
			name[i] = '\0';
		}
	}

	const char* pResult = nullptr;

	pResult = g_pStringTable ? g_pStringTable->translate(name).c_str() : getDefaultNameOfSelectedType(type);

	return pResult;
}

void clsid_manager::add_mp_stuff(CLASS_ID id) {
	if (!is_item(id))
		mp_stuffs.insert(id);
}

bool clsid_manager::is_mp_stuff(CLASS_ID id) {
	return mp_stuffs.find(id) != mp_stuffs.end();
}

void clsid_manager::add_item(CLASS_ID id) {
	if (!is_item(id))
		items.insert(id);
}

bool clsid_manager::is_item(CLASS_ID id) {
	return items.find(id) != items.end();
}

void clsid_manager::add_outfit(CLASS_ID id) {
	if (!is_outfit(id))
		outfits.insert(id);
}

bool clsid_manager::is_outfit(CLASS_ID id) {
	return outfits.find(id) != outfits.end();
}
void clsid_manager::add_ammo(CLASS_ID id) {
	if (!is_ammo(id))
		ammo.insert(id);
}

bool clsid_manager::is_ammo(CLASS_ID id) {
	return ammo.find(id) != ammo.end();
}
void clsid_manager::add_weapon(CLASS_ID id) {
	if (!is_weapon(id))
		weapons.insert(id);
}

bool clsid_manager::is_weapon(CLASS_ID id) {
	return weapons.find(id) != weapons.end();
}
void clsid_manager::add_monster(CLASS_ID id) {
	if (!is_monster(id))
		monsters.insert(id);
}

bool clsid_manager::is_monster(CLASS_ID id) {
	return monsters.find(id) != monsters.end();
}
void clsid_manager::add_addon(CLASS_ID id) {
	if (!is_addon(id))
		addons.insert(id);
}

bool clsid_manager::is_addon(CLASS_ID id) {
	return addons.find(id) != addons.end();
}
void clsid_manager::add_artefact(CLASS_ID id) {
	if (!is_artefact(id))
		artefacts.insert(id);
}

bool clsid_manager::is_artefact(CLASS_ID id) {
	return artefacts.find(id) != artefacts.end();
}
void clsid_manager::add_vehicle(CLASS_ID id) {
	if (!is_vehicle(id))
		vehicles.insert(id);
}

bool clsid_manager::is_vehicle(CLASS_ID id) {
	return vehicles.find(id) != vehicles.end();
}
const char* clsid_manager::translateCLSID(CLASS_ID id) {
	char name[16]{};
	CLSID2TEXT(id, name);

	for (int i = 0; i < 16; ++i)
	{
		if (name[i] == 32)
		{
			name[i] = '\0';
		}
	}
	return g_pStringTable ? g_pStringTable->translate(name).c_str() : name;
}

#include "stdafx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

void execute_console_command_deferred(CConsole* c, LPCSTR string_to_execute);

struct
{
	void add_weapon(CLASS_ID id)
	{
		if (weapons.find(id) == weapons.end())
			weapons.insert(id);
	}

	void add_monster(CLASS_ID id)
	{
		if (monsters.find(id) == monsters.end())
			monsters.insert(id);
	}

	bool is_weapon(CLASS_ID id)
	{
		return weapons.find(id) != weapons.end();
	}

	bool is_monster(CLASS_ID id)
	{
		return monsters.find(id) != monsters.end();
	}

	const xr_set<CLASS_ID>& get_monsters(void) const { return monsters; }
	const xr_set<CLASS_ID>& get_weapons(void) const { return weapons; }

	const char* translateCLSID(CLASS_ID id)
	{
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

	// reminder: information took from class_registrator.script because it overloads existed classes (clsids)
	CLASS_ID artefact = TEXT2CLSID("SCRPTART");
	CLASS_ID car = TEXT2CLSID("SCRPTCAR");
	CLASS_ID stalker = TEXT2CLSID("AI_STL_S");
	CLASS_ID smart_terrain = TEXT2CLSID("SMRTTRRN");
	CLASS_ID smart_cover = TEXT2CLSID("SMRT_C_S");
	CLASS_ID level_changer = TEXT2CLSID("LVL_CHNG");
	CLASS_ID sim_squad_scripted = TEXT2CLSID("ON_OFF_S");
	CLASS_ID outfit = TEXT2CLSID("E_STLK");
	CLASS_ID pda = TEXT2CLSID("S_PDA");
	CLASS_ID food = TEXT2CLSID("S_FOOD");

	CLASS_ID monster_bloodsucker = TEXT2CLSID("SM_BLOOD");
	CLASS_ID monster_boar = TEXT2CLSID("SM_BOARW");
	CLASS_ID monster_dog = TEXT2CLSID("SM_DOG_S");
	CLASS_ID monster_flesh = TEXT2CLSID("SM_FLESH");
	CLASS_ID monster_pseudodog = TEXT2CLSID("SM_P_DOG");
	CLASS_ID monster_burer = TEXT2CLSID("SM_BURER");
	CLASS_ID monster_cat = TEXT2CLSID("SM_CAT_S");
	CLASS_ID monster_chimera = TEXT2CLSID("SM_CHIMS");
	CLASS_ID monster_controller = TEXT2CLSID("SM_CONTR");
	CLASS_ID monster_izlom = TEXT2CLSID("SM_IZLOM");
	CLASS_ID monster_poltergeist = TEXT2CLSID("SM_POLTR");
	CLASS_ID monster_pseudogigant = TEXT2CLSID("SM_GIANT");
	CLASS_ID monster_zombie = TEXT2CLSID("SM_ZOMBI");
	CLASS_ID monster_snork = TEXT2CLSID("SM_SNORK");
	CLASS_ID monster_tushkano = TEXT2CLSID("SM_TUSHK");
	CLASS_ID monster_psydog = TEXT2CLSID("SM_DOG_P");
	CLASS_ID monster_psydogphantom = TEXT2CLSID("SM_DOG_F");

	CLASS_ID weapon_binocular = TEXT2CLSID("WP_BINOC");
	CLASS_ID weapon_knife = TEXT2CLSID("WP_KNIFE");
	CLASS_ID weapon_bm16 = TEXT2CLSID("WP_BM16");
	CLASS_ID weapon_groza = TEXT2CLSID("WP_GROZA");
	CLASS_ID weapon_svd = TEXT2CLSID("WP_SVD");
	CLASS_ID weapon_ak74 = TEXT2CLSID("WP_AK74");
	CLASS_ID weapon_lr300 = TEXT2CLSID("WP_LR300");
	CLASS_ID weapon_hpsa = TEXT2CLSID("WP_HPSA");
	CLASS_ID weapon_pm = TEXT2CLSID("WP_PM");
	CLASS_ID weapon_rg6 = TEXT2CLSID("WP_RG6");
	CLASS_ID weapon_rpg7 = TEXT2CLSID("WP_RPG7");
	CLASS_ID weapon_shotgun = TEXT2CLSID("WP_SHOTG");
	CLASS_ID weapon_autoshotgun = TEXT2CLSID("WP_ASHTG");
	CLASS_ID weapon_svu = TEXT2CLSID("WP_SVU");
	CLASS_ID weapon_usp45 = TEXT2CLSID("WP_USP45");
	CLASS_ID weapon_val = TEXT2CLSID("WP_VAL");
	CLASS_ID weapon_vintorez = TEXT2CLSID("WP_VINT");
	CLASS_ID weapon_walther = TEXT2CLSID("WP_WALTH");
	CLASS_ID weapon_magazine = TEXT2CLSID("WP_MAGAZ");
	CLASS_ID weapon_stationary_machine_gun = TEXT2CLSID("W_STMGUN");

private:
	xr_set<CLASS_ID> weapons;
	xr_set<CLASS_ID> monsters;
	xr_set<CLASS_ID> zones;
	xr_set<CLASS_ID> devices;

}

imgui_clsid_manager;


enum eSelectedType {
	kSelectedType_All,
	kSelectedType_SmartTerrain,
	kSelectedType_SmartCover,
	kSelectedType_LevelChanger,
	kSelectedType_Artefact,
	kSelectedType_Stalker,
	kSelectedType_Car,
	kSelectedType_Monster_All,
	kSelectedType_Monster_BloodSucker,
	kSelectedType_Monster_Boar,
	kSelectedType_Monster_Dog,
	kSelectedType_Monster_Flesh,
	kSelectedType_Monster_PseudoDog,
	kSelectedType_Monster_Burer,
	kSelectedType_Monster_Cat,
	kSelectedType_Monster_Chimera,
	kSelectedType_Monster_Controller,
	kSelectedType_Monster_Izlom,
	kSelectedType_Monster_Poltergeist,
	kSelectedType_Monster_PseudoGigant,
	kSelectedType_Monster_Zombie,
	kSelectedType_Monster_Snork,
	kSelectedType_Monster_Tushkano,
	kSelectedType_Monster_PsyDog,
	kSelectedType_Monster_PsyDogPhantom,
	kSelectedType_Weapon_All,
	kSelectedType_Weapon_Binocular,
	kSelectedType_Weapon_Knife,
	kSelectedType_Weapon_BM16,
	kSelectedType_Weapon_Groza,
	kSelectedType_Weapon_SVD,
	kSelectedType_Weapon_AK74,
	kSelectedType_Weapon_LR300,
	kSelectedType_Weapon_HPSA,
	kSelectedType_Weapon_PM,
	kSelectedType_Weapon_RG6,
	kSelectedType_Weapon_RPG7,
	kSelectedType_Weapon_Shotgun,
	kSelectedType_Weapon_AutoShotgun,
	kSelectedType_Weapon_SVU,
	kSelectedType_Weapon_USP45,
	kSelectedType_Weapon_VAL,
	kSelectedType_Weapon_VINTOREZ,
	kSelectedType_Weapon_WALTHER,
	kSelectedType_Weapon_Magazine,
	kSelectedType_Weapon_StationaryMachineGun,
	kSelectedType_Count
};

struct {

	bool show_alive_creatures{};
	int selected_type{};
	char search_string[256]{};
	char category_names[(eSelectedType::kSelectedType_Count)][32];
	const char* combo_items[(eSelectedType::kSelectedType_Count)]{};
	int counts[(eSelectedType::kSelectedType_Count)]{};

	xr_hash_map<eSelectedType, CLASS_ID> type_to_class;
	xr_hash_map<CLASS_ID, eSelectedType> class_to_type;

	eSelectedType convertCLSIDToType(CLASS_ID id)
	{
		eSelectedType result = eSelectedType::kSelectedType_Count;

		if (class_to_type.find(id) != class_to_type.end())
			result = class_to_type.at(id);

		return result;
	}

	const char* convertTypeToString(int type)
	{
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


		if (type_to_class.find(eSelectedType(type)) != type_to_class.end())
		{
			char name[16]{};
			CLASS_ID id = type_to_class.at(eSelectedType(type));
			CLSID2TEXT(id, name);

			for (int i = 0; i < 16; ++i)
			{
				if (name[i] == 32)
				{
					name[i] = '\0';
				}
			}
			const char* pTranslatedName = g_pStringTable ? g_pStringTable->translate(name).c_str() : name;
			char result[32]{};


			if (imgui_clsid_manager.is_monster(id))
			{
				memcpy_s(result, sizeof(result), "Monster - ", sizeof("Monster - "));
				memcpy_s(&result[0] + sizeof("Monster -"), sizeof(result), pTranslatedName, strlen(pTranslatedName));
			}
			else if (imgui_clsid_manager.is_weapon(id))
			{
				memcpy_s(result, sizeof(result), "Weapon - ", sizeof("Weapon - "));
				memcpy_s(&result[0] + sizeof("Weapon -"), sizeof(result), pTranslatedName, strlen(pTranslatedName));
			}
			else
			{
				memcpy_s(result, sizeof(result), pTranslatedName, strlen(pTranslatedName));
			}

			return result;
		}
		
		return "unknown";
	}

	bool filter(CLASS_ID id)
	{
		bool result{};

		if (selected_type == eSelectedType::kSelectedType_All)
		{
			result = true;
			return result;
		}

		if (selected_type == eSelectedType::kSelectedType_Monster_All)
		{
			if (imgui_clsid_manager.is_monster(id))
			{
				result = true;
				return result;
			}
		}

		if (selected_type == eSelectedType::kSelectedType_Weapon_All)
		{
			if (imgui_clsid_manager.is_weapon(id))
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

	void count(CLASS_ID id)
	{
		counts[(eSelectedType::kSelectedType_All)] += 1;

		if (imgui_clsid_manager.is_monster(id))
		{
			counts[eSelectedType::kSelectedType_Monster_All] += 1;

			if (class_to_type.find(id) != class_to_type.end())
				counts[class_to_type.at(id)] += 1;

		}
		else if (imgui_clsid_manager.is_weapon(id))
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

	void init()
	{
		type_to_class[eSelectedType::kSelectedType_SmartTerrain] = imgui_clsid_manager.smart_terrain;
		type_to_class[eSelectedType::kSelectedType_SmartCover] = imgui_clsid_manager.smart_cover;
		type_to_class[eSelectedType::kSelectedType_LevelChanger] = imgui_clsid_manager.level_changer;
		type_to_class[eSelectedType::kSelectedType_Artefact] = imgui_clsid_manager.artefact;
		type_to_class[eSelectedType::kSelectedType_Stalker] = imgui_clsid_manager.stalker;
		type_to_class[eSelectedType::kSelectedType_Car] = imgui_clsid_manager.car;
		type_to_class[eSelectedType::kSelectedType_Monster_BloodSucker] = imgui_clsid_manager.monster_bloodsucker;
		type_to_class[eSelectedType::kSelectedType_Monster_Boar] = imgui_clsid_manager.monster_boar;
		type_to_class[eSelectedType::kSelectedType_Monster_Dog] = imgui_clsid_manager.monster_dog;
		type_to_class[eSelectedType::kSelectedType_Monster_Flesh] = imgui_clsid_manager.monster_flesh;
		type_to_class[eSelectedType::kSelectedType_Monster_PseudoDog] = imgui_clsid_manager.monster_pseudodog;
		type_to_class[eSelectedType::kSelectedType_Monster_Burer] = imgui_clsid_manager.monster_burer;
		type_to_class[eSelectedType::kSelectedType_Monster_Cat] = imgui_clsid_manager.monster_cat;
		type_to_class[eSelectedType::kSelectedType_Monster_Chimera] = imgui_clsid_manager.monster_chimera;
		type_to_class[eSelectedType::kSelectedType_Monster_Controller] = imgui_clsid_manager.monster_controller;
		type_to_class[eSelectedType::kSelectedType_Monster_Izlom] = imgui_clsid_manager.monster_izlom;
		type_to_class[eSelectedType::kSelectedType_Monster_Poltergeist] = imgui_clsid_manager.monster_poltergeist;
		type_to_class[eSelectedType::kSelectedType_Monster_PseudoGigant] = imgui_clsid_manager.monster_pseudogigant;
		type_to_class[eSelectedType::kSelectedType_Monster_Zombie] = imgui_clsid_manager.monster_zombie;
		type_to_class[eSelectedType::kSelectedType_Monster_Snork] = imgui_clsid_manager.monster_snork;
		type_to_class[eSelectedType::kSelectedType_Monster_Tushkano] = imgui_clsid_manager.monster_tushkano;
		type_to_class[eSelectedType::kSelectedType_Monster_PsyDog] = imgui_clsid_manager.monster_psydog;
		type_to_class[eSelectedType::kSelectedType_Monster_PsyDogPhantom] = imgui_clsid_manager.monster_psydogphantom;


		type_to_class[eSelectedType::kSelectedType_Weapon_Binocular] = imgui_clsid_manager.weapon_binocular;
		type_to_class[eSelectedType::kSelectedType_Weapon_Knife] = imgui_clsid_manager.weapon_knife;
		type_to_class[eSelectedType::kSelectedType_Weapon_BM16] = imgui_clsid_manager.weapon_bm16;
		type_to_class[eSelectedType::kSelectedType_Weapon_Groza] = imgui_clsid_manager.weapon_groza;
		type_to_class[eSelectedType::kSelectedType_Weapon_SVD] = imgui_clsid_manager.weapon_svd;
		type_to_class[eSelectedType::kSelectedType_Weapon_AK74] = imgui_clsid_manager.weapon_ak74;
		type_to_class[eSelectedType::kSelectedType_Weapon_LR300] = imgui_clsid_manager.weapon_lr300;
		type_to_class[eSelectedType::kSelectedType_Weapon_HPSA] = imgui_clsid_manager.weapon_hpsa;
		type_to_class[eSelectedType::kSelectedType_Weapon_PM] = imgui_clsid_manager.weapon_pm;
		type_to_class[eSelectedType::kSelectedType_Weapon_RG6] = imgui_clsid_manager.weapon_rg6;
		type_to_class[eSelectedType::kSelectedType_Weapon_RPG7] = imgui_clsid_manager.weapon_rpg7;
		type_to_class[eSelectedType::kSelectedType_Weapon_Shotgun] = imgui_clsid_manager.weapon_shotgun;
		type_to_class[eSelectedType::kSelectedType_Weapon_AutoShotgun] = imgui_clsid_manager.weapon_autoshotgun;
		type_to_class[eSelectedType::kSelectedType_Weapon_SVU] = imgui_clsid_manager.weapon_svu;
		type_to_class[eSelectedType::kSelectedType_Weapon_USP45] = imgui_clsid_manager.weapon_usp45;
		type_to_class[eSelectedType::kSelectedType_Weapon_VAL] = imgui_clsid_manager.weapon_val;
		type_to_class[eSelectedType::kSelectedType_Weapon_VINTOREZ] = imgui_clsid_manager.weapon_vintorez;
		type_to_class[eSelectedType::kSelectedType_Weapon_WALTHER] = imgui_clsid_manager.weapon_walther;
		type_to_class[eSelectedType::kSelectedType_Weapon_Magazine] = imgui_clsid_manager.weapon_magazine;
		type_to_class[eSelectedType::kSelectedType_Weapon_StationaryMachineGun] = imgui_clsid_manager.weapon_stationary_machine_gun;

		for (const std::pair<eSelectedType, CLASS_ID>& pair : type_to_class)
		{
			class_to_type[pair.second] = pair.first;
		}

		for (int i = 0; i < (eSelectedType::kSelectedType_Count); ++i)
		{
			char* pPtr = &category_names[i][0];
			const char* pStr = convertTypeToString(i);

			memcpy_s(pPtr, sizeof(category_names[i]), pStr, strlen(pStr));

			combo_items[i] = pPtr;
		}
	}
}

imgui_search_manager;

void RenderSearchManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SearchManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	if (ImGui::Begin("Search Manager"), &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SearchManager)])
	{
		constexpr size_t kItemSize = sizeof(imgui_search_manager.combo_items) / sizeof(imgui_search_manager.combo_items[0]);
		ImGui::Combo("Category", &imgui_search_manager.selected_type, imgui_search_manager.combo_items, kItemSize);

		ImGui::SeparatorText("Stats");
		ImGui::Text("Current category: %s (%d)", imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type), imgui_search_manager.selected_type);
		ImGui::Text("Level: %s", Level().name().c_str());

		ImGui::Text("All: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_All)]);
		ImGui::Text("%s: %d", imgui_search_manager.convertTypeToString(eSelectedType::kSelectedType_SmartCover), imgui_search_manager.counts[(eSelectedType::kSelectedType_SmartCover)]);
		ImGui::Text("%s: %d", imgui_search_manager.convertTypeToString(eSelectedType::kSelectedType_SmartTerrain), imgui_search_manager.counts[(eSelectedType::kSelectedType_SmartTerrain)]);
		ImGui::Text("%s: %d", imgui_search_manager.convertTypeToString(eSelectedType::kSelectedType_Stalker), imgui_search_manager.counts[(eSelectedType::kSelectedType_Stalker)]);
		ImGui::Text("%s: %d", imgui_search_manager.convertTypeToString(eSelectedType::kSelectedType_Car), imgui_search_manager.counts[(eSelectedType::kSelectedType_Car)]);
		ImGui::Text("%s: %d", imgui_search_manager.convertTypeToString(eSelectedType::kSelectedType_LevelChanger), imgui_search_manager.counts[(eSelectedType::kSelectedType_LevelChanger)]);
		ImGui::Text("%s: %d", imgui_search_manager.convertTypeToString(eSelectedType::kSelectedType_Artefact), imgui_search_manager.counts[(eSelectedType::kSelectedType_Artefact)]);

		char colh_monsters[24]{};
		sprintf_s(colh_monsters, sizeof(colh_monsters), "Monsters: %d", imgui_search_manager.counts[eSelectedType::kSelectedType_Monster_All]);

		if (ImGui::CollapsingHeader(colh_monsters))
		{
			for (const auto& id : imgui_clsid_manager.get_monsters())
			{
				char monster_name[32]{};
				sprintf_s(monster_name, sizeof(monster_name), "%s: %d", imgui_clsid_manager.translateCLSID(id), imgui_search_manager.counts[imgui_search_manager.convertCLSIDToType(id)]);
				ImGui::Text(monster_name);
			}
		}

		char colh_weapons[24]{};
		sprintf_s(colh_weapons, sizeof(colh_weapons), "Weapons: %d", imgui_search_manager.counts[eSelectedType::kSelectedType_Weapon_All]);

		if (ImGui::CollapsingHeader(colh_weapons))
		{
			for (const auto& id : imgui_clsid_manager.get_weapons())
			{
				char weapon_name[32]{};
				sprintf_s(weapon_name, sizeof(weapon_name), "%s: %d", imgui_clsid_manager.translateCLSID(id), imgui_search_manager.counts[imgui_search_manager.convertCLSIDToType(id)]);
				ImGui::Text(weapon_name);
			}
		}

		ImGui::SeparatorText("Settings");
		ImGui::Checkbox("Alive", &imgui_search_manager.show_alive_creatures);
		if (ImGui::BeginItemTooltip())
		{
			ImGui::Text("Shows alive or not alive creature(if it is not creature this flag doesn't affect)");
			ImGui::EndTooltip();
		}

		ImGui::SeparatorText("Simulation");

		if (ImGui::BeginTabBar("##TB_InGameSearchManager"))
		{
			if (ImGui::BeginTabItem("Online##TB_Online_InGameSearchManager"))
			{
				memset(imgui_search_manager.counts, 0, sizeof(imgui_search_manager.counts));

				ImGui::InputText("##IT_InGameSeachManager", imgui_search_manager.search_string, sizeof(imgui_search_manager.search_string));

				char category_name_separator[64]{};
				const char* pTranslatedCategoryName = imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type);
				size_t translate_str_len = strlen(pTranslatedCategoryName);
				memcpy_s(category_name_separator, sizeof(category_name_separator), pTranslatedCategoryName, translate_str_len);
				ImGui::SeparatorText(category_name_separator);

				auto size = Level().Objects.o_count();

				for (auto i = 0; i < size; ++i)
				{
					auto* pObject = Level().Objects.o_get_by_iterator(i);

					if (pObject && pObject->H_Parent() == nullptr)
					{
						imgui_search_manager.count(pObject->CLS_ID);

						if (imgui_search_manager.filter(pObject->CLS_ID))
						{
							auto filter_string_size = strlen(imgui_search_manager.search_string);

							CGameObject* pCasted = smart_cast<CGameObject*>(pObject);
							bool passed_filter{ true };
							if (filter_string_size)
							{
								if (pCasted && pObject)
								{
									std::string_view cname = pObject->cName().c_str();
									std::string_view translate_name = Platform::ANSI_TO_UTF8(g_pStringTable->translate(pCasted->Name()).c_str()).c_str();

									if (cname.find(imgui_search_manager.search_string) == xr_string::npos && translate_name.find(imgui_search_manager.search_string) == xr_string::npos)
									{
										passed_filter = false;
									}
								}
							}

							if (pCasted)
							{
								if (imgui_search_manager.show_alive_creatures)
								{
									CEntity* pEntity = smart_cast<CEntity*>(pCasted);

									if (pEntity)
									{
										if (!pEntity->g_Alive())
										{
											passed_filter = false;
										}
									}
								}
							}

							if (passed_filter)
							{
								xr_string name;

								name = pObject->cName().c_str();

								if (pCasted)
								{
									name += " ";
									name += "[";
									name += Platform::ANSI_TO_UTF8(g_pStringTable->translate(pCasted->Name()).c_str());
									name += "]";
								}
								name += "##InGame_SM_";
								name += std::to_string(i);

								if (ImGui::Button(name.c_str()))
								{
									CActor* pActor = smart_cast<CActor*>(Level().CurrentEntity());

									if (pActor)
									{
										xr_string cmd;
										cmd = "set_actor_position ";
										cmd += cmd.ToString(pObject->Position().x);
										cmd += ",";
										cmd += cmd.ToString(pObject->Position().y);
										cmd += ",";
										cmd += cmd.ToString(pObject->Position().z);

										execute_console_command_deferred(Console, cmd.c_str());
									}
								}

								if (ImGui::BeginItemTooltip())
								{
									ImGui::Text("system name: [%s]", pObject->cName().c_str());
									ImGui::Text("section name: [%s]", pObject->cNameSect().c_str());
									ImGui::Text("translated name: [%s]", Platform::ANSI_TO_UTF8(g_pStringTable->translate(pCasted->Name()).c_str()).c_str());
									ImGui::Text("position: %f %f %f", pObject->Position().x, pObject->Position().y, pObject->Position().z);



									ImGui::EndTooltip();
								}
							}
						}
					}
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Offline##TB_Offline_InGameSearchManager"))
			{
				memset(imgui_search_manager.counts, 0, sizeof(imgui_search_manager.counts));

				ImGui::InputText("##IT_InGameSearchManager", imgui_search_manager.search_string, sizeof(imgui_search_manager.search_string));

				char category_name_separator[64]{};
				const char* pTranslatedCategoryName = imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type);
				size_t translate_str_len = strlen(pTranslatedCategoryName);
				memcpy_s(category_name_separator, sizeof(category_name_separator), pTranslatedCategoryName, translate_str_len);
				ImGui::SeparatorText(category_name_separator);

				const auto& objects = ai().alife().objects().objects();
				for (const auto& it : objects)
				{
					auto* pServerObject = it.second;

					if (pServerObject)
					{
						if (pServerObject->ID_Parent == 0xffff)
						{
							imgui_search_manager.count(pServerObject->m_tClassID);

							if (imgui_search_manager.filter(pServerObject->m_tClassID))
							{
								xr_string name;

								name = pServerObject->name_replace();
								if (ImGui::Button(name.c_str()))
								{
									CActor* pActor = smart_cast<CActor*>(Level().CurrentEntity());

									if (pActor)
									{
										xr_string cmd;
										cmd = "set_actor_position ";
										cmd += cmd.ToString(pServerObject->Position().x);
										cmd += ",";
										cmd += cmd.ToString(pServerObject->Position().y);
										cmd += ",";
										cmd += cmd.ToString(pServerObject->Position().z);

										execute_console_command_deferred(Console, cmd.c_str());
									}
								}
							}
						}
					}
				}


				ImGui::EndTabItem();
			}

			ImGui::EndTabBar();
		}

		ImGui::End();
	}
}

void InitImGuiCLSIDInGame()
{
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_BLOOD"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_BOARW"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_DOG_S"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_FLESH"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_P_DOG"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_BURER"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_CAT_S"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_CHIMS"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_CONTR"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_IZLOM"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_POLTR"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_GIANT"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_ZOMBI"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_SNORK"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_TUSHK"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_DOG_P"));
	imgui_clsid_manager.add_monster(TEXT2CLSID("SM_DOG_F"));

	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_BINOC"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_KNIFE"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_BM16"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_GROZA"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_SVD"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_AK74"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_LR300"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_HPSA"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_PM"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_RG6"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_RPG7"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_SHOTG"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_ASHTG"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_MAGAZ"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_SVU"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_USP45"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_VAL"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_VINT"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("WP_WALTH"));
	imgui_clsid_manager.add_weapon(TEXT2CLSID("W_STMGUN"));
}


void InitImGuiSearchInGame()
{
	imgui_search_manager.init();
}

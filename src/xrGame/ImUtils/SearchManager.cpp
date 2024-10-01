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
		if (id == monster_bloodsucker)
		{
			return "Bloodsucker";
		}
		else if (id == monster_boar)
		{
			return "Boar";
		}
		else if (id == monster_burer)
		{
			return "Burer";
		}
		else if (id == monster_cat)
		{
			return "Cat";
		}
		else if (id == monster_chimera)
		{
			return "Chimera";
		}
		else if (id == monster_controller)
		{
			return "Controller";
		}
		else if (id == monster_dog)
		{
			return "Dog";
		}
		else if (id == monster_flesh)
		{
			return "Flesh";
		}
		else if (id == monster_izlom)
		{
			return "Izlom";
		}
		else if (id == monster_poltergeist)
		{
			return "Poltergeist";
		}
		else if (id == monster_pseudodog)
		{
			return "PseudoDog";
		}
		else if (id == monster_pseudogigant)
		{
			return "PseudoGigant";
		}
		else if (id == monster_psydog)
		{
			return "PsyDog";
		}
		else if (id == monster_psydogphantom)
		{
			return "PsyDog (Phantom)";
		}
		else if (id == monster_snork)
		{
			return "Snork";
		}
		else if (id == monster_tushkano)
		{
			return "Tushkano";
		}
		else if (id == monster_zombie)
		{
			return "Zombie";
		}
		else
		{
			return "unknown";
		}
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
	kSelectedType_Count
};

struct {

	bool show_alive_creatures{};
	int selected_type{};
	char search_string[256]{};
	char category_names[(eSelectedType::kSelectedType_Count)][32];
	const char* combo_items[(eSelectedType::kSelectedType_Count)]{};
	int counts[(eSelectedType::kSelectedType_Count)]{};

	eSelectedType convertCLSIDToType(CLASS_ID id)
	{
		if (id == imgui_clsid_manager.monster_bloodsucker)
		{
			return eSelectedType::kSelectedType_Monster_BloodSucker;
		}
		else if (id == imgui_clsid_manager.monster_boar)
		{
			return eSelectedType::kSelectedType_Monster_Boar;
		}
		else if (id == imgui_clsid_manager.monster_burer)
		{
			return eSelectedType::kSelectedType_Monster_Burer;
		}
		else if (id == imgui_clsid_manager.monster_cat)
		{
			return eSelectedType::kSelectedType_Monster_Cat;
		}
		else if (id == imgui_clsid_manager.monster_chimera)
		{
			return eSelectedType::kSelectedType_Monster_Chimera;
		}
		else if (id == imgui_clsid_manager.monster_controller)
		{
			return eSelectedType::kSelectedType_Monster_Controller;
		}
		else if (id == imgui_clsid_manager.monster_dog)
		{
			return eSelectedType::kSelectedType_Monster_Dog;
		}
		else if (id == imgui_clsid_manager.monster_flesh)
		{
			return eSelectedType::kSelectedType_Monster_Flesh;
		}
		else if (id == imgui_clsid_manager.monster_izlom)
		{
			return eSelectedType::kSelectedType_Monster_Izlom;
		}
		else if (id == imgui_clsid_manager.monster_poltergeist)
		{
			return eSelectedType::kSelectedType_Monster_Poltergeist;
		}
		else if (id == imgui_clsid_manager.monster_pseudodog)
		{
			return eSelectedType::kSelectedType_Monster_PseudoDog;
		}
		else if (id == imgui_clsid_manager.monster_pseudogigant)
		{
			return eSelectedType::kSelectedType_Monster_PseudoGigant;
		}
		else if (id == imgui_clsid_manager.monster_psydog)
		{
			return eSelectedType::kSelectedType_Monster_PsyDog;
		}
		else if (id == imgui_clsid_manager.monster_psydogphantom)
		{
			return eSelectedType::kSelectedType_Monster_PsyDogPhantom;
		}
		else if (id == imgui_clsid_manager.monster_snork)
		{
			return eSelectedType::kSelectedType_Monster_Snork;
		}
		else if (id == imgui_clsid_manager.monster_tushkano)
		{
			return eSelectedType::kSelectedType_Monster_Tushkano;
		}
		else if (id == imgui_clsid_manager.monster_zombie)
		{
			return eSelectedType::kSelectedType_Monster_Zombie;
		}
		else
		{
			return eSelectedType::kSelectedType_Count;
		}
	}

	const char* convertTypeToString(int type)
	{
		switch (static_cast<eSelectedType>(type))
		{
		case eSelectedType::kSelectedType_All:
		{
			return "All";
		}
		case eSelectedType::kSelectedType_SmartTerrain:
		{
			return "Smart terrain";
		}
		case eSelectedType::kSelectedType_SmartCover:
		{
			return "Smart cover";
		}
		case eSelectedType::kSelectedType_LevelChanger:
		{
			return "Level changer";
		}
		case eSelectedType::kSelectedType_Artefact:
		{
			return "Artefact";
		}
		case eSelectedType::kSelectedType_Stalker:
		{
			return "Stalker";
		}
		case eSelectedType::kSelectedType_Car:
		{
			return "Car";
		}
		case eSelectedType::kSelectedType_Monster_All:
		{
			return "Monster - All";
		}
		case eSelectedType::kSelectedType_Monster_BloodSucker:
		{
			return "Monster - bloodsucker";
		}
		case eSelectedType::kSelectedType_Monster_Boar:
		{
			return "Monster - Boar";
		}
		case eSelectedType::kSelectedType_Monster_Burer:
		{
			return "Monster - Burer";
		}
		case eSelectedType::kSelectedType_Monster_Cat:
		{
			return "Monster - Cat";
		}
		case eSelectedType::kSelectedType_Monster_Chimera:
		{
			return "Monster - Chimera";
		}
		case eSelectedType::kSelectedType_Monster_Controller:
		{
			return "Monster - Controller";
		}
		case eSelectedType::kSelectedType_Monster_Dog:
		{
			return "Monster - Dog";
		}
		case eSelectedType::kSelectedType_Monster_Flesh:
		{
			return "Monster - Flesh";
		}
		case eSelectedType::kSelectedType_Monster_Izlom:
		{
			return "Monster - Izlom";
		}
		case eSelectedType::kSelectedType_Monster_Poltergeist:
		{
			return "Monster - Poltergeist";
		}
		case eSelectedType::kSelectedType_Monster_PseudoDog:
		{
			return "Monster - Pseudodog";
		}
		case eSelectedType::kSelectedType_Monster_PseudoGigant:
		{
			return "Monster - PseudoGigant";
		}
		case eSelectedType::kSelectedType_Monster_PsyDog:
		{
			return "Monster - Psydog";
		}
		case eSelectedType::kSelectedType_Monster_PsyDogPhantom:
		{
			return "Monster - psydogphantom";
		}
		case eSelectedType::kSelectedType_Monster_Snork:
		{
			return "Monster - Snork";
		}
		case eSelectedType::kSelectedType_Monster_Tushkano:
		{
			return "Monster - Tushkano";
		}
		case eSelectedType::kSelectedType_Monster_Zombie:
		{
			return "Monster - Zombie";
		}
		default:
		{
			return "unknown";
		}
		}
	}

	bool filter(CLASS_ID id)
	{
		bool result{};

		if (selected_type == eSelectedType::kSelectedType_Monster_All)
		{
			if (imgui_clsid_manager.is_monster(id))
			{
				result = true;
				return result;
			}
		}

		switch (static_cast<eSelectedType>(selected_type))
		{
		case eSelectedType::kSelectedType_All:
		{
			result = true;
			break;
		}
		case eSelectedType::kSelectedType_SmartTerrain:
		{
			result = id == imgui_clsid_manager.smart_terrain;
			break;
		}
		case eSelectedType::kSelectedType_SmartCover:
		{
			result = id == imgui_clsid_manager.smart_cover;
			break;
		}
		case eSelectedType::kSelectedType_LevelChanger:
		{
			result = id == imgui_clsid_manager.level_changer;
			break;
		}
		case eSelectedType::kSelectedType_Artefact:
		{
			result = id == imgui_clsid_manager.artefact;
			break;
		}
		case eSelectedType::kSelectedType_Stalker:
		{
			result = id == imgui_clsid_manager.stalker;
			break;
		}
		case eSelectedType::kSelectedType_Car:
		{
			result = id == imgui_clsid_manager.car;
			break;
		}
		case eSelectedType::kSelectedType_Monster_BloodSucker:
		{
			result = id == imgui_clsid_manager.monster_bloodsucker;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Boar:
		{
			result = id == imgui_clsid_manager.monster_boar;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Burer:
		{
			result = id == imgui_clsid_manager.monster_burer;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Cat:
		{
			result = id == imgui_clsid_manager.monster_cat;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Chimera:
		{
			result = id == imgui_clsid_manager.monster_chimera;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Controller:
		{
			result = id == imgui_clsid_manager.monster_controller;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Dog:
		{
			result = id == imgui_clsid_manager.monster_dog;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Flesh:
		{
			result = id == imgui_clsid_manager.monster_flesh;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Izlom:
		{
			result = id == imgui_clsid_manager.monster_izlom;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Poltergeist:
		{
			result = id == imgui_clsid_manager.monster_poltergeist;
			break;
		}
		case eSelectedType::kSelectedType_Monster_PseudoDog:
		{
			result = id == imgui_clsid_manager.monster_pseudodog;
			break;
		}
		case eSelectedType::kSelectedType_Monster_PseudoGigant:
		{
			result = id == imgui_clsid_manager.monster_pseudogigant;
			break;
		}
		case eSelectedType::kSelectedType_Monster_PsyDog:
		{
			result = id == imgui_clsid_manager.monster_psydog;
			break;
		}
		case eSelectedType::kSelectedType_Monster_PsyDogPhantom:
		{
			result = id == imgui_clsid_manager.monster_psydogphantom;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Snork:
		{
			result = id == imgui_clsid_manager.monster_snork;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Tushkano:
		{
			result = id == imgui_clsid_manager.monster_tushkano;
			break;
		}
		case eSelectedType::kSelectedType_Monster_Zombie:
		{
			result = id == imgui_clsid_manager.monster_zombie;
			break;
		}
		}

		return result;
	}

	void count(CLASS_ID id)
	{
		counts[(eSelectedType::kSelectedType_All)] += 1;

		if (id == imgui_clsid_manager.smart_terrain)
		{
			counts[(eSelectedType::kSelectedType_SmartTerrain)] += 1;
		}
		else if (id == imgui_clsid_manager.smart_cover)
		{
			counts[(eSelectedType::kSelectedType_SmartCover)] += 1;
		}
		else if (id == imgui_clsid_manager.level_changer)
		{
			counts[(eSelectedType::kSelectedType_LevelChanger)] += 1;
		}
		else if (id == imgui_clsid_manager.artefact)
		{
			counts[(eSelectedType::kSelectedType_Artefact)] += 1;
		}
		else if (id == imgui_clsid_manager.stalker)
		{
			counts[(eSelectedType::kSelectedType_Stalker)] += 1;
		}
		else if (id == imgui_clsid_manager.car)
		{
			counts[(eSelectedType::kSelectedType_Car)] += 1;
		}
		else if (imgui_clsid_manager.is_monster(id))
		{
			counts[eSelectedType::kSelectedType_Monster_All] += 1;

			if (id == imgui_clsid_manager.monster_bloodsucker)
			{
				counts[eSelectedType::kSelectedType_Monster_BloodSucker] += 1;
			}
			else if (id == imgui_clsid_manager.monster_boar)
			{
				counts[eSelectedType::kSelectedType_Monster_Boar] += 1;
			}
			else if (id == imgui_clsid_manager.monster_burer)
			{
				counts[eSelectedType::kSelectedType_Monster_Burer] += 1;
			}
			else if (id == imgui_clsid_manager.monster_cat)
			{
				counts[eSelectedType::kSelectedType_Monster_Cat] += 1;
			}
			else if (id == imgui_clsid_manager.monster_chimera)
			{
				counts[eSelectedType::kSelectedType_Monster_Chimera] += 1;
			}
			else if (id == imgui_clsid_manager.monster_controller)
			{
				counts[eSelectedType::kSelectedType_Monster_Controller] += 1;
			}
			else if (id == imgui_clsid_manager.monster_dog)
			{
				counts[eSelectedType::kSelectedType_Monster_Dog] += 1;
			}
			else if (id == imgui_clsid_manager.monster_flesh)
			{
				counts[eSelectedType::kSelectedType_Monster_Flesh] += 1;
			}
			else if (id == imgui_clsid_manager.monster_izlom)
			{
				counts[eSelectedType::kSelectedType_Monster_Izlom] += 1;
			}
			else if (id == imgui_clsid_manager.monster_poltergeist)
			{
				counts[eSelectedType::kSelectedType_Monster_Poltergeist] += 1;
			}
			else if (id == imgui_clsid_manager.monster_pseudodog)
			{
				counts[eSelectedType::kSelectedType_Monster_PseudoDog] += 1;
			}
			else if (id == imgui_clsid_manager.monster_pseudogigant)
			{
				counts[eSelectedType::kSelectedType_Monster_PseudoGigant] += 1;
			}
			else if (id == imgui_clsid_manager.monster_psydog)
			{
				counts[eSelectedType::kSelectedType_Monster_PsyDog] += 1;
			}
			else if (id == imgui_clsid_manager.monster_psydogphantom)
			{
				counts[eSelectedType::kSelectedType_Monster_PsyDogPhantom] += 1;
			}
			else if (id == imgui_clsid_manager.monster_snork)
			{
				counts[eSelectedType::kSelectedType_Monster_Snork] += 1;
			}
			else if (id == imgui_clsid_manager.monster_tushkano)
			{
				counts[eSelectedType::kSelectedType_Monster_Tushkano] += 1;
			}
			else if (id == imgui_clsid_manager.monster_zombie)
			{
				counts[eSelectedType::kSelectedType_Monster_Zombie] += 1;
			}

		}
	}

	void init()
	{
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
		ImGui::Text("Smart covers: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_SmartCover)]);
		ImGui::Text("Smart terrains: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_SmartTerrain)]);
		ImGui::Text("Stalker: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_Stalker)]);
		ImGui::Text("Car: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_Car)]);
		ImGui::Text("Level changer: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_LevelChanger)]);
		ImGui::Text("Artefact: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_Artefact)]);

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

				ImGui::SeparatorText(imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type));

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
										cmd += std::to_string(pObject->Position().x);
										cmd += ",";
										cmd += std::to_string(pObject->Position().y);
										cmd += ",";
										cmd += std::to_string(pObject->Position().z);

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

				ImGui::SeparatorText(imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type));

				const auto& objects = ai().alife().objects().objects();
				for (const auto& it : objects)
				{
					auto* pServerObject = it.second;

					if (pServerObject)
					{
						if (pServerObject->ID_Parent == 0xffff)
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
									cmd += std::to_string(pServerObject->Position().x);
									cmd += ",";
									cmd += std::to_string(pServerObject->Position().y);
									cmd += ",";
									cmd += std::to_string(pServerObject->Position().z);

									execute_console_command_deferred(Console, cmd.c_str());
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

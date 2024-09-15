////////////////////////////////////////////////////////////////////////////
//	Module 		: level_script.cpp
//	Created 	: 28.06.2004
//  Modified 	: 28.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Level script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "Level.h"
#include "Actor.h"
#include "script_game_object.h"
#include "patrol_path_storage.h"
#include "xrServer.h"
#include "client_spawn_manager.h"
#include "../xrEngine/igame_persistent.h"
#include "game_cl_base.h"
#include "UIGameCustom.h"
#include "UI/UIDialogWnd.h"
#include "date_time.h"
#include "ai_space.h"
#include "level_graph.h"
#include "PHCommander.h"
#include "PHScriptCall.h"
#include "../xrScripts/script_engine.h"
#include "game_cl_single.h"
#include "game_sv_single.h"
#include "map_manager.h"
#include "map_spot.h"
#include "map_location.h"
#include "physics_world_scripted.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"
#include "UI/UIGameTutorial.h"
#include "../xrEngine/string_table.h"
#include "ui/UIInventoryUtilities.h"
#include "alife_object_registry.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "HUDAnimItem.h"
#include "ActorCondition.h"
#include "../xrEngine/XR_IOConsole.h"
#include "Inventory.h"
#include "ShootingObject.h"
#include "Weapon.h"


#include "ai_object_location.h"

using namespace luabind;

namespace ixray::save
{
	xr_string CurrentSaveStage = "";
	void SaveError()
	{
		R_ASSERT2(!"Save file is big. Chunk: ", CurrentSaveStage.c_str());
	}

	void SaveStage(const char* Name)
	{
		CurrentSaveStage = Name;
	}
}

struct {
	u32 nTime{};
	char hour[3]{};
}
imgui_time_manager;

void RenderTimeManagerWindow()
{
	if (!g_pGameLevel)
		return;

	if (Level().Server == nullptr)
		return;

	if (Level().Server->game == nullptr)
		return;

	game_sv_Single* pGame = dynamic_cast<game_sv_Single*>(Level().Server->game);

	if (!pGame)
		return;

	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_TimeManager)])
		return;

	if (!ImGui::Begin("Time Manager##InGame", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_TimeManager)]))
	{
		ImGui::End();
		return;
	}

	u64 time = pGame->alife().time_manager().game_time();
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time(time, year, month, day, hours, mins, secs, milisecs);

	ImGui::Text("Current date: %zu/%zu/%zu (day/month/year)", day, month, year);
	ImGui::Text("Current time: %zu [h] : %zu [min] : %zu [sec]", hours, mins, secs);

	ImGui::Text("Add hour:");
	ImGui::SameLine();
	ImGui::InputText("##InputHour_GameTimeManager", imgui_time_manager.hour, sizeof(imgui_time_manager.hour));
	ImGui::SameLine();

	if (ImGui::Button("add##InputHour_GameTimeManager"))
	{
		size_t len = strlen(imgui_time_manager.hour);

		bool valid{ true };
		for (int i = 0; i < len; ++i)
		{
			if (imgui_time_manager.hour[i] == '\0')
				break;

			if (!((imgui_time_manager.hour[i] >= '0' && imgui_time_manager.hour[i] <= '9')))
			{
				valid = false;
				break;
			}
		}

		if (valid)
		{
			u32 casted = atoi(imgui_time_manager.hour);
			clamp(casted, u32(0), u32(24));
			u32 value = casted * 3600 * 1000;
			float fv = casted * 3600;
			g_pGamePersistent->Environment().ChangeGameTime(fv);
			pGame->alife().time_manager().change_game_time(value);
		}
	}



	ImGui::End();
}

struct {

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

struct
{
	// weapon tab
	bool weapon_sort_by_max_cost{};
	bool weapon_sort_by_max_hit_power{};
	bool weapon_sort_by_max_fire_distance{};
	bool weapon_sort_by_min_cost{};
	bool weapon_sort_by_min_hit_power{};
	bool weapon_sort_by_min_fire_distance{};
	bool weapon_spawn_on_level{};
	char weapon_spawn_count[3]{};
	// weapon tab
}

imgui_spawn_manager;

extern CSE_Abstract* CALifeSimulator__spawn_item2(CALifeSimulator* self_, LPCSTR section, const Fvector& position,
	u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent);
void execute_console_command_deferred(CConsole* c, LPCSTR string_to_execute);

void RenderSpawnManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SpawnManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	if (!g_pStringTable)
		return;

	constexpr size_t kSpawnManagerMaxSectionName = 64;

	if (ImGui::Begin("Spawn Manager", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SpawnManager)]))
	{
		if (ImGui::BeginTabBar("##TabBar_InGameSpawnManager"))
		{
			if (ImGui::BeginTabItem("Items"))
			{




				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Artefacts"))
			{




				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Weapons"))
			{
				if (pSettings)
				{
					size_t total_count_sort{};
					size_t total_count_unsort{};

					auto translate_difficulty = [](ESingleGameDifficulty value) -> const char* {
						switch (value)
						{
						case egdNovice:
						{
							return "novice";
						}
						case egdStalker:
						{
							return "stalker";
						}
						case egdVeteran:
						{
							return "veteran";
						}
						case egdMaster:
						{
							return "master";
						}
						default:
							return "unknown";
						}
						};

					/* for fast disabling copy and paste (but delete what is necessary)
						imgui_spawn_manager.weapon_sort_by_max_cost = false;
						imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_cost = false;
						imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
					*/

					if (ImGui::Checkbox("sort by max cost##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_max_cost))
					{
						imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_min_cost = false;
						imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
					}
					ImGui::SetItemTooltip("Sorts items by maximum cost field that defined in weapon section in ltx file");


					if (ImGui::Checkbox("sort by max fire distance##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_max_fire_distance))
					{
						imgui_spawn_manager.weapon_sort_by_max_cost = false;
						imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_min_cost = false;
						imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
					}
					ImGui::SetItemTooltip("Sorts items by maximum fire distance field that defined in weapon section in ltx file");

					if (ImGui::Checkbox("sort by max hit power##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_max_hit_power))
					{
						imgui_spawn_manager.weapon_sort_by_max_cost = false;
						imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_cost = false;
						imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
					}
					ImGui::SetItemTooltip("Sorts items by maximum hit_power field for current game difficulty[%s] that defined in weapon section in ltx file", translate_difficulty(g_SingleGameDifficulty));

					if (ImGui::Checkbox("sort by min cost##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_min_cost))
					{
						imgui_spawn_manager.weapon_sort_by_max_cost = false;
						imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
					}
					ImGui::SetItemTooltip("Sorts items by minimal cost field that defined in weapon section in ltx file");

					if (ImGui::Checkbox("sort by min fire distance##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_min_fire_distance))
					{
						imgui_spawn_manager.weapon_sort_by_max_cost = false;
						imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_cost = false;
						imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
					}
					ImGui::SetItemTooltip("Sorts items by minimal fire_distance field that defined in weapon section in ltx file");

					if (ImGui::Checkbox("sort by min hit power##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_min_hit_power))
					{
						imgui_spawn_manager.weapon_sort_by_max_cost = false;
						imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
						imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
						imgui_spawn_manager.weapon_sort_by_min_cost = false;
						imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
					}
					ImGui::SetItemTooltip("Sorts items by minimal hit_power field for current game difficulty[%s]that defined in weapon section in ltx file", translate_difficulty(g_SingleGameDifficulty));

					ImGui::InputText("count##IT_InGameSpawnManager", imgui_spawn_manager.weapon_spawn_count, sizeof(imgui_spawn_manager).weapon_spawn_count);
					ImGui::SetItemTooltip("How many items to spawn by one click, from 1 to specified number by user input");

					if (ImGui::Checkbox("spawn on Level", &imgui_spawn_manager.weapon_spawn_on_level))
					{

					}
					ImGui::SetItemTooltip("if this checkbox is enabled that means it will spawn on level not in inventory");

					ImGui::Separator();

					// that has all fields for sorting
					xr_vector<std::pair<std::string_view, CInifile::Sect*>> section_names_sort;
					// lack of some fields for sorting thus can't be included for 'sorting' categorty
					xr_vector<std::pair<std::string_view, CInifile::Sect*>> section_names_unsort;

					for (const auto& pSection : pSettings->sections())
					{
						if (pSection)
						{
							std::string_view name = pSection->Name.c_str();

							if (!name.empty())
							{
								size_t index = name.find("wpn_");
								if (index != std::string_view::npos && index == 0)
								{
									if (pSection->line_exist("hit_power") && pSection->line_exist("cost") && pSection->line_exist("fire_distance"))
									{
										section_names_sort.push_back({ name, pSection });
										++total_count_sort;
									}
									else
									{
										section_names_unsort.push_back({ name, pSection });
										++total_count_unsort;
									}
								}
							}
						}
					}

					ImGui::Text("total [wpn_xxx] sections count: %d", total_count_sort + total_count_unsort);
					ImGui::Text("total [wpn_xxx] sections count (can be sorted): %d", total_count_sort);
					ImGui::Text("total [wpn_xxx] sections count (can't be sorted): %d", total_count_unsort);
					ImGui::Text("current difficulty: %s", translate_difficulty(g_SingleGameDifficulty));
					ImGui::Separator();

					if (imgui_spawn_manager.weapon_sort_by_max_cost)
					{
						std::sort(section_names_sort.begin(), section_names_sort.end(), [](const auto& pair_left, const auto& pair_right)->bool {
							if (pSettings && pair_left.second && pair_right.second)
							{
								const char* pLeftName = pair_left.first.data();
								const char* pRightName = pair_right.first.data();

								float cost_left{};
								float cost_right{};

								if (pSettings->line_exist(pLeftName, "cost"))
								{
									cost_left = pSettings->r_float(pLeftName, "cost");
								}

								if (pSettings->line_exist(pRightName, "cost"))
								{
									cost_right = pSettings->r_float(pRightName, "cost");
								}

								return cost_left > cost_right;
							}

							return false;
							});
					}

					if (imgui_spawn_manager.weapon_sort_by_min_cost)
					{
						std::sort(section_names_sort.begin(), section_names_sort.end(), [](const auto& pair_left, const auto& pair_right)->bool {
							if (pSettings && pair_left.second && pair_right.second)
							{
								const char* pLeftName = pair_left.first.data();
								const char* pRightName = pair_right.first.data();

								float value_left{};
								float value_right{};

								if (pSettings->line_exist(pLeftName, "cost"))
								{
									value_left = pSettings->r_float(pLeftName, "cost");
								}

								if (pSettings->line_exist(pRightName, "cost"))
								{
									value_right = pSettings->r_float(pRightName, "cost");
								}

								return value_left < value_right;
							}

							return false;
							});
					}

					if (imgui_spawn_manager.weapon_sort_by_max_fire_distance)
					{
						std::sort(section_names_sort.begin(), section_names_sort.end(), [](const auto& pair_left, const auto& pair_right)->bool {
							if (pSettings && pair_left.second && pair_right.second)
							{
								const char* pLeftName = pair_left.first.data();
								const char* pRightName = pair_right.first.data();

								float value_left{};
								float value_right{};

								if (pSettings->line_exist(pLeftName, "fire_distance"))
								{
									value_left = pSettings->r_float(pLeftName, "fire_distance");
								}

								if (pSettings->line_exist(pRightName, "fire_distance"))
								{
									value_right = pSettings->r_float(pRightName, "fire_distance");
								}

								return value_left > value_right;
							}

							return false;
							});
					}

					if (imgui_spawn_manager.weapon_sort_by_min_fire_distance)
					{
						std::sort(section_names_sort.begin(), section_names_sort.end(), [](const auto& pair_left, const auto& pair_right)->bool {
							if (pSettings && pair_left.second && pair_right.second)
							{
								const char* pLeftName = pair_left.first.data();
								const char* pRightName = pair_right.first.data();

								float value_left{};
								float value_right{};

								if (pSettings->line_exist(pLeftName, "fire_distance"))
								{
									value_left = pSettings->r_float(pLeftName, "fire_distance");
								}

								if (pSettings->line_exist(pRightName, "fire_distance"))
								{
									value_right = pSettings->r_float(pRightName, "fire_distance");
								}

								return value_left < value_right;
							}

							return false;
							});
					}

					if (imgui_spawn_manager.weapon_sort_by_max_hit_power)
					{
						std::sort(section_names_sort.begin(), section_names_sort.end(), [](const auto& pair_left, const auto& pair_right)->bool {
							if (pSettings && pair_left.second && pair_right.second)
							{
								const char* pLeftName = pair_left.first.data();
								const char* pRightName = pair_right.first.data();

								float value_left{};
								float value_right{};


								if (pSettings->line_exist(pLeftName, "hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(pLeftName, "hit_power");
									string32 buffer{};
									if (g_SingleGameDifficulty == egdNovice)
									{
										_GetItem(*hit_str, 3, buffer);
										value_left = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdStalker)
									{
										_GetItem(*hit_str, 2, buffer);
										value_left = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdVeteran)
									{
										_GetItem(*hit_str, 1, buffer);
										value_left = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdMaster)
									{
										_GetItem(*hit_str, 0, buffer);
										value_left = atof(buffer);
									}
								}

								if (pSettings->line_exist(pRightName, "hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(pRightName, "hit_power");
									string32 buffer{};
									if (g_SingleGameDifficulty == egdNovice)
									{
										_GetItem(*hit_str, 3, buffer);
										value_right = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdStalker)
									{
										_GetItem(*hit_str, 2, buffer);
										value_right = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdVeteran)
									{
										_GetItem(*hit_str, 1, buffer);
										value_right = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdMaster)
									{
										_GetItem(*hit_str, 0, buffer);
										value_right = atof(buffer);
									}
								}

								return value_left > value_right;
							}

							return false;
							});
					}

					if (imgui_spawn_manager.weapon_sort_by_min_hit_power)
					{
						std::sort(section_names_sort.begin(), section_names_sort.end(), [](const auto& pair_left, const auto& pair_right)->bool {
							if (pSettings && pair_left.second && pair_right.second)
							{
								const char* pLeftName = pair_left.first.data();
								const char* pRightName = pair_right.first.data();

								float value_left{};
								float value_right{};

								if (pSettings->line_exist(pLeftName, "hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(pLeftName, "hit_power");
									string32 buffer{};
									if (g_SingleGameDifficulty == egdNovice)
									{
										_GetItem(*hit_str, 3, buffer);
										value_left = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdStalker)
									{
										_GetItem(*hit_str, 2, buffer);
										value_left = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdVeteran)
									{
										_GetItem(*hit_str, 1, buffer);
										value_left = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdMaster)
									{
										_GetItem(*hit_str, 0, buffer);
										value_left = atof(buffer);
									}
								}

								if (pSettings->line_exist(pRightName, "hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(pRightName, "hit_power");
									string32 buffer{};
									if (g_SingleGameDifficulty == egdNovice)
									{
										_GetItem(*hit_str, 3, buffer);
										value_right = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdStalker)
									{
										_GetItem(*hit_str, 2, buffer);
										value_right = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdVeteran)
									{
										_GetItem(*hit_str, 1, buffer);
										value_right = atof(buffer);
									}
									else if (g_SingleGameDifficulty == egdMaster)
									{
										_GetItem(*hit_str, 0, buffer);
										value_right = atof(buffer);
									}
								}

								return value_left < value_right;
							}

							return false;
							});
					}

					size_t number_imgui{};
					for (const auto& data : section_names_sort)
					{
						const auto& section_name = data.first;
						const auto& pSection = data.second;

						if (!section_name.empty() && pSection)
						{
							char imname[kSpawnManagerMaxSectionName]{};
							memcpy_s(imname, sizeof(imname), section_name.data(), section_name.size());

							char index[6]{};
							sprintf_s(index, sizeof(index), "##%zu", number_imgui);
							memcpy_s(imname + section_name.size(), sizeof(imname), index, sizeof(index));

							if (ImGui::Button(imname))
							{
								int count = atoi(imgui_spawn_manager.weapon_spawn_count);

								if (count == 0)
									count = 1;

								if (Console)
								{
									xr_string cmd;

									if (!imgui_spawn_manager.weapon_spawn_on_level)
									{
										cmd += "g_spawn_inv ";
									}
									else
									{
										cmd += "g_spawn ";
									}

									cmd += section_name.data();
									cmd += " ";
									cmd += std::to_string(count);

									execute_console_command_deferred(Console, cmd.c_str());
								}

							}

							if (ImGui::BeginItemTooltip())
							{
								if (pSection->line_exist("inv_name"))
								{
									const char* pTranslateSectionName = pSettings->r_string(section_name.data(), "inv_name");
									const auto& pTranslatedString = g_pStringTable->translate(pTranslateSectionName);
									ImGui::Text("In game name: [%s]", Platform::ANSI_TO_UTF8(pTranslatedString.c_str()).c_str());
								}

								if (pSection->line_exist("cost"))
								{
									const char* pCost = pSettings->r_string(section_name.data(), "cost");
									ImGui::Text("cost: [%s]", pCost);
								}

								if (pSection->line_exist("inv_weight"))
								{
									const char* pInvW = pSettings->r_string(section_name.data(), "inv_weight");
									ImGui::Text("inventory weight: [%s]", pInvW);
								}

								if (pSection->line_exist("hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(section_name.data(), "hit_power");
									float value{};
									string32 buffer{};
									if (g_SingleGameDifficulty == egdNovice)
									{
										value = atof(_GetItem(*hit_str, 3, buffer));
									}
									else if (g_SingleGameDifficulty == egdStalker)
									{
										value = atof(_GetItem(*hit_str, 2, buffer));
									}
									else if (g_SingleGameDifficulty == egdVeteran)
									{
										value = atof(_GetItem(*hit_str, 1, buffer));
									}
									else if (g_SingleGameDifficulty == egdMaster)
									{
										value = atof(_GetItem(*hit_str, 0, buffer));
									}

									ImGui::Text("hit power: [%.2f]", value);
								}

								if (pSection->line_exist("fire_distance"))
								{
									const char* pField = pSettings->r_string(section_name.data(), "fire_distance");
									ImGui::Text("fire distance: [%s]", pField);
								}

								ImGui::EndTooltip();
							}

							ImGui::NewLine();
							++number_imgui;
						}
					}

					ImGui::SeparatorText("Unsorted");

					for (const auto& data : section_names_unsort)
					{
						const auto& section_name = data.first;
						const auto& pSection = data.second;

						if (!section_name.empty() && pSection)
						{
							char imname[kSpawnManagerMaxSectionName]{};
							memcpy_s(imname, sizeof(imname), section_name.data(), section_name.size());

							char index[6]{};
							sprintf_s(index, sizeof(index), "##%zu", number_imgui);
							memcpy_s(imname + section_name.size(), sizeof(imname), index, sizeof(index));

							if (ImGui::Button(imname))
							{

								int count = atoi(imgui_spawn_manager.weapon_spawn_count);

								if (count == 0)
									count = 1;

								if (Console)
								{
									xr_string cmd;

									if (!imgui_spawn_manager.weapon_spawn_on_level)
									{
										cmd += "g_spawn_inv ";
									}
									else
									{
										cmd += "g_spawn ";
									}

									cmd += section_name.data();
									cmd += " ";
									cmd += std::to_string(count);

									execute_console_command_deferred(Console, cmd.c_str());
								}
							}

							if (ImGui::BeginItemTooltip())
							{
								if (pSection->line_exist("inv_name"))
								{
									const char* pTranslateSectionName = pSettings->r_string(section_name.data(), "inv_name");
									const auto& pTranslatedString = g_pStringTable->translate(pTranslateSectionName);
									ImGui::Text("In game name: [%s]", Platform::ANSI_TO_UTF8(pTranslatedString.c_str()).c_str());
								}

								if (pSection->line_exist("cost"))
								{
									const char* pCost = pSettings->r_string(section_name.data(), "cost");
									ImGui::Text("cost: [%s]", pCost);
								}

								if (pSection->line_exist("inv_weight"))
								{
									const char* pInvW = pSettings->r_string(section_name.data(), "inv_weight");
									ImGui::Text("inventory weight: [%s]", pInvW);
								}

								if (pSection->line_exist("hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(section_name.data(), "hit_power");
									float value{};
									string32 buffer{};
									if (g_SingleGameDifficulty == egdNovice)
									{
										value = atof(_GetItem(*hit_str, 3, buffer));
									}
									else if (g_SingleGameDifficulty == egdStalker)
									{
										value = atof(_GetItem(*hit_str, 2, buffer));
									}
									else if (g_SingleGameDifficulty == egdVeteran)
									{
										value = atof(_GetItem(*hit_str, 1, buffer));
									}
									else if (g_SingleGameDifficulty == egdMaster)
									{
										value = atof(_GetItem(*hit_str, 0, buffer));
									}

									ImGui::Text("hit power: [%.2f]", value);
								}

								if (pSection->line_exist("fire_distance"))
								{
									const char* pField = pSettings->r_string(section_name.data(), "fire_distance");
									ImGui::Text("fire distance: [%s]", pField);
								}

								ImGui::EndTooltip();
							}

							ImGui::NewLine();
							++number_imgui;
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

struct 
{
	bool init{};
	u32 inv_cost{};
	float inv_weight{};

}
imgui_weapon_manager;

void RenderWeaponManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	auto draw_item = [](CInventoryItem* pItem) {
		if (pItem)
		{
			if (!imgui_weapon_manager.init)
			{
				imgui_weapon_manager.inv_cost = pItem->Cost();
				imgui_weapon_manager.inv_weight = pItem->Weight();
				imgui_weapon_manager.init = true;
			}


			if (ImGui::CollapsingHeader("Information"))
			{
				if (ImGui::CollapsingHeader("Inventory information"))
				{
					ImGui::Text("Cost: %d", pItem->Cost());
					ImGui::Text("Weight: %f", pItem->Weight());
					ImGui::Text("Name: [%s]", Platform::ANSI_TO_UTF8(pItem->NameItem()).c_str());
					ImGui::Text("Short name: [%s]", Platform::ANSI_TO_UTF8(pItem->NameShort()).c_str());
					ImGui::TextWrapped("Description: [%s]", Platform::ANSI_TO_UTF8(pItem->ItemDescription().c_str()).c_str());
				}

				CShootingObject* pSO = dynamic_cast<CShootingObject*>(pItem);

				if (pSO)
				{
					if (ImGui::CollapsingHeader("Ballistic information"))
					{
						ImGui::Text("Fire distance: %.4f", pSO->getFireDistance());
						ImGui::Text("Bullet speed: %.4f", pSO->getStartBulletSpeed());
						ImGui::Text("RPM: %.4f", pSO->getRPM());
					}


					if (ImGui::CollapsingHeader("Hit information"))
					{
						ImGui::Text("Hit impulse: %.4f", pSO->getHitImpulse());
						const auto& hit_power = pSO->getHitPower();
						ImGui::Text("Hit power: %.4f %.4f %.4f %.4f", hit_power.x, hit_power.y, hit_power.z, hit_power.z, hit_power.w);
						const auto& hit_power_critical = pSO->getHitPowerCritical();
						ImGui::Text("Hit power critical: %.4f %.4f %.4f %.4f", hit_power_critical.x, hit_power_critical.y, hit_power_critical.z, hit_power_critical.w);
					}
				}

				CWeapon* pWeapon = dynamic_cast<CWeapon*>(pItem);
				if (pWeapon && pSO)
				{
					if (ImGui::CollapsingHeader("Ammunition"))
					{
						ImGui::Text("Magazine size: %d", pWeapon->GetAmmoMagSize());

						xr_string ammos = "Ammo: ";
						for (const auto& str : pWeapon->getAmmoTypes())
						{
							ammos += str.c_str();
							ammos += ',';
						}

						ammos.erase(ammos.rfind(','));

						ImGui::TextWrapped(ammos.c_str());
					}

					if (ImGui::CollapsingHeader("Dispersion"))
					{
						ImGui::Text("Fire dispersion base: %.4f", pSO->getFireDispersionBase());
						ImGui::Text("Control inertion factor: %.4f", pItem->GetControlInertionFactor());
						ImGui::Text("Crosshair inertion: %.4f", pWeapon->GetCrosshairInertion());
					}

					if (ImGui::CollapsingHeader("Recoil"))
					{
						const auto& cam_recoil = pWeapon->getCameraRecoil();
						ImGui::Text("Camera return: %s", cam_recoil.ReturnMode ? "enabled" : "disabled");
						ImGui::Text("Camera relax speed: %.4f", cam_recoil.RelaxSpeed);
						ImGui::Text("Camera relax speed ai: %.4f", cam_recoil.RelaxSpeed_AI);
						ImGui::Text("Camera dispersion: %.4f", cam_recoil.Dispersion);
						ImGui::Text("Camera dispersion inc: %.4f", cam_recoil.DispersionInc);
						ImGui::Text("Camera dispersion frac: %.4f", cam_recoil.DispersionFrac);
						if (ImGui::BeginItemTooltip())
						{
							ImGui::SetItemTooltip("Where gun will be pointed that described by law cam_dispersion*cam_dispersion_frac +- cam_dispersion*(1-cam_dispersion_frac)");
							ImGui::EndTooltip();
						}

						ImGui::Text("Camera max angle vertical: %.4f", cam_recoil.MaxAngleVert);
						ImGui::Text("Camera max angle horizontal: %.4f", cam_recoil.MaxAngleHorz);
						ImGui::Text("Camera step angle horizontal: %.4f", cam_recoil.StepAngleHorz);

						ImGui::SeparatorText("Zoom");
						const auto& zoom_cam_recoil = pWeapon->getCameraZoomRecoil();
						ImGui::Text("Zoom camera relax speed: %.4f", zoom_cam_recoil.RelaxSpeed);
						ImGui::Text("Zoom camera relax speed ai: %.4f", zoom_cam_recoil.RelaxSpeed_AI);
						ImGui::Text("Zoom cam dispersion: %.4f", zoom_cam_recoil.Dispersion);
						ImGui::Text("Zoom cam dispersion inc: %.4f", zoom_cam_recoil.DispersionInc);
						ImGui::Text("Zoom cam dispersion frac: %.4f", zoom_cam_recoil.DispersionFrac);
						if (ImGui::BeginItemTooltip())
						{
							ImGui::SetItemTooltip("Where gun will be pointed that described by law cam_dispersion*cam_dispersion_frac +- cam_dispersion*(1-cam_dispersion_frac)");
							ImGui::EndTooltip();
						}
						ImGui::Text("Zoom cam max angle vertical: %.4f", zoom_cam_recoil.MaxAngleVert);
						ImGui::Text("Zoom cam max angle horizontal: %.4f", zoom_cam_recoil.MaxAngleHorz);
						ImGui::Text("Zoom step angle horizontal: %.4f", zoom_cam_recoil.StepAngleHorz);

					}

				}
			}

			if (ImGui::CollapsingHeader("Editing"))
			{

			}
		}
	};

	if (ImGui::Begin("Weapon Manager", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponManager)]))
	{
		if (ImGui::BeginTabBar("##TB_InGameWeaponManager"))
		{
			if (ImGui::BeginTabItem("Slot 2 (INV_SLOT_2)##TB_InGameWeaponManager"))
			{
				CActor* pActor = smart_cast<CActor*>(Level().CurrentEntity());

				if (pActor)
				{
					CInventoryItem* pItem = pActor->inventory().ItemFromSlot(INV_SLOT_2); 

					draw_item(pItem);
				}

				ImGui::EndTabItem();
			}


			ImGui::EndTabBar();
		}


		ImGui::End();
	}
}

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

void RegisterImGuiInGame()
{
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Subscribe("Time Manager", CImGuiManager::ERenderPriority::eMedium, RenderTimeManagerWindow);
		CImGuiManager::Instance().Subscribe("Spawn Manager", CImGuiManager::ERenderPriority::eMedium, RenderSpawnManagerWindow);
		CImGuiManager::Instance().Subscribe("Weapon Manager", CImGuiManager::ERenderPriority::eMedium, RenderWeaponManagerWindow);
		CImGuiManager::Instance().Subscribe("Search Manager", CImGuiManager::ERenderPriority::eMedium, RenderSearchManagerWindow);

		InitImGuiCLSIDInGame();

		imgui_search_manager.init();
	}
}

void block_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().block_action(cmd);
}

bool is_block_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return false;
	}

	return Level().is_block_action(cmd);
}

void unblock_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().unblock_action(cmd);
}

void press_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().IR_OnKeyboardPress(cmd);
}

void hold_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().IR_OnKeyboardHold(cmd);
}

void release_action_script(int cmd) {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().IR_OnKeyboardRelease(cmd);
}

void LockActorWithCameraRotation_script() {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().LockActorWithCameraRotation();
}

void UnLockActor_script() {
	if (g_pGameLevel == nullptr) {
		return;
	}

	Level().UnLockActor();
}

LPCSTR command_line()
{
	return		(Core.Params);
}
bool IsDynamicMusic()
{
	return !!psActorFlags.test(AF_DYNAMIC_MUSIC);
}

bool IsImportantSave()
{
	return !!psActorFlags.test(AF_IMPORTANT_SAVE);
}

float get_compass_direction()
{
	float compass_angle, p;
	Device.vCameraDirection.getHP(compass_angle, p);

	return compass_angle;
}

#ifdef DEBUG
void check_object(CScriptGameObject* object)
{
	try {
		Msg("check_object %s", object->Name());
	}
	catch (...) {
		object = object;
	}
}


CScriptGameObject* tpfGetActor()
{
	static bool first_time = true;
	if (first_time)
		ai().script_engine().script_log(eLuaMessageTypeError, "Do not use level.actor function!");
	first_time = false;

	CActor* l_tpActor = smart_cast<CActor*>(Level().CurrentEntity());
	if (l_tpActor)
		return	(smart_cast<CGameObject*>(l_tpActor)->lua_game_object());
	else
		return	(0);
}

CScriptGameObject* get_object_by_name(LPCSTR caObjectName)
{
	static bool first_time = true;
	if (first_time)
		ai().script_engine().script_log(eLuaMessageTypeError, "Do not use level.object function!");
	first_time = false;

	CGameObject* l_tpGameObject = smart_cast<CGameObject*>(Level().Objects.FindObjectByName(caObjectName));
	if (l_tpGameObject)
		return		(l_tpGameObject->lua_game_object());
	else
		return		(0);
}
#endif

CScriptGameObject* get_object_by_id(u16 id)
{
	CGameObject* pGameObject = smart_cast<CGameObject*>(Level().Objects.net_Find(id));
	if (!pGameObject)
		return nullptr;

	return pGameObject->lua_game_object();
}

LPCSTR get_past_wdesc()
{
	return			(g_pGamePersistent->Environment().Current[0] ? g_pGamePersistent->Environment().Current[0]->m_identifier.c_str() : "null");
}

LPCSTR get_next_wdesc()
{
	return			(g_pGamePersistent->Environment().Current[1] ? g_pGamePersistent->Environment().Current[1]->m_identifier.c_str() : "null");
}

float get_past_wdesc_execution_time()
{
	return			(g_pGamePersistent->Environment().Current[0] ? g_pGamePersistent->Environment().Current[0]->exec_time : -1.f);
}

float get_next_wdesc_execution_time()
{
	return			(g_pGamePersistent->Environment().Current[1] ? g_pGamePersistent->Environment().Current[1]->exec_time : -1.f);
}

float get_weather_game_time()
{
	return			(&g_pGamePersistent->Environment() ? g_pGamePersistent->Environment().GetGameTime() : -1.f);
}

void set_past_wdesc(LPCSTR WeatherSection)
{
	if (&g_pGamePersistent->Environment())
	{
		g_pGamePersistent->Environment().SetEnvDesc(WeatherSection, g_pGamePersistent->Environment().Current[0]);
	}
}

void set_next_wdesc(LPCSTR WeatherSection)
{
	if (&g_pGamePersistent->Environment())
	{
		g_pGamePersistent->Environment().SetEnvDesc(WeatherSection, g_pGamePersistent->Environment().Current[1]);
	}
}

LPCSTR get_weather()
{
	return			(*g_pGamePersistent->Environment().GetWeather());
}

void set_weather(LPCSTR weather_name, bool forced)
{
	g_pGamePersistent->Environment().SetWeather(weather_name, forced);
}

bool set_weather_fx(LPCSTR weather_name)
{
	return		(g_pGamePersistent->Environment().SetWeatherFX(weather_name));
}

bool start_weather_fx_from_time(LPCSTR weather_name, float time)
{
	return		(g_pGamePersistent->Environment().StartWeatherFXFromTime(weather_name, time));
}

bool is_wfx_playing()
{
	return			(g_pGamePersistent->Environment().IsWFXPlaying());
}

float get_wfx_time()
{
	return			(g_pGamePersistent->Environment().wfx_time);
}

void stop_weather_fx()
{
	g_pGamePersistent->Environment().StopWFX();
}

void set_time_factor(float time_factor)
{
	if (!OnServer())
		return;

	Level().Server->game->SetGameTimeFactor(time_factor);
}

float get_time_factor()
{
	return			(Level().GetGameTimeFactor());
}

void set_global_time_factor(float tf) {
	if (!OnServer())
		return;

	Device.time_factor(tf);
}

float get_global_time_factor() { return (Device.time_factor()); }

void set_game_difficulty(ESingleGameDifficulty dif)
{
	g_SingleGameDifficulty = dif;
	game_cl_Single* game = smart_cast<game_cl_Single*>(Level().game); VERIFY(game);
	game->OnDifficultyChanged();
}
ESingleGameDifficulty get_game_difficulty()
{
	return g_SingleGameDifficulty;
}

u32 get_time_days()
{
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time((g_pGameLevel && Level().game) ? Level().GetGameTime() : ai().alife().time_manager().game_time(), year, month, day, hours, mins, secs, milisecs);
	return			day;
}

u32 get_time_hours()
{
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time((g_pGameLevel && Level().game) ? Level().GetGameTime() : ai().alife().time_manager().game_time(), year, month, day, hours, mins, secs, milisecs);
	return			hours;
}

u32 get_time_minutes()
{
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time((g_pGameLevel && Level().game) ? Level().GetGameTime() : ai().alife().time_manager().game_time(), year, month, day, hours, mins, secs, milisecs);
	return			mins;
}

void change_game_time(u32 days, u32 hours, u32 mins)
{
	game_sv_Single* tpGame = smart_cast<game_sv_Single*>(Level().Server->game);
	if (tpGame && ai().get_alife())
	{
		u32 value = days * 86400 + hours * 3600 + mins * 60;
		float fValue = static_cast<float> (value);
		value *= 1000;//msec		
		g_pGamePersistent->Environment().ChangeGameTime(fValue);
		tpGame->alife().time_manager().change_game_time(value);
	}
}

float high_cover_in_direction(u32 level_vertex_id, const Fvector& direction)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return 0.0f;
	}

	float			y, p;
	direction.getHP(y, p);
	return			(ai().level_graph().high_cover_in_direction(y, level_vertex_id));
}

float low_cover_in_direction(u32 level_vertex_id, const Fvector& direction)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return 0.0f;
	}

	float			y, p;
	direction.getHP(y, p);
	return			(ai().level_graph().low_cover_in_direction(y, level_vertex_id));
}

float rain_factor()
{
	return			(g_pGamePersistent->Environment().CurrentEnv->rain_density);
}

u32	vertex_in_direction(u32 level_vertex_id, Fvector direction, float max_distance)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return u32(-1);
	}

	direction.normalize_safe();
	direction.mul(max_distance);
	Fvector			start_position = ai().level_graph().vertex_position(level_vertex_id);
	Fvector			finish_position = Fvector(start_position).add(direction);
	u32				result_ = u32(-1);
	ai().level_graph().farthest_vertex_in_direction(level_vertex_id, start_position, finish_position, result_, 0);
	return			(ai().level_graph().valid_vertex_id(result_) ? result_ : level_vertex_id);
}

Fvector vertex_position(u32 level_vertex_id)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id)) {
		return Fvector{};
	}
	return			(ai().level_graph().vertex_position(level_vertex_id));
}

void map_add_object_spot(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type, id);
	if (xr_strlen(text))
	{
		ml->SetHint(text);
	}
}

void map_add_object_spot_ser(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().AddMapLocation(spot_type, id);
	if (xr_strlen(text))
		ml->SetHint(text);

	ml->SetSerializable(true);
}

void map_change_spot_hint(u16 id, LPCSTR spot_type, LPCSTR text)
{
	CMapLocation* ml = Level().MapManager().GetMapLocation(spot_type, id);
	if (!ml)				return;
	ml->SetHint(text);
}

void map_remove_object_spot(u16 id, LPCSTR spot_type)
{
	Level().MapManager().RemoveMapLocation(spot_type, id);
}

u16 map_has_object_spot(u16 id, LPCSTR spot_type)
{
	return Level().MapManager().HasMapLocation(spot_type, id);
}

bool patrol_path_exists(LPCSTR patrol_path)
{
	return		(!!ai().patrol_paths().path(patrol_path, true));
}

LPCSTR get_name()
{
	return		(*Level().name());
}

void prefetch_sound(LPCSTR name)
{
	Level().PrefetchSound(name);
}


CClientSpawnManager& get_client_spawn_manager()
{
	return		(Level().client_spawn_manager());
}
/*
void start_stop_menu(CUIDialogWnd* pDialog, bool bDoHideIndicators)
{
	if(pDialog->IsShown())
		pDialog->HideDialog();
	else
		pDialog->ShowDialog(bDoHideIndicators);
}
*/

void add_dialog_to_render(CUIDialogWnd* pDialog)
{
	CurrentGameUI()->AddDialogToRender(pDialog);
}

void remove_dialog_to_render(CUIDialogWnd* pDialog)
{
	CurrentGameUI()->RemoveDialogToRender(pDialog);
}

void hide_indicators()
{
	if (CurrentGameUI())
	{
		CurrentGameUI()->HideShownDialogs();
		CurrentGameUI()->ShowGameIndicators(false);
		CurrentGameUI()->ShowCrosshair(false);
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, TRUE);
}

void hide_indicators_safe()
{
	if (CurrentGameUI())
	{
		CurrentGameUI()->ShowGameIndicators(false);
		CurrentGameUI()->ShowCrosshair(false);

		CurrentGameUI()->OnExternalHideIndicators();
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, TRUE);
}

void show_indicators()
{
	if (CurrentGameUI())
	{
		CurrentGameUI()->ShowGameIndicators(true);
		CurrentGameUI()->ShowCrosshair(true);
	}
	psActorFlags.set(AF_DISABLE_CONDITION_TEST, FALSE);
}

void show_weapon(bool b)
{
	psHUD_Flags.set(HUD_WEAPON_RT2, b);
}

bool is_level_present()
{
	return (!!g_pGameLevel);
}

void add_call(const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{
	luabind::functor<bool>		_condition = condition;
	luabind::functor<void>		_action = action;
	CPHScriptCondition* c = new CPHScriptCondition(_condition);
	CPHScriptAction* a = new CPHScriptAction(_action);
	Level().ph_commander_scripts().add_call(c, a);
}

void remove_call(const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{
	CPHScriptCondition	c(condition);
	CPHScriptAction		a(action);
	Level().ph_commander_scripts().remove_call(&c, &a);
}

void add_call(const luabind::object& lua_object, LPCSTR condition, LPCSTR action)
{
	//	try{	
	//		CPHScriptObjectCondition	*c=new CPHScriptObjectCondition(lua_object,condition);
	//		CPHScriptObjectAction		*a=new CPHScriptObjectAction(lua_object,action);
	luabind::functor<bool>		_condition = object_cast<luabind::functor<bool>>(lua_object[condition]);
	luabind::functor<void>		_action = object_cast<luabind::functor<void>>(lua_object[action]);
	CPHScriptObjectConditionN* c = new CPHScriptObjectConditionN(lua_object, _condition);
	CPHScriptObjectActionN* a = new CPHScriptObjectActionN(lua_object, _action);
	Level().ph_commander_scripts().add_call_unique(c, c, a, a);
	//	}
	//	catch(...)
	//	{
	//		Msg("add_call excepted!!");
	//	}
}

void remove_call(const luabind::object& lua_object, LPCSTR condition, LPCSTR action)
{
	CPHScriptObjectCondition	c(lua_object, condition);
	CPHScriptObjectAction		a(lua_object, action);
	Level().ph_commander_scripts().remove_call(&c, &a);
}

void add_call(const luabind::object& lua_object, const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{

	CPHScriptObjectConditionN* c = new CPHScriptObjectConditionN(lua_object, condition);
	CPHScriptObjectActionN* a = new CPHScriptObjectActionN(lua_object, action);
	Level().ph_commander_scripts().add_call(c, a);
}

void remove_call(const luabind::object& lua_object, const luabind::functor<bool>& condition, const luabind::functor<void>& action)
{
	CPHScriptObjectConditionN	c(lua_object, condition);
	CPHScriptObjectActionN		a(lua_object, action);
	Level().ph_commander_scripts().remove_call(&c, &a);
}

void remove_calls_for_object(const luabind::object& lua_object)
{
	CPHSriptReqObjComparer c(lua_object);
	Level().ph_commander_scripts().remove_calls(&c);
}

cphysics_world_scripted* physics_world_scripted()
{
	return	get_script_wrapper<cphysics_world_scripted>(*physics_world());
}
CEnvironment* environment()
{
	return		(g_pGamePersistent->pEnvironment);
}

CEnvDescriptor* current_environment(CEnvironment* self_)
{
	return		(self_->CurrentEnv);
}
extern bool g_bDisableAllInput;
void disable_input()
{
	g_bDisableAllInput = true;
#ifdef DEBUG
	Msg("input disabled");
#endif // #ifdef DEBUG
}
void enable_input()
{
	g_bDisableAllInput = false;
#ifdef DEBUG
	Msg("input enabled");
#endif // #ifdef DEBUG
}

void spawn_phantom(const Fvector& position)
{
	Level().spawn_item("m_phantom", position, u32(-1), u16(-1), false);
}

Fbox get_bounding_volume()
{
	return Level().ObjectSpace.GetBoundingVolume();
}

void iterate_sounds(LPCSTR prefix, u32 max_count, const CScriptCallbackEx<void>& callback)
{
	for (int j = 0, N = _GetItemCount(prefix); j < N; ++j) {
		string_path					fn, s;
		LPSTR						S = (LPSTR)&s;
		_GetItem(prefix, j, s);
		if (FS.exist(fn, "$game_sounds$", S, ".ogg"))
			callback(prefix);

		for (u32 i = 0; i < max_count; ++i)
		{
			string_path					name;
			xr_sprintf(name, "%s%d", S, i);
			if (FS.exist(fn, "$game_sounds$", name, ".ogg"))
				callback(name);
		}
	}
}

void iterate_sounds1(LPCSTR prefix, u32 max_count, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set(functor);
	iterate_sounds(prefix, max_count, temp);
}

void iterate_sounds2(LPCSTR prefix, u32 max_count, luabind::object object, luabind::functor<void> functor)
{
	CScriptCallbackEx<void>		temp;
	temp.set(functor, object);
	iterate_sounds(prefix, max_count, temp);
}

#include "actoreffector.h"
float add_cam_effector(LPCSTR fn, int id, bool cyclic, LPCSTR cb_func)
{
	CAnimatorCamEffectorScriptCB* e = new CAnimatorCamEffectorScriptCB(cb_func);
	e->SetType((ECamEffectorType)id);
	e->SetCyclic(cyclic);
	e->Start(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

float add_cam_effector2(LPCSTR fn, int id, bool cyclic, LPCSTR cb_func, float cam_fov)
{
	CAnimatorCamEffectorScriptCB* e = new CAnimatorCamEffectorScriptCB(cb_func);
	e->m_bAbsolutePositioning = true;
	e->m_fov = cam_fov;
	e->SetType((ECamEffectorType)id);
	e->SetCyclic(cyclic);
	e->Start(fn);
	Actor()->Cameras().AddCamEffector(e);
	return						e->GetAnimatorLength();
}

void remove_cam_effector(int id)
{
	Actor()->Cameras().RemoveCamEffector((ECamEffectorType)id);
}

float get_snd_volume()
{
	return psSoundVFactor;
}

void set_snd_volume(float v)
{
	psSoundVFactor = v;
	clamp(psSoundVFactor, 0.0f, 1.0f);
}
#include "actor_statistic_mgr.h"
void add_actor_points(LPCSTR sect, LPCSTR detail_key, int cnt, int pts)
{
	return Actor()->StatisticMgr().AddPoints(sect, detail_key, cnt, pts);
}

void add_actor_points_str(LPCSTR sect, LPCSTR detail_key, LPCSTR str_value)
{
	return Actor()->StatisticMgr().AddPoints(sect, detail_key, str_value);
}

int get_actor_points(LPCSTR sect)
{
	return Actor()->StatisticMgr().GetSectionPoints(sect);
}



#include "ActorEffector.h"
void add_complex_effector(LPCSTR section, int id)
{
	AddEffector(Actor(), id, section);
}

void remove_complex_effector(int id)
{
	RemoveEffector(Actor(), id);
}

#include "postprocessanimator.h"
void add_pp_effector(LPCSTR fn, int id, bool cyclic)
{
	CPostprocessAnimator* pp = new CPostprocessAnimator(id, cyclic);
	pp->Load(fn);
	Actor()->Cameras().AddPPEffector(pp);
}

void remove_pp_effector(int id)
{
	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp) pp->Stop(1.0f);

}

void set_pp_effector_factor(int id, float f, float f_sp)
{
	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp) pp->SetDesiredFactor(f, f_sp);
}

void set_pp_effector_factor2(int id, float f)
{
	CPostprocessAnimator* pp = smart_cast<CPostprocessAnimator*>(Actor()->Cameras().GetPPEffector((EEffectorPPType)id));

	if (pp) pp->SetCurrentFactor(f);
}

#include "relation_registry.h"

int g_community_goodwill(LPCSTR _community, int _entity_id)
{
	CHARACTER_COMMUNITY c;
	c.set(_community);

	return RELATION_REGISTRY().GetCommunityGoodwill(c.index(), u16(_entity_id));
}

void g_set_community_goodwill(LPCSTR _community, int _entity_id, int val)
{
	CHARACTER_COMMUNITY	c;
	c.set(_community);
	RELATION_REGISTRY().SetCommunityGoodwill(c.index(), u16(_entity_id), val);
}

void g_change_community_goodwill(LPCSTR _community, int _entity_id, int val)
{
	CHARACTER_COMMUNITY	c;
	c.set(_community);
	RELATION_REGISTRY().ChangeCommunityGoodwill(c.index(), u16(_entity_id), val);
}

int g_get_community_relation(LPCSTR comm_from, LPCSTR comm_to)
{
	CHARACTER_COMMUNITY	community_from;
	community_from.set(comm_from);
	CHARACTER_COMMUNITY	community_to;
	community_to.set(comm_to);

	return RELATION_REGISTRY().GetCommunityRelation(community_from.index(), community_to.index());
}

void g_set_community_relation(LPCSTR comm_from, LPCSTR comm_to, int value)
{
	CHARACTER_COMMUNITY	community_from;
	community_from.set(comm_from);
	CHARACTER_COMMUNITY	community_to;
	community_to.set(comm_to);

	RELATION_REGISTRY().SetCommunityRelation(community_from.index(), community_to.index(), value);
}

int g_get_general_goodwill_between(u16 from, u16 to)
{
	CHARACTER_GOODWILL presonal_goodwill = RELATION_REGISTRY().GetGoodwill(from, to); VERIFY(presonal_goodwill != NO_GOODWILL);

	CSE_ALifeTraderAbstract* from_obj = smart_cast<CSE_ALifeTraderAbstract*>(ai().alife().objects().object(from));
	CSE_ALifeTraderAbstract* to_obj = smart_cast<CSE_ALifeTraderAbstract*>(ai().alife().objects().object(to));

	if (!from_obj || !to_obj) {
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "RELATION_REGISTRY::get_general_goodwill_between  : cannot convert obj to CSE_ALifeTraderAbstract!");
		return (0);
	}
	CHARACTER_GOODWILL community_to_obj_goodwill = RELATION_REGISTRY().GetCommunityGoodwill(from_obj->Community(), to);
	CHARACTER_GOODWILL community_to_community_goodwill = RELATION_REGISTRY().GetCommunityRelation(from_obj->Community(), to_obj->Community());

	return presonal_goodwill + community_to_obj_goodwill + community_to_community_goodwill;
}

u32 vertex_id(Fvector position)
{
	return	(ai().level_graph().vertex_id(position));
}

u32 render_get_dx_level()
{
	return ::Render->get_dx_level();
}

CUISequencer* g_tutorial = nullptr;
CUISequencer* g_tutorial2 = nullptr;

void start_tutorial(LPCSTR name)
{
	if (load_screen_renderer.IsActive()) {
		return;
	}

	if (g_tutorial) {
		VERIFY(!g_tutorial2);
		g_tutorial2 = g_tutorial;
	};

	g_tutorial = new CUISequencer();
	g_tutorial->Start(name);
	if (g_tutorial2)
		g_tutorial->m_pStoredInputReceiver = g_tutorial2->m_pStoredInputReceiver;

}

void stop_tutorial()
{
	if (g_tutorial)
		g_tutorial->Stop();
}

LPCSTR translate_string(LPCSTR str)
{
	return *g_pStringTable->translate(str);
}

bool has_active_tutotial()
{
	return (g_tutorial != nullptr);
}

bool valid_vertex_id(u32 level_vertex_id) {
	return ai().level_graph().valid_vertex_id(level_vertex_id);
}

bool is_accessible_vertex_id(u32 level_vertex_id) {
	return ai().level_graph().is_accessible(level_vertex_id);
}

void disable_vertex(u32 vertex_id) {
	ai().level_graph().set_mask(vertex_id);
}

void enable_vertex(u32 vertex_id) {
	ai().level_graph().clear_mask(vertex_id);
}

bool is_dedicated()
{
	return g_dedicated_server;
}

//ability to update level netpacket
void g_send(NET_Packet& P, bool bReliable = 0, bool bSequential = 1, bool bHighPriority = 0, bool bSendImmediately = 0)
{
	Level().Send(P, net_flags(bReliable, bSequential, bHighPriority, bSendImmediately));
}

void u_event_gen(NET_Packet& P, u32 _event, u32 _dest)
{
	CGameObject::u_EventGen(P, _event, _dest);
}

void u_event_send(NET_Packet& P)
{
	CGameObject::u_EventSend(P);
}

//can spawn entities like bolts, phantoms, ammo, etc. which normally crash when using alife():create()
void spawn_section(LPCSTR sSection, Fvector3 vPosition, u32 LevelVertexID, u16 ParentID, bool bReturnItem = false)
{
	Level().spawn_item(sSection, vPosition, LevelVertexID, ParentID, bReturnItem);
}

#include "HUDManager.h"
//ability to get the target game_object at crosshair
CScriptGameObject* g_get_target_obj()
{
	collide::rq_result& RQ = HUD().GetCurrentRayQuery();
	if (RQ.O)
	{
		CGameObject* game_object = static_cast<CGameObject*>(RQ.O);
		if (game_object)
			return game_object->lua_game_object();
	}
	return (0);
}

float g_get_target_dist()
{
	collide::rq_result& RQ = HUD().GetCurrentRayQuery();
	if (RQ.range)
		return RQ.range;
	return (0);
}

u32 g_get_target_element()
{
	collide::rq_result& RQ = HUD().GetCurrentRayQuery();
	if (RQ.element)
	{
		return RQ.element;
	}
	return (0);
}

u8 get_active_cam()
{
	CActor* actor = smart_cast<CActor*>(Level().CurrentViewEntity());
	if (actor)
		return (u8)actor->active_cam();

	return 255;
}

void LevelPressAction(EGameActions cmd)
{
	Level().IR_OnKeyboardPress(cmd);
}

void LevelReleaseAction(EGameActions cmd)
{
	Level().IR_OnKeyboardRelease(cmd);
}

void LevelHoldAction(EGameActions cmd)
{
	Level().IR_OnKeyboardHold(cmd);
}

bool valid_vertex(u32 level_vertex_id)
{
	return ai().level_graph().valid_vertex_id(level_vertex_id);
}

xrTime get_start_time()
{
	return (xrTime(Level().GetStartGameTime()));
}

CScriptGameObject* get_view_entity_script()
{
	CGameObject* pGameObject = smart_cast<CGameObject*>(Level().CurrentViewEntity());
	if (!pGameObject)
		return (0);

	return pGameObject->lua_game_object();
}

void set_view_entity_script(CScriptGameObject* go)
{
	CObject* o = smart_cast<CObject*>(&go->object());
	if (o)
		Level().SetViewEntity(o);
}

void set_active_cam(u8 mode)
{
	CActor* actor = smart_cast<CActor*>(Level().CurrentViewEntity());
	if (actor && mode <= ACTOR_DEFS::EActorCameras::eacMaxCam)
		actor->cam_Set((ACTOR_DEFS::EActorCameras)mode);
}

namespace level_nearest
{
	xr_vector<CObject*> ObjectList;
	void Set(float Radius, const Fvector& Pos)
	{
		ObjectList.clear();
		g_pGameLevel->ObjectSpace.GetNearest(ObjectList, Pos, Radius, nullptr);
	}

	u32 Size()
	{
		return (u32)ObjectList.size();
	}

	CScriptGameObject* Get(int Idx)
	{
		if (Idx > (int)ObjectList.size())
			return nullptr;

		CGameObject* pObj = smart_cast<CGameObject*>(ObjectList[Idx]);
		return pObj->lua_game_object();
	}
}

#pragma optimize("s",on)
void CLevel::script_register(lua_State* L)
{
	class_<CEnvDescriptor>("CEnvDescriptor")
		.def_readonly("fog_density", &CEnvDescriptor::fog_density)
		.def_readonly("far_plane", &CEnvDescriptor::far_plane),

		class_<CEnvironment>("CEnvironment")
		.def("current", current_environment);

	module(L, "level")
		[
			// obsolete\deprecated
			def("object_by_id", get_object_by_id),
#ifdef DEBUG
				def("debug_object", get_object_by_name),
				def("debug_actor", tpfGetActor),
				def("check_object", check_object),
#endif

				def("get_weather", get_weather),
				def("set_weather", set_weather),
				def("set_weather_fx", set_weather_fx),
				def("set_past_weather", set_past_wdesc),
				def("set_next_weather", set_next_wdesc),
				def("get_weather_game_time", get_weather_game_time),
				def("get_past_wdesc_execution_time", get_past_wdesc_execution_time),
				def("get_next_wdesc_execution_time", get_next_wdesc_execution_time),
				def("get_past_weather", get_past_wdesc),
				def("get_next_weather", get_next_wdesc),
				def("start_weather_fx_from_time", start_weather_fx_from_time),
				def("is_wfx_playing", is_wfx_playing),
				def("get_wfx_time", get_wfx_time),
				def("stop_weather_fx", stop_weather_fx),

				def("environment", environment),

				def("set_time_factor", set_time_factor),
				def("get_time_factor", get_time_factor),

				def("set_global_time_factor", &set_global_time_factor),
				def("get_global_time_factor", &get_global_time_factor),

				def("set_game_difficulty", set_game_difficulty),
				def("get_game_difficulty", get_game_difficulty),

				def("get_time_days", get_time_days),
				def("get_time_hours", get_time_hours),
				def("get_time_minutes", get_time_minutes),
				def("change_game_time", change_game_time),

				def("high_cover_in_direction", high_cover_in_direction),
				def("low_cover_in_direction", low_cover_in_direction),
				def("vertex_in_direction", vertex_in_direction),
				def("rain_factor", rain_factor),
				def("patrol_path_exists", patrol_path_exists),
				def("vertex_position", vertex_position),
				def("name", get_name),
				def("prefetch_sound", prefetch_sound),

				def("client_spawn_manager", get_client_spawn_manager),

				def("map_add_object_spot_ser", map_add_object_spot_ser),
				def("map_add_object_spot", map_add_object_spot),
				//-		def("map_add_object_spot_complex",		map_add_object_spot_complex),
				def("map_remove_object_spot", map_remove_object_spot),
				def("map_has_object_spot", map_has_object_spot),
				def("map_change_spot_hint", map_change_spot_hint),

				def("add_dialog_to_render", add_dialog_to_render),
				def("remove_dialog_to_render", remove_dialog_to_render),
				def("hide_indicators", hide_indicators),
				def("hide_indicators_safe", hide_indicators_safe),

				def("show_indicators", show_indicators),
				def("show_weapon", show_weapon),
				def("add_call", ((void (*) (const luabind::functor<bool> &, const luabind::functor<void> &)) & add_call)),
				def("add_call", ((void (*) (const luabind::object&, const luabind::functor<bool> &, const luabind::functor<void> &)) & add_call)),
				def("add_call", ((void (*) (const luabind::object&, LPCSTR, LPCSTR)) & add_call)),
				def("remove_call", ((void (*) (const luabind::functor<bool> &, const luabind::functor<void> &)) & remove_call)),
				def("remove_call", ((void (*) (const luabind::object&, const luabind::functor<bool> &, const luabind::functor<void> &)) & remove_call)),
				def("remove_call", ((void (*) (const luabind::object&, LPCSTR, LPCSTR)) & remove_call)),
				def("remove_calls_for_object", remove_calls_for_object),
				def("present", is_level_present),
				def("disable_input", disable_input),
				def("enable_input", enable_input),
				def("spawn_phantom", spawn_phantom),

				def("get_bounding_volume", get_bounding_volume),

				def("iterate_sounds", &iterate_sounds1),
				def("iterate_sounds", &iterate_sounds2),
				def("physics_world", &physics_world_scripted),
				def("get_snd_volume", &get_snd_volume),
				def("set_snd_volume", &set_snd_volume),
				def("add_cam_effector", &add_cam_effector),
				def("add_cam_effector2", &add_cam_effector2),
				def("remove_cam_effector", &remove_cam_effector),
				def("add_pp_effector", &add_pp_effector),
				def("set_pp_effector_factor", &set_pp_effector_factor),
				def("set_pp_effector_factor", &set_pp_effector_factor2),
				def("remove_pp_effector", &remove_pp_effector),
				def("get_compass_direction", &get_compass_direction),

				def("add_complex_effector", &add_complex_effector),
				def("remove_complex_effector", &remove_complex_effector),

				def("valid_vertex_id", valid_vertex_id),
				def("is_accessible_vertex_id", is_accessible_vertex_id),
				def("disable_vertex", disable_vertex),
				def("enable_vertex", enable_vertex),
				def("vertex_id", &vertex_id),

				def("game_id", &GameID),

				def("block_action", &block_action_script),
				def("is_block_action", &is_block_action_script),
				def("unblock_action", &unblock_action_script),
				def("press_action", &press_action_script),
				def("hold_action", &hold_action_script),
				def("release_action", &release_action_script),
				def("lock_actor", &LockActorWithCameraRotation_script),
				def("unlock_actor", &UnLockActor_script),

				def("u_event_gen", &u_event_gen), //Send events via packet
				def("u_event_send", &u_event_send),
				def("send", &g_send), //allow the ability to send netpacket to level
				def("get_target_obj", &g_get_target_obj), //intentionally named to what is in xray extensions
				def("get_target_dist", &g_get_target_dist),
				def("press_action", &LevelPressAction),
				def("release_action", &LevelReleaseAction),
				def("hold_action", &LevelHoldAction),
				def("get_target_element", &g_get_target_element), //Can get bone cursor is targetting
				def("get_view_entity", &get_view_entity_script),
				def("set_view_entity", &set_view_entity_script),
				def("spawn_item", &spawn_section),
				def("get_active_cam", &get_active_cam),
				def("set_active_cam", &set_active_cam),
				def("get_start_time", &get_start_time),
				def("valid_vertex", &valid_vertex)
		],

		module(L, "nearest")
		[
			def("set", &level_nearest::Set),
				def("size", &level_nearest::Size),
				def("get", &level_nearest::Get)
		];

	module(L, "animslot")
		[
			def("play", &CHUDAnimItem::PlayHudAnim)
		];

	module(L, "actor_stats")
		[
			def("add_points", &add_actor_points),
				def("add_points_str", &add_actor_points_str),
				def("get_points", &get_actor_points)
		];

	module(L)
		[
			def("command_line", &command_line),
				def("IsGameTypeSingle", &IsGameTypeSingle),
				def("IsDynamicMusic", &IsDynamicMusic),
				def("render_get_dx_level", &render_get_dx_level),
				def("IsImportantSave", &IsImportantSave),
				def("IsDedicated", &is_dedicated),
				def("OnClient", &OnClient),
				def("OnServer", &OnServer)
		];

	module(L, "relation_registry")
		[
			def("community_goodwill", &g_community_goodwill),
				def("set_community_goodwill", &g_set_community_goodwill),
				def("change_community_goodwill", &g_change_community_goodwill),

				def("community_relation", &g_get_community_relation),
				def("set_community_relation", &g_set_community_relation),
				def("get_general_goodwill_between", &g_get_general_goodwill_between)
		];

	module(L, "save")
		[
			def("call_error", &ixray::save::SaveError),
				def("set_stage", &ixray::save::SaveStage)
		];

	module(L, "game")
		[
			class_< xrTime >("CTime")
				.enum_("date_format")
				[
					value("DateToDay", int(InventoryUtilities::edpDateToDay)),
						value("DateToMonth", int(InventoryUtilities::edpDateToMonth)),
						value("DateToYear", int(InventoryUtilities::edpDateToYear))
				]
				.enum_("time_format")
				[
					value("TimeToHours", int(InventoryUtilities::etpTimeToHours)),
						value("TimeToMinutes", int(InventoryUtilities::etpTimeToMinutes)),
						value("TimeToSeconds", int(InventoryUtilities::etpTimeToSeconds)),
						value("TimeToMilisecs", int(InventoryUtilities::etpTimeToMilisecs))
				]
				.def(constructor<>())
				.def(constructor<const xrTime&>())
				.def(const_self < xrTime())
				.def(const_self <= xrTime())
				.def(const_self > xrTime())
				.def(const_self >= xrTime())
				.def(const_self == xrTime())
				.def(self + xrTime())
				.def(self - xrTime())

				.def("diffSec", &xrTime::diffSec_script)
				.def("add", &xrTime::add_script)
				.def("sub", &xrTime::sub_script)

				.def("save", &xrTime::Save)
				.def("load", &xrTime::Load)

				.def("setHMS", &xrTime::setHMS)
				.def("setHMSms", &xrTime::setHMSms)
				.def("set", &xrTime::set)
				.def("get", &xrTime::get, out_value<2>() + out_value<3>() + out_value<4>() + out_value<5>() + out_value<6>() + out_value<7>() + out_value<8>())
				.def("dateToString", &xrTime::dateToString)
				.def("timeToString", &xrTime::timeToString),
				// declarations
				def("time", get_time),
				def("get_game_time", get_time_struct),
				//			def("get_surge_time",	Game::get_surge_time),
				//			def("get_object_by_name",Game::get_object_by_name),

				def("start_tutorial", &start_tutorial),
				def("stop_tutorial", &stop_tutorial),
				def("has_active_tutorial", &has_active_tutotial),
				def("translate_string", &translate_string)
		];
}

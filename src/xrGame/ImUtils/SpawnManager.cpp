#include "stdafx.h"
#include "../Level.h"
#include "../GamePersistent.h"
#include "../game_cl_single.h"
#include "../alife_simulator.h"
#include "../alife_time_manager.h"
#include "../ui/UIInventoryUtilities.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

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

							auto surfaceParams = ::Render->getSurface("ui\\ui_icon_equipment");
							bool isPressed = false;
							if (surfaceParams.Surface != nullptr)
							{
								float x = pSettings->r_float(section_name.data(), "inv_grid_x") * INV_GRID_WIDTH(isHQIcons);
								float y = pSettings->r_float(section_name.data(), "inv_grid_y") * INV_GRID_HEIGHT(isHQIcons);
								float w = pSettings->r_float(section_name.data(), "inv_grid_width") * INV_GRID_WIDTH(isHQIcons);
								float h = pSettings->r_float(section_name.data(), "inv_grid_height") * INV_GRID_HEIGHT(isHQIcons);
								ImGui::SeparatorText(section_name.data());
								isPressed = ImGui::ImageButton(imname, surfaceParams.Surface, { w, h },
									{ x / surfaceParams.w, y / surfaceParams.h },
									{ (x + w) / surfaceParams.w, (y + h) / surfaceParams.h }
								);
							}
							else
							{
								isPressed = ImGui::Button(imname);
							}

							if (isPressed)
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

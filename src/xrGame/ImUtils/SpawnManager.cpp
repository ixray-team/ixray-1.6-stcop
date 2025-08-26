#include "stdafx.h"
#include "../Level.h"
#include "../GamePersistent.h"
#include "../game_cl_single.h"
#include "../alife_simulator.h"
#include "../alife_time_manager.h"
#include "../ui/UIInventoryUtilities.h"

#include "ai_space.h"
#include "ImUtils.h"
#include "../../xrEngine/xr_input.h"
#include "Actor.h"
#include "game_news.h"
#include "script_sound.h"
#include "../../xrCore/xr_ini.h"

using Section = xr_vector<xr_pair<xr_string_view, CInifile::Sect*>>;
struct SectionData
{
	Section Sorted{};
	Section Unsorted{};
};

struct
{
	// weapon tab
	bool sort_by_grenade_launcher{};
	bool sort_by_scope_status{};
	bool sort_by_silencer_status{};

	bool sort_by_max_cost{};
	bool weapon_sort_by_max_hit_power{};
	bool weapon_sort_by_max_fire_distance{};
	bool sort_by_min_cost{};
	bool weapon_sort_by_min_hit_power{};
	bool weapon_sort_by_min_fire_distance{};
	bool spawn_on_level{};
	bool render_as_table{ true };
	char spawn_count[4]{};
	// weapon tab

	SectionData WeaponsSections = {};
	SectionData ItemsSections = {};
	SectionData ItemsUsedSections = {};
	SectionData DevicesSections = {};
	SectionData AmmoSections = {};
	SectionData OutfitSections = {};
	SectionData AddonSections = {};
	SectionData ArtefactSections = {};
	SectionData MpStuffSections = {};

	Section NpcList{};
	Section Monsters{};
	Section Vehicles{};
	Section DynamicObjects{};
	Section Explosives{};
	Section Others{};

	xr_unique_ptr<CScriptSound> sound_tip;
	string_path sound_tip_path{};
} imgui_spawn_manager;


static void SectionStatistics(const SectionData& sections) {
	size_t total = sections.Sorted.size() + sections.Unsorted.size();
	ImGui::Text("Total sections count: %zu", total);
	ImGui::Text("Total sections count (can be sorted): %zu", sections.Sorted.size());
	ImGui::Text("Total sections count (can't be sorted): %zu", sections.Unsorted.size());
	ImGui::Separator();
}

constexpr size_t kSpawnManagerMaxSectionName = 64;

float SpawnManager_ParseHitPower(const shared_str& hit_str);
void SpawnManager_RenderTooltip(CInifile::Sect* section);
bool SpawnManager_RenderButtonOrImage(CInifile::Sect* section, const char* imname);
void SpawnManager_HandleButtonPress(CInifile::Sect* section);
void SpawnManager_ProcessSections(Section& sections, size_t& number_imgui);

void DestroySpawnManagerWindow()
{
	imgui_spawn_manager.sound_tip.reset();
}

void InitSections()
{
	if (g_pClsidManager == nullptr)
	{
		R_ASSERT(!"! clsid manager uninitialized!");
		return;
	}

	string_path	file_path;
	FS.update_path(file_path, "$game_config$", "misc\\script_sound.ltx");
	CInifile ini(file_path);

	if (ini.line_exist("pda_tips", "path") == 1)
	{
		memset(file_path, 0, sizeof(file_path));
		const char* pSoundRelativeName = ini.r_string("pda_tips", "path");

		char with_format_name[128]{};
		sprintf_s(with_format_name, sizeof(with_format_name), "%s.ogg", pSoundRelativeName);


		if (FS.exist("$game_sounds$", with_format_name))
		{
			memcpy_s(imgui_spawn_manager.sound_tip_path, sizeof(imgui_spawn_manager.sound_tip_path), pSoundRelativeName, strlen(pSoundRelativeName));
		}
	}

	//xr_set<xr_string> classes = {};
	for (const auto& pSection : pSettings->sections())
	{
		if (pSection == nullptr)
			continue;

		xr_string_view name = pSection->Name.c_str();

		if (name.empty())
			continue;

		if (!pSection->line_exist("class"))
			continue;

		CLASS_ID classId = pSettings->r_clsid(name.data(), "class");

		if (pSection->line_exist("visual"))
		{
			xr_string_view visual = pSettings->r_string(name.data(), "visual");
			shared_str full_path;

			if (visual.find(".ogf") == xr_string::npos)
			{
				full_path.printf("%s%s%s", FS.get_path("$game_meshes$")->m_Path, visual.data(), ".ogf");
			}
			else {
				full_path.printf("%s%s", FS.get_path("$game_meshes$")->m_Path, visual.data());
			}
			xr_strlwr(full_path);
			if (!FS.exist(full_path.c_str()))
			{
				Msg("! SpawnManager: failed to spawn [%s] visual not found: %s", name.data(), full_path.c_str());
				continue;
			}
		}
		else {
			continue;
		}

		bool isInvItem = pSection->line_exist("cost") && pSection->line_exist("inv_weight");
		bool isFakeItem = pSection->line_exist("inv_grid_width") && pSettings->r_u32(name.data(), "inv_grid_width") <= 0 &&
						  pSection->line_exist("inv_grid_height") && pSettings->r_u32(name.data(), "inv_grid_height") <= 0;

		isInvItem &= !isFakeItem;

		size_t mp_index = name.find("mp_");
		if (g_pClsidManager->is_mp_stuff(classId) || (mp_index != xr_string_view::npos && mp_index == 0))
		{
			if (isInvItem)
			{
				imgui_spawn_manager.MpStuffSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.MpStuffSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_weapon(classId))
		{
			bool isValidSect = true;
			if (pSection->line_exist("parent_section"))
			{
				const char* parentSection = pSettings->r_string(name.data(), "parent_section");
				isValidSect = strcmp(parentSection, name.data()) == 0;
			}
			if (isInvItem && isValidSect)
			{
				imgui_spawn_manager.WeaponsSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.WeaponsSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_item(classId))
		{
			if (!pSection->line_exist("immunities_sect"))
				continue;

			if (isInvItem)
			{
				imgui_spawn_manager.ItemsSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.ItemsSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_item_used(classId))
		{
			if (!pSection->line_exist("immunities_sect"))
				continue;

			if (isInvItem)
			{
				imgui_spawn_manager.ItemsUsedSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.ItemsUsedSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_device(classId))
		{
			if (!pSection->line_exist("immunities_sect"))
				continue;

			if (isInvItem)
			{
				imgui_spawn_manager.DevicesSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.DevicesSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_ammo(classId))
		{
			if (isInvItem)
			{
				imgui_spawn_manager.AmmoSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.AmmoSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_outfit(classId))
		{
			if (isInvItem)
			{
				imgui_spawn_manager.OutfitSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.OutfitSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_addon(classId))
		{
			if (isInvItem)
			{
				imgui_spawn_manager.AddonSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.AddonSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_artefact(classId))
		{
			if (isInvItem)
			{
				imgui_spawn_manager.ArtefactSections.Sorted.push_back({ name, pSection });
			}
			else
			{
				imgui_spawn_manager.ArtefactSections.Unsorted.push_back({ name, pSection });
			}
		}
		else if (g_pClsidManager->is_npc(classId))
		{
			imgui_spawn_manager.NpcList.push_back({ name, pSection });
		}
		else if (g_pClsidManager->is_monster(classId))
		{
			imgui_spawn_manager.Monsters.push_back({ name, pSection });
		}
		else if (g_pClsidManager->is_vehicle(classId))
		{
			imgui_spawn_manager.Vehicles.push_back({ name, pSection });
		}
		else if (g_pClsidManager->is_dynamic_object(classId))
		{
			imgui_spawn_manager.DynamicObjects.push_back({ name, pSection });
		}
		else if (g_pClsidManager->is_explo(classId))
		{
			imgui_spawn_manager.Explosives.push_back({ name, pSection });
		}
		else {
			//string32 temp; CLSID2TEXT(classId, temp);
			//classes.insert(temp);

			imgui_spawn_manager.Others.push_back({ name, pSection });
		}
	}
}

void RenderSpawnManagerWindow() {
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SpawnManager)])
		return;

	if (g_pGameLevel == nullptr)
		return;

	if (ai().get_alife() == nullptr)
		return;

	if (g_pClsidManager == nullptr)
		return;

	if (imgui_spawn_manager.sound_tip.get() == nullptr)
	{
		imgui_spawn_manager.sound_tip = xr_make_unique<CScriptSound>(imgui_spawn_manager.sound_tip_path);
		imgui_spawn_manager.sound_tip->SetVolume(0.8f);
	}

	auto maxSortCost = [](Section& collection)
		{
			std::sort(collection.begin(), collection.end(), [](const auto& pair_left, const auto& pair_right)->bool
				{
					if (pair_left.second && pair_right.second)
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
		};

	auto minSortCost = [](Section& collection)
		{
			std::sort(collection.begin(), collection.end(), [](const auto& pair_left, const auto& pair_right) -> bool
				{
					if (pair_left.second && pair_right.second)
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
		};

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));
	if (ImGui::Begin("Spawn Manager", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SpawnManager)], ImGuiWindowFlags_AlwaysAutoResize))
	{
		if (ImGui::Checkbox("spawn on Level", &imgui_spawn_manager.spawn_on_level))
		{
		}
		ImGui::SetItemTooltip("if this checkbox is enabled that means it will spawn on level not in inventory");

		if (ImGui::Checkbox("View as Table", &imgui_spawn_manager.render_as_table))
		{
		}
		ImGui::SetItemTooltip("If this checkbox is enabled the content in the window will be rendered as table otherwise it is a list view");

		ImGui::InputText("count##IT_InGameSpawnManager", imgui_spawn_manager.spawn_count, sizeof(imgui_spawn_manager).spawn_count);
		ImGui::SetItemTooltip("How many items to spawn by one click, from 1 to specified number by user input");

		if (ImGui::Checkbox("sort by max cost##CheckBox_InGameSpawnManager", &imgui_spawn_manager.sort_by_max_cost))
		{
			imgui_spawn_manager.sort_by_min_cost = false;
		}
		ImGui::SetItemTooltip("Sorts items by maximum cost field that defined in weapon section in ltx file");

		if (ImGui::Checkbox("sort by min cost##CheckBox_InGameSpawnManager", &imgui_spawn_manager.sort_by_min_cost))
		{
			imgui_spawn_manager.sort_by_max_cost = false;
		}
		ImGui::SetItemTooltip("Sorts items by minimal cost field that defined in weapon section in ltx file");

		ImGui::Separator();

		if (ImGui::BeginTabBar("##TabBar_InGameSpawnManager"))
		{
			if (ImGui::BeginTabItem("Inventory Stuff"))
			{
				if (ImGui::BeginTabBar("##TabBarItems_InGameSpawnManager"))
				{
					if (ImGui::BeginTabItem("Used"))
					{
						size_t number_imgui{};
						SectionStatistics(imgui_spawn_manager.ItemsUsedSections);
						SpawnManager_ProcessSections(imgui_spawn_manager.ItemsUsedSections.Sorted, number_imgui);

						if (imgui_spawn_manager.sort_by_max_cost)
						{
							maxSortCost(imgui_spawn_manager.ItemsUsedSections.Sorted);
						}
						else if (imgui_spawn_manager.sort_by_min_cost)
						{
							minSortCost(imgui_spawn_manager.ItemsUsedSections.Sorted);
						}

						if (imgui_spawn_manager.ItemsUsedSections.Unsorted.size() > 0)
						{
							ImGui::SeparatorText("Unsorted");
							SpawnManager_ProcessSections(imgui_spawn_manager.ItemsUsedSections.Unsorted, number_imgui);
						}

						ImGui::EndTabItem();
					}

					if (ImGui::BeginTabItem("Devices"))
					{
						size_t number_imgui{};
						SectionStatistics(imgui_spawn_manager.DevicesSections);
						SpawnManager_ProcessSections(imgui_spawn_manager.DevicesSections.Sorted, number_imgui);

						if (imgui_spawn_manager.sort_by_max_cost)
						{
							maxSortCost(imgui_spawn_manager.DevicesSections.Sorted);
						}
						else if (imgui_spawn_manager.sort_by_min_cost)
						{
							minSortCost(imgui_spawn_manager.DevicesSections.Sorted);
						}

						if (imgui_spawn_manager.DevicesSections.Unsorted.size() > 0)
						{
							ImGui::SeparatorText("Unsorted");
							SpawnManager_ProcessSections(imgui_spawn_manager.DevicesSections.Unsorted, number_imgui);
						}

						ImGui::EndTabItem();
					}

					if (ImGui::BeginTabItem("Items"))
					{
						size_t number_imgui{};
						SectionStatistics(imgui_spawn_manager.ItemsSections);
						SpawnManager_ProcessSections(imgui_spawn_manager.ItemsSections.Sorted, number_imgui);

						if (imgui_spawn_manager.sort_by_max_cost)
						{
							maxSortCost(imgui_spawn_manager.ItemsSections.Sorted);
						}
						else if (imgui_spawn_manager.sort_by_min_cost)
						{
							minSortCost(imgui_spawn_manager.ItemsSections.Sorted);
						}

						if (imgui_spawn_manager.ItemsSections.Unsorted.size() > 0)
						{
							ImGui::SeparatorText("Unsorted");
							SpawnManager_ProcessSections(imgui_spawn_manager.ItemsSections.Unsorted, number_imgui);
						}

						ImGui::EndTabItem();
					}

					ImGui::EndTabBar();
				}
				

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Weapons"))
			{
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

				ImGui::Columns(2, "##filter_columns", true);

				// Left column - FILTERING (modifies data subset)
				ImGui::TextColored(ImVec4(0.8f, 0.9f, 1.0f, 1.0f), "Active Filters");
				ImGui::Separator();
				if (ImGui::Checkbox("with grenade launcher##CheckBox_InGameSpawnManager", &imgui_spawn_manager.sort_by_grenade_launcher))
				{
				}
				ImGui::SetItemTooltip("Show only weapons with underbarrel grenade launcher capability");

				if (ImGui::Checkbox("with scope##CheckBox_InGameSpawnManager", &imgui_spawn_manager.sort_by_scope_status))
				{
				}
				ImGui::SetItemTooltip("Show only weapons that support optical scopes");

				if (ImGui::Checkbox("with silencer##CheckBox_InGameSpawnManager", &imgui_spawn_manager.sort_by_silencer_status))
				{
				}
				ImGui::SetItemTooltip("Show only weapons that support suppressors");

				ImGui::NextColumn();

				// Right column - SORTING (orders existing subset)
				ImGui::TextColored(ImVec4(0.8f, 0.9f, 1.0f, 1.0f), "Sorting Methods");
				ImGui::Separator();

				if (ImGui::Checkbox("sort by max fire distance##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_max_fire_distance))
				{
					imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
					imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
					imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
				}
				ImGui::SetItemTooltip("Sorts items by maximum fire distance field that defined in weapon section in ltx file");

				if (ImGui::Checkbox("sort by max hit power##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_max_hit_power))
				{
					imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
					imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
					imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
				}
				ImGui::SetItemTooltip("Sorts items by maximum hit_power field for current game difficulty[%s] that defined in weapon section in ltx file", translate_difficulty(g_SingleGameDifficulty));

				if (ImGui::Checkbox("sort by min fire distance##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_min_fire_distance))
				{
					imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
					imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
					imgui_spawn_manager.weapon_sort_by_min_hit_power = false;
				}
				ImGui::SetItemTooltip("Sorts items by minimal fire_distance field that defined in weapon section in ltx file");

				if (ImGui::Checkbox("sort by min hit power##CheckBox_InGameSpawnManager", &imgui_spawn_manager.weapon_sort_by_min_hit_power))
				{
					imgui_spawn_manager.weapon_sort_by_max_hit_power = false;
					imgui_spawn_manager.weapon_sort_by_max_fire_distance = false;
					imgui_spawn_manager.weapon_sort_by_min_fire_distance = false;
				}
				ImGui::SetItemTooltip("Sorts items by minimal hit_power field for current game difficulty[%s]that defined in weapon section in ltx file", translate_difficulty(g_SingleGameDifficulty));
				
				ImGui::Columns(1);

				ImGui::Text("current difficulty: %s", translate_difficulty(g_SingleGameDifficulty));
				SectionStatistics(imgui_spawn_manager.WeaponsSections);

				if (imgui_spawn_manager.sort_by_max_cost)
				{
					maxSortCost(imgui_spawn_manager.WeaponsSections.Sorted);
				}
				else if (imgui_spawn_manager.sort_by_min_cost)
				{
					minSortCost(imgui_spawn_manager.WeaponsSections.Sorted);
				}

				if (imgui_spawn_manager.weapon_sort_by_max_fire_distance)
				{
					std::sort(imgui_spawn_manager.WeaponsSections.Sorted.begin(), imgui_spawn_manager.WeaponsSections.Sorted.end(),
						[](const auto& pair_left, const auto& pair_right) -> bool
						{
							if (pair_left.second && pair_right.second)
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
					std::sort(imgui_spawn_manager.WeaponsSections.Sorted.begin(), imgui_spawn_manager.WeaponsSections.Sorted.end(),
						[](const auto& pair_left, const auto& pair_right) -> bool
						{
							if (pair_left.second && pair_right.second)
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
					std::sort(imgui_spawn_manager.WeaponsSections.Sorted.begin(), imgui_spawn_manager.WeaponsSections.Sorted.end(), [](const auto& pair_left, const auto& pair_right)->bool {
						if (pair_left.second && pair_right.second)
						{
							float value_left{};
							const char* pLeftName = pair_left.first.data();
							if (pSettings->line_exist(pLeftName, "hit_power"))
							{
								auto hit_str = pSettings->r_string_wb(pLeftName, "hit_power");
								value_left = SpawnManager_ParseHitPower(hit_str);
							}

							float value_right{};
							const char* pRightName = pair_right.first.data();
							if (pSettings->line_exist(pRightName, "hit_power"))
							{
								auto hit_str = pSettings->r_string_wb(pRightName, "hit_power");
								value_right = SpawnManager_ParseHitPower(hit_str);
							}

							return value_left > value_right;
						}

						return false;
						});
				}

				if (imgui_spawn_manager.weapon_sort_by_min_hit_power)
				{
					std::sort(imgui_spawn_manager.WeaponsSections.Sorted.begin(), imgui_spawn_manager.WeaponsSections.Sorted.end(),
						[](const auto& pair_left, const auto& pair_right) -> bool
						{
							if (pair_left.second && pair_right.second)
							{
								float value_left{};
								const char* pLeftName = pair_left.first.data();
								if (pSettings->line_exist(pLeftName, "hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(pLeftName, "hit_power");
									value_left = SpawnManager_ParseHitPower(hit_str);
								}

								float value_right{};
								const char* pRightName = pair_right.first.data();
								if (pSettings->line_exist(pRightName, "hit_power"))
								{
									auto hit_str = pSettings->r_string_wb(pRightName, "hit_power");
									value_right = SpawnManager_ParseHitPower(hit_str);
								}

								return value_left < value_right;
							}

							return false;
						});
				}

				Section filteredWeapons = imgui_spawn_manager.WeaponsSections.Sorted;

				if (imgui_spawn_manager.sort_by_grenade_launcher ||
					imgui_spawn_manager.sort_by_scope_status ||
					imgui_spawn_manager.sort_by_silencer_status)
				{
					Section tempFiltered;

					std::copy_if(
						filteredWeapons.begin(),
						filteredWeapons.end(),
						std::back_inserter(tempFiltered),
						[&](const auto& pair) {
							if (!pair.second)
								return false;

							const char* section = pair.first.data();
							bool meetsConditions = true;

							if (imgui_spawn_manager.sort_by_grenade_launcher)
							{
								if (!pSettings->line_exist(section, "grenade_launcher_status"))
									meetsConditions = false;
								else if (pSettings->r_u32(section, "grenade_launcher_status") <= 0)
									meetsConditions = false;
							}

							if (meetsConditions && imgui_spawn_manager.sort_by_scope_status)
							{
								if (!pSettings->line_exist(section, "scope_status"))
									meetsConditions = false;
								else if (pSettings->r_u32(section, "scope_status") <= 0)
									meetsConditions = false;
							}

							if (meetsConditions && imgui_spawn_manager.sort_by_silencer_status)
							{
								if (!pSettings->line_exist(section, "silencer_status"))
									meetsConditions = false;
								else if (pSettings->r_u32(section, "silencer_status") <= 0)
									meetsConditions = false;
							}

							return meetsConditions;
						}
					);

					filteredWeapons = std::move(tempFiltered);
				}

				size_t number_imgui{};
				SpawnManager_ProcessSections(filteredWeapons, number_imgui);

				if (imgui_spawn_manager.WeaponsSections.Unsorted.size() > 0)
				{
					ImGui::SeparatorText("Unsorted");
					SpawnManager_ProcessSections(imgui_spawn_manager.WeaponsSections.Unsorted, number_imgui);
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Ammo"))
			{
				size_t number_imgui{};
				SectionStatistics(imgui_spawn_manager.AmmoSections);
				SpawnManager_ProcessSections(imgui_spawn_manager.AmmoSections.Sorted, number_imgui);

				if (imgui_spawn_manager.sort_by_max_cost)
				{
					maxSortCost(imgui_spawn_manager.AmmoSections.Sorted);
				}
				else if (imgui_spawn_manager.sort_by_min_cost)
				{
					minSortCost(imgui_spawn_manager.AmmoSections.Sorted);
				}

				if (imgui_spawn_manager.AmmoSections.Unsorted.size() > 0)
				{
					ImGui::SeparatorText("Unsorted");
					SpawnManager_ProcessSections(imgui_spawn_manager.AmmoSections.Unsorted, number_imgui);
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Addons"))
			{
				size_t number_imgui{};
				SectionStatistics(imgui_spawn_manager.AddonSections);
				SpawnManager_ProcessSections(imgui_spawn_manager.AddonSections.Sorted, number_imgui);

				if (imgui_spawn_manager.sort_by_max_cost)
				{
					maxSortCost(imgui_spawn_manager.AddonSections.Sorted);
				}
				else if (imgui_spawn_manager.sort_by_min_cost)
				{
					minSortCost(imgui_spawn_manager.AddonSections.Sorted);
				}
				if (imgui_spawn_manager.AddonSections.Unsorted.size() > 0)
				{
					ImGui::SeparatorText("Unsorted");
					SpawnManager_ProcessSections(imgui_spawn_manager.AddonSections.Unsorted, number_imgui);
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Artefacts"))
			{
				size_t number_imgui{};
				SectionStatistics(imgui_spawn_manager.ArtefactSections);
				SpawnManager_ProcessSections(imgui_spawn_manager.ArtefactSections.Sorted, number_imgui);

				if (imgui_spawn_manager.sort_by_max_cost)
				{
					maxSortCost(imgui_spawn_manager.ArtefactSections.Sorted);
				}
				else if (imgui_spawn_manager.sort_by_min_cost)
				{
					minSortCost(imgui_spawn_manager.ArtefactSections.Sorted);
				}
				if (imgui_spawn_manager.ArtefactSections.Unsorted.size() > 0)
				{
					ImGui::SeparatorText("Unsorted");
					SpawnManager_ProcessSections(imgui_spawn_manager.ArtefactSections.Unsorted, number_imgui);
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Outfits"))
			{
				size_t number_imgui{};
				SectionStatistics(imgui_spawn_manager.OutfitSections);
				SpawnManager_ProcessSections(imgui_spawn_manager.OutfitSections.Sorted, number_imgui);

				if (imgui_spawn_manager.sort_by_max_cost)
				{
					maxSortCost(imgui_spawn_manager.OutfitSections.Sorted);
				}
				else if (imgui_spawn_manager.sort_by_min_cost)
				{
					minSortCost(imgui_spawn_manager.OutfitSections.Sorted);
				}
				if (imgui_spawn_manager.OutfitSections.Unsorted.size() > 0)
				{
					ImGui::SeparatorText("Unsorted");
					SpawnManager_ProcessSections(imgui_spawn_manager.OutfitSections.Unsorted, number_imgui);
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Multiplayer Stuff"))
			{
				size_t number_imgui{};
				SectionStatistics(imgui_spawn_manager.MpStuffSections);
				SpawnManager_ProcessSections(imgui_spawn_manager.MpStuffSections.Sorted, number_imgui);

				if (imgui_spawn_manager.sort_by_max_cost)
				{
					maxSortCost(imgui_spawn_manager.MpStuffSections.Sorted);
				}
				else if (imgui_spawn_manager.sort_by_min_cost)
				{
					minSortCost(imgui_spawn_manager.MpStuffSections.Sorted);
				}
				if (imgui_spawn_manager.MpStuffSections.Unsorted.size() > 0)
				{
					ImGui::SeparatorText("Unsorted");
					SpawnManager_ProcessSections(imgui_spawn_manager.MpStuffSections.Unsorted, number_imgui);
				}

				ImGui::EndTabItem();
			}

			if (imgui_spawn_manager.NpcList.size() > 0)
			{
				if (ImGui::BeginTabItem("NPC"))
				{
					size_t number_imgui{};
					ImGui::Text("total sections count: %d", imgui_spawn_manager.NpcList.size());
					SpawnManager_ProcessSections(imgui_spawn_manager.NpcList, number_imgui);

					ImGui::EndTabItem();
				}
			}

			if (imgui_spawn_manager.Monsters.size() > 0)
			{
				if (ImGui::BeginTabItem("Monsters"))
				{
					size_t number_imgui{};
					ImGui::Text("total sections count: %d", imgui_spawn_manager.Monsters.size());
					SpawnManager_ProcessSections(imgui_spawn_manager.Monsters, number_imgui);

					ImGui::EndTabItem();
				}
			}

			if (imgui_spawn_manager.Vehicles.size() > 0)
			{
				if (ImGui::BeginTabItem("Vehicles"))
				{
					size_t number_imgui{};
					ImGui::Text("total sections count: %d", imgui_spawn_manager.Vehicles.size());
					SpawnManager_ProcessSections(imgui_spawn_manager.Vehicles, number_imgui);

					ImGui::EndTabItem();
				}
			}

			if (imgui_spawn_manager.Explosives.size() > 0)
			{
				if (ImGui::BeginTabItem("Explosives"))
				{
					size_t number_imgui{};
					ImGui::Text("total sections count: %d", imgui_spawn_manager.Explosives.size());
					SpawnManager_ProcessSections(imgui_spawn_manager.Explosives, number_imgui);

					ImGui::EndTabItem();
				}
			}

			if (imgui_spawn_manager.DynamicObjects.size() > 0)
			{
				if (ImGui::BeginTabItem("Dynamic Objects"))
				{
					size_t number_imgui{};
					ImGui::Text("total sections count: %d", imgui_spawn_manager.DynamicObjects.size());
					SpawnManager_ProcessSections(imgui_spawn_manager.DynamicObjects, number_imgui);

					ImGui::EndTabItem();
				}
			}

			if (imgui_spawn_manager.Others.size() > 0)
			{
				if (ImGui::BeginTabItem("Others"))
				{
					size_t number_imgui{};
					ImGui::Text("total sections count: %d", imgui_spawn_manager.Others.size());
					SpawnManager_ProcessSections(imgui_spawn_manager.Others, number_imgui);

					ImGui::EndTabItem();
				}
			}

			ImGui::EndTabBar();
		}
	}
	ImGui::End();
	ImGui::PopStyleColor(1);
}

void SpawnManager_ProcessSections(Section& sections, size_t& number_imgui)
{
	auto sm_process_button = [](bool is_table, const xr_string_view& section_name, CInifile::Sect* pSection, size_t& number_imgui) {
		char imname[kSpawnManagerMaxSectionName]{};
		memcpy_s(imname, sizeof(imname), section_name.data(), section_name.size());

		char index[10]{};
		sprintf_s(index, sizeof(index), "##%zu", number_imgui);
		memcpy_s(imname + section_name.size(), sizeof(imname), index, sizeof(index));

		if (SpawnManager_RenderButtonOrImage(pSection, imname))
		{
			SpawnManager_HandleButtonPress(pSection);
		}

		SpawnManager_RenderTooltip(pSection);

		if (!is_table)
			ImGui::NewLine();

		++number_imgui;
		};

	if (imgui_spawn_manager.render_as_table)
	{
		ImGuiTableFlags flags =
			ImGuiTableFlags_Borders |
			ImGuiTableFlags_SizingStretchSame;

		constexpr size_t kSpawnManagerTableViewColumnSize = 5;
		size_t size_of_sections = sections.size();
		size_t row_max = (size_of_sections + kSpawnManagerTableViewColumnSize - 1) / kSpawnManagerTableViewColumnSize;

		if (ImGui::BeginTable("##SpawnManagerRenderAsTable", kSpawnManagerTableViewColumnSize, flags))
		{
			for (size_t row = 0; row < row_max; ++row)
			{
				ImGui::TableNextRow();

				for (size_t column = 0; column < kSpawnManagerTableViewColumnSize; ++column)
				{
					size_t current_section_index = row * kSpawnManagerTableViewColumnSize + column;

					if (current_section_index < size_of_sections)
					{
						ImGui::TableSetColumnIndex((int)column);

						const auto& pair = sections[current_section_index];

						const xr_string_view& section_name = pair.first;
						CInifile::Sect* pSection = pair.second;

						if (!section_name.empty() && pSection)
						{
							sm_process_button(imgui_spawn_manager.render_as_table, section_name, pSection, number_imgui);
						}
					}
				}
			}
			ImGui::EndTable();
		}
	}
	else
	{
		for (const auto& data : sections)
		{
			const auto& section_name = data.first;
			const auto& pSection = data.second;

			if (!section_name.empty() && pSection)
			{
				sm_process_button(imgui_spawn_manager.render_as_table, section_name, pSection, number_imgui);
			}
		}
	}

	std::sort(sections.begin(), sections.end());
}

bool SpawnManager_RenderButtonOrImage(CInifile::Sect* section, const char* imname)
{
	auto name = section->Name.c_str();
	const auto surface = READ_IF_EXISTS(pSettings, r_string, name, "icons_texture", "ui\\ui_icon_equipment");
	const auto surfaceParams = ::Render->getSurface(surface);

	bool isIcon = section->line_exist("inv_grid_x")
		&& section->line_exist("inv_grid_y")
		&& section->line_exist("inv_grid_width")
		&& section->line_exist("inv_grid_height");

	if (surfaceParams.Surface == nullptr || !isIcon)
		return ImGui::Button(imname);

	float x = pSettings->r_float(name, "inv_grid_x") * INV_GRID_WIDTH(isHQIcons);
	float y = pSettings->r_float(name, "inv_grid_y") * INV_GRID_HEIGHT(isHQIcons);
	float w = pSettings->r_float(name, "inv_grid_width") * INV_GRID_WIDTH(isHQIcons);
	float h = pSettings->r_float(name, "inv_grid_height") * INV_GRID_HEIGHT(isHQIcons);

	ImGui::SeparatorText(name);
	return ImGui::ImageButton(imname, surfaceParams.Surface, { w / (1 + isHQIcons), h / (1 + isHQIcons)},
		{ x / surfaceParams.w, y / surfaceParams.h },
		{ (x + w) / surfaceParams.w, (y + h) / surfaceParams.h });

}

void SpawnManager_HandleButtonPress(CInifile::Sect* section)
{
	int count = atoi(imgui_spawn_manager.spawn_count);
	if (count == 0)
		count = 1;

	if (Actor())
	{
		if (imgui_spawn_manager.sound_tip.get())
		{
			imgui_spawn_manager.sound_tip->PlayAtPos(Actor()->lua_game_object(), Fvector().set(0.0f, 0.0f, 0.0f), 0.0f, sm_2D);
		}

		char text_news[128]{};
		if (section->line_exist("inv_name"))
		{
			const char* name = g_pStringTable->translate(pSettings->r_string(section->Name.c_str(), "inv_name")).c_str();
			sprintf_s(text_news, sizeof(text_news), "[%s] x [%d] (%s)", section->Name.c_str(), count, name);
		}
		else
		{
			sprintf_s(text_news, sizeof(text_news), "[%s] x [%d]", section->Name.c_str(), count);
		}

		GAME_NEWS_DATA				news_data;
		news_data.m_type = GAME_NEWS_DATA::eNewsType::eNews;
		news_data.news_caption = g_pStringTable->translate("general_in_item");
		news_data.news_text = text_news;
		news_data.show_time = 3000;
		news_data.texture_name = "ui_ixray_spawn_icon";
		Actor()->AddGameNews(news_data);
	}

	xr_string cmd = "g_spawn_inv ";
	bool isInvItem = section->line_exist("cost") && section->line_exist("inv_weight");

	bool spawn_on_level = imgui_spawn_manager.spawn_on_level;
	if (pInput != nullptr && pInput->iGetAsyncKeyState(SDL_SCANCODE_LCTRL))
	{
		spawn_on_level = !imgui_spawn_manager.spawn_on_level;
	}

	if (spawn_on_level || !isInvItem)
	{
		cmd = "g_spawn ";
	}

	cmd += section->Name.c_str();
	cmd += " ";
	cmd += xr_string::ToString(count);

	execute_console_command_deferred(Console, cmd.c_str());
}

void SpawnManager_RenderTooltip(CInifile::Sect* section)
{
	if (ImGui::BeginItemTooltip())
	{
		if (section->line_exist("inv_name"))
		{
			const char* pTranslateSectionName = pSettings->r_string(section->Name.c_str(), "inv_name");
			if (pTranslateSectionName != nullptr)
			{
				const auto& pTranslatedString = g_pStringTable->translate(pTranslateSectionName);
				ImGui::Text("In game name: [%s]", Platform::ANSI_TO_UTF8(pTranslatedString.c_str()).c_str());
			}
		}

		if (section->line_exist("class"))
		{
			const char* pClass = pSettings->r_string(section->Name.c_str(), "class");
			ImGui::Text("class: [%s]", pClass);
		}

		if (section->line_exist("cost"))
		{
			const char* pCost = pSettings->r_string(section->Name.c_str(), "cost");
			ImGui::Text("cost: [%s]", pCost);
		}

		if (section->line_exist("inv_weight"))
		{
			const char* pInvW = pSettings->r_string(section->Name.c_str(), "inv_weight");
			ImGui::Text("inventory weight: [%s]", pInvW);
		}

		if (section->line_exist("hit_power"))
		{
			auto hit_str = pSettings->r_string_wb(section->Name.c_str(), "hit_power");
			float value = SpawnManager_ParseHitPower(hit_str);
			ImGui::Text("hit power: [%.2f]", value);
		}

		if (section->line_exist("fire_distance"))
		{
			const char* pField = pSettings->r_string(section->Name.c_str(), "fire_distance");
			ImGui::Text("fire distance: [%s]", pField);
		}

		ImGui::EndTooltip();
	}
}

float SpawnManager_ParseHitPower(const shared_str& hit_str) {

	Fvector4 fvHitPower{};
	string32 buffer{};
	fvHitPower[egdMaster] = (float)atof(_GetItem(*hit_str, 0, buffer));
	fvHitPower[egdNovice] = fvHitPower[egdStalker] = fvHitPower[egdVeteran] = fvHitPower[egdMaster];
	int num_game_diff_param = _GetItemCount(*hit_str);
	if (num_game_diff_param > 1)
	{
		fvHitPower[egdVeteran] = (float)atof(_GetItem(*hit_str, 1, buffer));
	}
	if (num_game_diff_param > 2)
	{
		fvHitPower[egdStalker] = (float)atof(_GetItem(*hit_str, 2, buffer));
	}
	if (num_game_diff_param > 3)
	{
		fvHitPower[egdNovice] = (float)atof(_GetItem(*hit_str, 3, buffer));
	}

	return fvHitPower[g_SingleGameDifficulty];
}
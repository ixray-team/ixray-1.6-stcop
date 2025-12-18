
#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"
#include "../player_hud.h"
#include "ai_space.h"
#include "../../xrUI/ui_base.h"
#include "ImUtils.h"
#include "../game_news.h"

CImGuiTextureEditor g_imgui_texture_editor;

constexpr decltype(CImGuiTextureEditor::selected_index) _kInvalidSelectedID = decltype(CImGuiTextureEditor::selected_index)(-1);

void TextureEditor_WorkerThread()
{
	if (!xr_FS)
		return;

	constexpr const char _kThreadName[] = "IXRAY - Texture Editor's Worker Thread";

	thread_name(_kThreadName);


	using status_t = CImGuiTextureEditor::eAnalyzedStatus;
	using texture_t = CImGuiTextureEditor::STextureEntry;
	using request_t = CImGuiTextureEditor::SRequestData;

	while (g_imgui_texture_editor.is_running_wt)
	{
		if (g_imgui_texture_editor.requests.empty() == false)
		{
			const request_t& req = g_imgui_texture_editor.requests.pop();

			switch (req.type)
			{
			case CImGuiTextureEditor::eRequestType::kReadSettings:
			{
				if (g_imgui_texture_editor.is_settings_read == false)
				{
					if (FS.exist("$app_data_root$", "texture_editor_settings.bin"))
					{
						IReader* pReader = FS.r_open("$app_data_root$", "texture_editor_settings.bin");

						if (pReader && pReader->length())
						{
							if (pReader->length() != sizeof(g_imgui_texture_editor.settings))
							{
								Msg("[TextureEditor]: trying to read different version of settings... Reset settings to default");
							}
							else
							{
								pReader->r(&g_imgui_texture_editor.settings, sizeof(g_imgui_texture_editor.settings));

								Msg("[TextureEditor]: read settings from -> texture_editor_settings.bin");
							}
						}

						if (pReader)
						{
							FS.r_close(pReader);
						}
					}
					else
					{
						string_path path_to_settings;
						FS.update_path(path_to_settings, "$app_data_root$", "texture_editor_settings.bin");
						Msg("[TextureEditor]: can't read settings because there's no file %s", path_to_settings);
					}

					g_imgui_texture_editor.is_settings_read = true;
				}

				break;
			}

			case CImGuiTextureEditor::eRequestType::kShutdownThread:
			{
				g_imgui_texture_editor.is_running_wt = false;
				break;
			}
			case CImGuiTextureEditor::eRequestType::kWriteSettings:
			{
				if (g_imgui_texture_editor.is_init)
				{
					string_path path_to_settings;
					FS.update_path(path_to_settings, "$app_data_root$", "texture_editor_settings.bin");
					IWriter* pWriter = FS.w_open(path_to_settings);

					if (pWriter)
					{
						pWriter->w(&g_imgui_texture_editor.settings, sizeof(g_imgui_texture_editor.settings));

						Msg("[TextureEditor]: saved settings to -> texture_editor_settings.bin");

						FS.w_close(pWriter);
					}
				}

				g_imgui_texture_editor.is_settings_write = true;

				break;
			}
			case CImGuiTextureEditor::eRequestType::kReadAll:
			{
				if (g_imgui_texture_editor.is_all_analyzed)
					g_imgui_texture_editor.is_all_analyzed = false;

				if (g_imgui_texture_editor.is_all_analyzed == false)
				{
					xr_set<xr_string> files;
					FS_Path* pPath = FS.get_path(_game_textures_);

					if (pPath)
					{
						FS.get_all_files_in_dir(files, pPath->m_Path);
						g_imgui_texture_editor.total_files_in_folder = files.size();
					}

					g_imgui_texture_editor.current_analyzed_count = 0;

					std::filesystem::path temp;
					for (const xr_string& file_path : files)
					{
						g_imgui_texture_editor.wt_current_analyzing_texture = file_path;
						temp = file_path.c_str();

						std::filesystem::path folder = temp.parent_path().filename();
						std::filesystem::path filename = temp.filename();

						std::filesystem::path real_name = folder / filename;

						const auto& fn = real_name.string();

						if (fn.find(".dds") != std::string::npos && fn.find(".thm") == std::string::npos)
						{
							CImGuiTextureEditor::STextureEntry data;
							data.path[0] = 0;

							std::strcat(
								data.path,
								fn.data()
							);

							constexpr u32 _kFileNameLimit = sizeof(texture_t::path) / sizeof(texture_t::path[0]);

							if (fn.size() > _kFileNameLimit)
							{
								data.analyze_status_result_flags |= status_t::kInvalidFileName;
							}
							else
							{
								++g_imgui_texture_editor.valid_count;
							}

							g_imgui_texture_editor.textures.push_back(data);

							++g_imgui_texture_editor.total_textures_in_folder;
						}
						else if (fn.find(".thm") != std::string::npos && fn.find(".dds") == std::string::npos)
						{
							++g_imgui_texture_editor.total_thm_in_folder;
						}
						else if (fn.find(".seq") != std::string::npos)
						{
							++g_imgui_texture_editor.total_seq_in_folder;
						}
						else if (
							fn.find(".png") != std::string::npos &&
							fn.find(".dds") == std::string::npos &&
							fn.find(".thm") == std::string::npos
							)
						{
							++g_imgui_texture_editor.total_png_in_folder;
						}
						else if (
							fn.find(".svg") != std::string::npos &&
							fn.find(".dds") == std::string::npos &&
							fn.find(".thm") == std::string::npos
							)
						{
							++g_imgui_texture_editor.total_svg_in_folder;
						}
						else if (
							fn.find(".bmp") != std::string::npos &&
							fn.find(".dds") == std::string::npos &&
							fn.find(".thm") == std::string::npos
							)
						{
							++g_imgui_texture_editor.total_bmp_in_folder;
						}
						else if (
							fn.find(".ogm") != std::string::npos &&
							fn.find(".dds") == std::string::npos &&
							fn.find(".thm") == std::string::npos
							)
						{
							++g_imgui_texture_editor.total_ogm_in_folder;
						}
						else if (
							fn.find(".ini") != std::string::npos &&
							fn.find(".dds") == std::string::npos &&
							fn.find(".thm") == std::string::npos
							)
						{
							++g_imgui_texture_editor.total_ini_in_folder;
						}
						else
						{
							if (
								fn.find(".dds") != std::string::npos &&
								fn.find(".thm") != std::string::npos
								)
							{
								++g_imgui_texture_editor.total_unable_to_classify_files_in_folder;
							}
							else
							{
								++g_imgui_texture_editor.total_other_in_folder;
							}

						}



						++g_imgui_texture_editor.current_analyzed_count;
					}

					g_imgui_texture_editor.is_all_analyzed = true;
				}

				break;
			}
			case CImGuiTextureEditor::eRequestType::kUpdateSelected:
			{
				if (req.selected_id != _kInvalidSelectedID)
				{
					if (req.selected_id < g_imgui_texture_editor.textures.size())
					{

					}
				}

				g_imgui_texture_editor.is_update_selected = true;

				break;
			}
			default:
			{
				g_imgui_texture_editor.is_running_wt = false;
				R_ASSERT2(false, "report to developers!");
			}
			}

		}


	}

	Msg("[TextureEditor]: Shutdown thread -> %s", _kThreadName);

	g_imgui_texture_editor.is_thread_finished_execution = true;
}

void RenderTextureEditor()
{
	if (g_imgui_texture_editor.is_thread_started == false)
	{
		g_imgui_texture_editor.worker_thread = std::thread(&TextureEditor_WorkerThread);
		g_imgui_texture_editor.is_thread_started = true;
	}


	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_TextureEditor)])
		return;

	if (g_imgui_texture_editor.is_init == false)
	{
		constexpr u32 _kReserve = 4096 * 4;

		g_imgui_texture_editor.textures.reserve(_kReserve);
		g_imgui_texture_editor.filter_query.reserve(_kReserve);

		g_imgui_texture_editor.requests.push({ .type = CImGuiTextureEditor::eRequestType::kReadSettings });
		g_imgui_texture_editor.requests.push({ .type = CImGuiTextureEditor::eRequestType::kReadAll });

		g_imgui_texture_editor.is_init = true;
	}

	bool is_in_game = g_actor && ai().get_alife() && g_pGameLevel;

	if (ImGui::Begin("Texture Editor", 0, ImGuiWindowFlags_AlwaysAutoResize))
	{
		if (ImGui::BeginTabBar("##TETB"))
		{
			if (ImGui::BeginTabItem("All"))
			{
				if (g_imgui_texture_editor.is_all_analyzed)
				{
					constexpr ImVec4 _kColorValid = ImVec4(0.0f, 0.8f, 0.0f, 1.0f);
					constexpr ImVec4 _kColorInvalid = ImVec4(0.8f, 0.0f, 0.0f, 1.0f);

					ImGui::SeparatorText("Stats");
					ImGui::Text("Total files: %d", g_imgui_texture_editor.total_files_in_folder);
					ImGui::Text("\t- .dds: %d", g_imgui_texture_editor.total_textures_in_folder);
					ImGui::Text("\t- .thm: %d", g_imgui_texture_editor.total_thm_in_folder);

					if (g_imgui_texture_editor.settings.show_only_dds_and_thm == false)
					{
						ImGui::Text("\t- .seq: %d", g_imgui_texture_editor.total_seq_in_folder);
						ImGui::Text("\t- .png: %d", g_imgui_texture_editor.total_png_in_folder);
						ImGui::Text("\t- .svg: %d", g_imgui_texture_editor.total_svg_in_folder);
						ImGui::Text("\t- .bmp: %d", g_imgui_texture_editor.total_bmp_in_folder);
						ImGui::Text("\t- .ogm: %d", g_imgui_texture_editor.total_ogm_in_folder);
						ImGui::Text("\t- .ini: %d", g_imgui_texture_editor.total_ini_in_folder);
						ImGui::Text("\t- other: %d", g_imgui_texture_editor.total_other_in_folder);
						ImGui::SetItemTooltip("extensions that weren't recognizable by engine");
					}

					ImGui::Text("\t- unknown: %d", g_imgui_texture_editor.total_unable_to_classify_files_in_folder);
					ImGui::SetItemTooltip("filename contains .thm and .dds\nso we won't try to determine them as thm or dds files");

					ImGui::Text("Valid:");
					ImGui::Text("\t- textures: %d", g_imgui_texture_editor.valid_count);
					ImGui::Text("Invalid:");
					ImGui::Text("\t- by filename: %d", 0);
					ImGui::Text("\t- no thm: %d", 0);
					ImGui::Text("\t- invalid thm: %d", 0);
					ImGui::Text("\t- not power of 2: %d", 0);
					ImGui::Text("\t- no mip-maps: %d", 0);

					ImGui::SeparatorText("Settings");

					ImGui::Checkbox("Show invalid first", &g_imgui_texture_editor.settings.show_invalid_first);
					ImGui::Checkbox("Show only dds and thm in stats", &g_imgui_texture_editor.settings.show_only_dds_and_thm);

					ImGui::SeparatorText("Search");

					char _input_buffer[sizeof(CImGuiTextureEditor::STextureEntry::path)];
					_input_buffer[0] = 0;

					ImGui::InputText("name", _input_buffer, sizeof(_input_buffer));
					ImGui::SameLine();

					if (ImGui::Button("submit##TESearch"))
					{
						// todo: do filter stuff by name
					}

					ImGui::Separator();

					constexpr const char* _kColumnNames[] = {
						"Name",
						"Status"
					};

					constexpr u32 _kColumnsSize = sizeof(_kColumnNames) / sizeof(_kColumnNames[0]);

					size_t textures_count = g_imgui_texture_editor.textures.size();
					size_t row_max = textures_count;



					ImGui::BeginTable("##TETV", _kColumnsSize);

					for (u32 i = 0; i < _kColumnsSize; ++i)
					{
						ImGui::TableSetupColumn(_kColumnNames[i]);
					}

					ImGui::TableHeadersRow();

					ImGuiListClipper clipper;

					clipper.Begin(u32(g_imgui_texture_editor.textures.size()));

					while (clipper.Step())
					{
						//for (u32 row = 0; row < row_max; ++row)
						for (u32 row = clipper.DisplayStart; row < clipper.DisplayEnd; ++row)
						{
							ImGui::TableNextRow();


							for (size_t column = 0; column < _kColumnsSize; ++column)
							{
								ImGui::TableSetColumnIndex((int)column);


								const CImGuiTextureEditor::STextureEntry& texture = g_imgui_texture_editor.textures[row];

								switch (column)
								{
								case 0:
								{
									char sel_name[sizeof(CImGuiTextureEditor::STextureEntry::path) * 2];
									std::sprintf(sel_name, "[%d] %s", row + 1, texture.path);

									bool selected_status = g_imgui_texture_editor.selected_index == row;

									if (ImGui::Selectable(sel_name, selected_status))
									{
										g_imgui_texture_editor.window_selected_name[0] = 0;
										g_imgui_texture_editor.is_update_selected = false;
										g_imgui_texture_editor.selected_index = row;
										g_imgui_texture_editor.requests.push({ .type = CImGuiTextureEditor::eRequestType::kUpdateSelected, .selected_id = row });
										std::strcat(g_imgui_texture_editor.window_selected_name, "Selected - ");
										std::strcat(g_imgui_texture_editor.window_selected_name, g_imgui_texture_editor.textures[row].path);
									}

									break;
								}
								case 1:
								{
									ImGui::TextColored(ImVec4(0.0f, 0.8f, 0.0f, 1.0f), "%s", "valid");
									break;
								}
								}
							}
						}
					}

					clipper.End();



					ImGui::EndTable();



				}
				else
				{
					if (g_imgui_texture_editor.total_files_in_folder == 0)
					{
						ImGui::Text("Preparing...");
					}
					else
					{
						ImGui::Text("Analyzing... [%s] %zu/%zu",
							g_imgui_texture_editor.wt_current_analyzing_texture.data(),
							g_imgui_texture_editor.current_analyzed_count,
							g_imgui_texture_editor.total_files_in_folder
						);
					}
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Game", &is_in_game))
			{

				ImGui::EndTabItem();
			}
		}
		ImGui::EndTabBar();
	}

	ImGui::End();

	if (g_imgui_texture_editor.selected_index != _kInvalidSelectedID)
	{
		if (ImGui::Begin(g_imgui_texture_editor.window_selected_name, 0, ImGuiWindowFlags_AlwaysAutoResize))
		{

			if (g_imgui_texture_editor.is_update_selected)
			{
				using texture_t = CImGuiTextureEditor::STextureEntry;

				const texture_t& selected = g_imgui_texture_editor.textures[g_imgui_texture_editor.selected_index];

				ImGui::Text("%s", selected.path);
			}
			else
			{
				ImGui::Text("Loading...");
			}

		}

		ImGui::End();
	}



}
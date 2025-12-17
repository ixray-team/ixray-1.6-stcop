
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
				if (g_imgui_texture_editor.is_settings_read==false)
				{
					if (FS.exist("$app_data_root$", "texture_editor_settings.bin"))
					{
						IReader* pReader = FS.r_open("$app_data_root$", "texture_editor_settings.bin");

						if (pReader && pReader->length())
						{
							pReader->r(&g_imgui_texture_editor.settings.show_invalid_first, sizeof(g_imgui_texture_editor.settings.show_invalid_first));
							
							Msg("[TextureEditor]: read settings from -> texture_editor_settings.bin");
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
						pWriter->w(&g_imgui_texture_editor.settings.show_invalid_first, sizeof(g_imgui_texture_editor.settings.show_invalid_first));

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

						if (fn.find(".thm") != std::string::npos && fn.find(".dds") == std::string::npos)
						{
							++g_imgui_texture_editor.total_thm_in_folder;
						}

						++g_imgui_texture_editor.current_analyzed_count;
					}

					g_imgui_texture_editor.is_all_analyzed = true;
				}

				break;
			}
			case CImGuiTextureEditor::eRequestType::kReadMetadataOfSelected:
			{
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

	Msg("[Threading]: Shutdown thread -> %s", _kThreadName);
}

void RenderTextureEditor()
{
	if (g_imgui_texture_editor.is_thread_started == false)
	{
		g_imgui_texture_editor.worker_thread = std::thread(&TextureEditor_WorkerThread);
		g_imgui_texture_editor.worker_thread.detach();
		g_imgui_texture_editor.is_thread_started = true;
	}


	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_TextureEditor)])
		return;

	if (g_imgui_texture_editor.is_init == false)
	{
		constexpr u32 _kReserve = 4096 * 4;
		constexpr u32 _kReserveFilter = 4096;

		g_imgui_texture_editor.textures.reserve(_kReserve);
		g_imgui_texture_editor.filter_query.reserve(_kReserveFilter);

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
					ImGui::Text("Total files: %zu", g_imgui_texture_editor.total_files_in_folder);
					ImGui::Text("\t- .dds: %zu", g_imgui_texture_editor.total_textures_in_folder);
					ImGui::Text("\t- .thm: %zu", g_imgui_texture_editor.total_thm_in_folder);

					ImGui::Text("Valid:");
					ImGui::Text("\t- textures: %zu", g_imgui_texture_editor.valid_count);
					ImGui::Text("Invalid:");
					ImGui::Text("\t- by filename: %zu", 0);
					ImGui::Text("\t- no thm: %zu", 0);
					ImGui::Text("\t- invalid thm: %zu", 0);
					ImGui::Text("\t- not power of 2: %zu", 0);
					ImGui::Text("\t- no mip-maps: %zu", 0);

					ImGui::SeparatorText("Settings");

					ImGui::Checkbox("Show invalid first", &g_imgui_texture_editor.settings.show_invalid_first);

					ImGui::SeparatorText("Search");

					char _name[sizeof(CImGuiTextureEditor::STextureEntry::path)];
					ImGui::InputText("name", _name, sizeof(_name));
					ImGui::SameLine();

					if (ImGui::Button("submit##TESearch"))
					{
						// todo: do filter stuff by name
					}

					ImGui::Separator();

					constexpr const char* _kColumnNames[] = {
						"Name",
						"Status",
						"Editing"
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

					for (size_t row = 0; row < row_max; ++row)
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
								ImGui::Text("[%d] %s", row+1, texture.path);
								break;
							}
							case 1:
							{
								ImGui::TextColored(ImVec4(0.0f, 0.8f, 0.0f, 1.0f), "%s", "valid");
								break;
							}
							case 2:
							{
								// todo: make selection
								if (row == 0)
								{
									ImGui::Text("editing selected...");
								}
								break;
							}
							}
						}
					}

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
}
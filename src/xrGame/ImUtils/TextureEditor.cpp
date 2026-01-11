
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

#ifdef IXR_WINDOWS
#include <dxgiformat.h>
#else
#endif

#include <magic_enum/magic_enum.hpp>

CImGuiTextureEditor g_imgui_texture_editor;

constexpr decltype(CImGuiTextureEditor::selected_index) _kInvalidSelectedID = decltype(CImGuiTextureEditor::selected_index)(-1);

constexpr u16 kTextureEditor_PreviewsVersion = sizeof(string_path) / sizeof(std::remove_extent_t<string_path>);
constexpr u16 kTextureEditor_PreviewsEntrySize = kTextureEditor_PreviewsVersion + sizeof(u64);

constexpr ImVec2 kTextureEditor_PreviewSizeHigh = ImVec2(512.0f, 512.0f);
constexpr ImVec2 kTextureEditor_PreviewSizeMid = ImVec2(256.0f, 256.0f);
constexpr ImVec2 kTextureEditor_PreviewSizeLow = ImVec2(64.0f, 64.0f);

void validate_entry(
	const xr_vector<const xr_string*>& thms,
	CImGuiTextureEditor::STextureEntry& entry
)
{
	if (thms.empty())
		return;

	g_imgui_texture_editor.wt_current_analyzing_texture = entry.path;

	std::filesystem::path thm = entry.path;
	thm.replace_extension(".thm");

	auto it = std::find_if(thms.begin(), thms.end(), [thm](const xr_string* const& el) -> bool {
		return el->c_str() == thm;
		});

	if (it != thms.end())
	{
		entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kHasTHM;
	}
	else
	{
		std::string_view path_view = entry.path;

		if (path_view.find("bump#") != std::string_view::npos)
		{
			entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kIgnoreTHM;
		}
	}
}



void TextureEditor_WorkerThread(const ime_request_t& req)
{
	if (!xr_FS)
		return;

	R_ASSERT2(static_cast<eImGuiEditorType>(req.editor_type) == eImGuiEditorType::kTextureEditor, "invalid data came you should debug code");

	if (static_cast<eImGuiEditorType>(req.editor_type) != eImGuiEditorType::kTextureEditor)
		return;

	using status_t = CImGuiTextureEditor::eAnalyzedStatus;
	using texture_t = CImGuiTextureEditor::STextureEntry;

	switch (static_cast<CImGuiTextureEditor::eRequestType>(req.request_type))
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
			FS_Path* pTexturesFolder = FS.get_path(_game_textures_);

			if (!pTexturesFolder)
			{
				g_imgui_texture_editor.is_all_analyzed = true;

				Msg("[TextureEditor]: ! invalid filesystem was initialized on your side -> report to developers!");

				break;
			}

			if (pTexturesFolder)
			{
				FS.get_all_files_in_dir(files, pTexturesFolder->m_Path);
				g_imgui_texture_editor.total_files_in_folder = files.size();
			}

			g_imgui_texture_editor.current_analyzed_count = 0;

			xr_vector<const xr_string*> thms;

			for (const xr_string& file_path : files)
			{
				const auto& fn = file_path;

				if (
					fn.find(".dds") != std::string::npos &&
					fn.find(".thm") == std::string::npos
					)
				{
					CImGuiTextureEditor::STextureEntry data;
					data.path[0] = 0;

					std::strcat(
						data.path,
						fn.data()
					);

					if (fn.size() > sizeof(string_path))
					{
						data.analyze_status_result_flags |= status_t::kTooLongPath;
					}
					else
					{
						std::filesystem::path temp = fn.c_str();

						data.filename[0] = 0;
						data.subpath[0] = 0;

						std::strcat(data.filename, temp.filename().string().c_str());

						std::filesystem::path no_fn = fn.c_str();
						no_fn.remove_filename();

						std::filesystem::path relative = std::filesystem::relative(no_fn, pTexturesFolder->m_Path);

						if (relative != ".")
						{
							std::strcat(data.subpath, relative.string().c_str());
						}

						g_imgui_texture_editor.wt_current_analyzing_texture = file_path.c_str();
						++g_imgui_texture_editor.valid_count;
					}

					g_imgui_texture_editor.textures.push_back(data);
					g_imgui_texture_editor.filter_query.push_back(g_imgui_texture_editor.textures.size() - 1);

					++g_imgui_texture_editor.total_textures_in_folder;
				}
				else if (fn.find(".thm") != std::string::npos && fn.find(".dds") == std::string::npos)
				{
					thms.push_back(&file_path);
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
			}

			xr_task_group tasks;

			constexpr u32 _kTaskBatchSize = 256;

			if (g_imgui_texture_editor.textures.size() > _kTaskBatchSize)
			{
				u32 task_batch_count = (g_imgui_texture_editor.textures.size() - (g_imgui_texture_editor.textures.size() % _kTaskBatchSize)) / _kTaskBatchSize;

				for (u32 i = 0; i < task_batch_count; ++i)
				{
					u32 iter_count = _kTaskBatchSize;
					u32 iter_start = _kTaskBatchSize * i;

					tasks.run([iter_count, iter_start, thms]() {
						for (u32 i = 0; i < iter_count; ++i)
						{
							u32 real_index = i + iter_start;
							CImGuiTextureEditor::STextureEntry& texture = g_imgui_texture_editor.textures[real_index];
							validate_entry(thms, texture);
							++g_imgui_texture_editor.current_analyzed_count;
						}
						});
				}



				if (g_imgui_texture_editor.textures.size() % _kTaskBatchSize != 0)
				{
					u32 iter_count = g_imgui_texture_editor.textures.size() % _kTaskBatchSize;
					u32 iter_start = (g_imgui_texture_editor.textures.size() - (g_imgui_texture_editor.textures.size() % _kTaskBatchSize));

					tasks.run([iter_count, iter_start, thms]() {
						for (u32 i = 0; i < iter_count; ++i)
						{
							u32 real_index = i + iter_start;

							CImGuiTextureEditor::STextureEntry& texture = g_imgui_texture_editor.textures[real_index];
							validate_entry(thms, texture);
							++g_imgui_texture_editor.current_analyzed_count;
						}
						});
				}
			}
			else
			{
				tasks.run([thms]() {
					for (u32 i = 0; i < g_imgui_texture_editor.textures.size(); ++i)
					{
						CImGuiTextureEditor::STextureEntry& texture = g_imgui_texture_editor.textures[i];
						validate_entry(thms, texture);
						++g_imgui_texture_editor.current_analyzed_count;
					}
					});
			}

			tasks.wait();

			g_imgui_texture_editor.is_all_analyzed = true;
		}

		break;
	}
	case CImGuiTextureEditor::eRequestType::kLoadTooltipPreview:
	{
		// for debug purposes
#if 0
		if (g_imgui_texture_editor.pTexturePreview != nullptr)
		{
			g_imgui_texture_editor.is_preview_tooltip_image_loaded = true;
			g_imgui_texture_editor.is_preview_tooltip_image_load_started = false;
			break;
		}
#endif

		if (g_imgui_texture_editor.pTexturePreview)
		{
			g_imgui_texture_editor.pTexturePreview->Release();
			g_imgui_texture_editor.pTexturePreview = nullptr;
		}

		if (g_imgui_texture_editor.pTexturePreviewSRV)
		{
			g_imgui_texture_editor.pTexturePreviewSRV->Release();
			g_imgui_texture_editor.pTexturePreviewSRV = nullptr;
		}

		u32 tex_id = req.payload;

		R_ASSERT(tex_id != u32(-1));

		if (tex_id != u32(-1))
		{
			const CImGuiTextureEditor::STextureEntry& tex = g_imgui_texture_editor.textures[tex_id];

			R_ASSERT(std::string_view(tex.path).empty() == false);
			R_ASSERT(std::string_view(tex.filename).empty() == false);

			string_path subpath;

			if (std::string_view(tex.subpath).empty())
				std::sprintf(subpath, "%s", tex.filename);
			else
			{
				std::filesystem::path builder;
				builder = tex.subpath;
				builder /= tex.filename;

				std::sprintf(subpath, "%s", builder.string().c_str());
			}

			u32 tex_size = 0;
			IRHISurface* pSurface = Render->load_texture(subpath, tex_size);

			if (pSurface)
			{
				g_imgui_texture_editor.pTexturePreview = pSurface;

				if (GRHI->APILevel != D3D9)
				{
					RHIShaderResourceViewDesc desc_srv;
					desc_srv.MipLevels = 1;
					desc_srv.Format = pSurface->GetFormat();
					desc_srv.MostDetailedMip = 0;
					desc_srv.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2D;
					desc_srv.FirstArraySlice = 0;
					desc_srv.ArraySize = 1;
					desc_srv.ElementWidth = 0;

					IRHIShaderResourceView* pView = GRHI->CreateShaderResourceView(pSurface, &desc_srv);

					if (pView)
					{
						g_imgui_texture_editor.pTexturePreviewSRV = pView;
					}
				}
			}

		}

		g_imgui_texture_editor.is_preview_tooltip_image_loaded = true;
		g_imgui_texture_editor.is_preview_tooltip_image_load_started = false;

		break;
	}
	case CImGuiTextureEditor::eRequestType::kUnloadResources:
	{
		if (g_imgui_texture_editor.pTexturePreview)
		{
			g_imgui_texture_editor.pTexturePreview->Release();
			g_imgui_texture_editor.pTexturePreview = nullptr;
		}

		if (g_imgui_texture_editor.pTexturePreviewSRV)
		{
			g_imgui_texture_editor.pTexturePreviewSRV->Release();
			g_imgui_texture_editor.pTexturePreviewSRV = nullptr;
		}

		break;
	}
	case CImGuiTextureEditor::eRequestType::kLoadMetadataOfSelected:
	{

		if (g_imgui_texture_editor.is_init)
		{
			if (g_imgui_texture_editor.is_selected_metadata_loaded == false)
			{
				u32 selected_id = req.payload;

				const CImGuiTextureEditor::STextureEntry& tex = g_imgui_texture_editor.textures[selected_id];

				bool is_loaded = Render->get_texture_metadata(tex.path, &g_imgui_texture_editor.selected_metadata);
				R_ASSERT(is_loaded && "failed to obtain metadata");

				g_imgui_texture_editor.is_selected_metadata_loaded = true;
			}
		}

		break;
	}
	case CImGuiTextureEditor::eRequestType::kLoadPreviewOfSelected:
	{
		if (g_imgui_texture_editor.is_init)
		{
			if (g_imgui_texture_editor.is_selected_preview_loaded == false)
			{
				u32 selected_id = req.payload;

				if (g_imgui_texture_editor.pTextureSelected)
				{
					g_imgui_texture_editor.pTextureSelected->Release();
					g_imgui_texture_editor.pTextureSelected = nullptr;
				}

				if (g_imgui_texture_editor.pTextureSelectedSRV)
				{
					g_imgui_texture_editor.pTextureSelectedSRV->Release();
					g_imgui_texture_editor.pTextureSelectedSRV = nullptr;
				}


				const CImGuiTextureEditor::STextureEntry& tex = g_imgui_texture_editor.textures[selected_id];

				R_ASSERT(std::string_view(tex.path).empty() == false);
				R_ASSERT(std::string_view(tex.filename).empty() == false);

				string_path subpath;

				if (std::string_view(tex.subpath).empty())
					std::sprintf(subpath, "%s", tex.filename);
				else
				{
					std::filesystem::path builder;
					builder = tex.subpath;
					builder /= tex.filename;

					std::sprintf(subpath, "%s", builder.string().c_str());
				}

				u32 tex_size = 0;
				IRHISurface* pSurface = Render->load_texture(subpath, tex_size);

				if (pSurface)
				{
					g_imgui_texture_editor.pTextureSelected = pSurface;

					if (GRHI->APILevel != D3D9)
					{
						RHIShaderResourceViewDesc desc_srv;
						desc_srv.MipLevels = 1;
						desc_srv.Format = pSurface->GetFormat();
						desc_srv.MostDetailedMip = 0;
						desc_srv.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2D;
						desc_srv.FirstArraySlice = 0;
						desc_srv.ArraySize = 1;
						desc_srv.ElementWidth = 0;

						IRHIShaderResourceView* pView = GRHI->CreateShaderResourceView(pSurface, &desc_srv);

						if (pView)
						{
							g_imgui_texture_editor.pTextureSelectedSRV = pView;
						}
					}
				}

				g_imgui_texture_editor.is_selected_preview_loaded = true;
			}
		}

		break;
	}
	case CImGuiTextureEditor::eRequestType::kLoadTHMOfSelected:
	{
		if (g_imgui_texture_editor.is_init)
		{
			if (g_imgui_texture_editor.is_selected_thm_data_loaded == false)
			{
				u32 selected_id = req.payload;

				g_imgui_texture_editor.is_selected_thm_data_loaded = true;
			}
		}

		break;
	}
	case CImGuiTextureEditor::eRequestType::kFilterQuery:
	{
		if (g_imgui_texture_editor.is_all_analyzed == false)
		{
			g_imgui_texture_editor.is_filter_processing = false;
			break;
		}

		CImGuiTextureEditor::eFilterQueryType ft = static_cast<CImGuiTextureEditor::eFilterQueryType>(req.payload);

		switch (ft)
		{
		case CImGuiTextureEditor::eFilterQueryType::kSearch:
		{
			g_imgui_texture_editor.is_filter_processing = true;

			std::string_view buf = g_imgui_texture_editor.search_input_buffer;

			if (buf.empty() == false)
			{
				g_imgui_texture_editor.filter_query.clear();

				u32 i = 0;
				for (const auto& t : g_imgui_texture_editor.textures)
				{
					std::string_view filename = t.filename;

					R_ASSERT(filename.empty() == false && "should be not empty at all!");

					if (filename.find(buf) != std::string_view::npos)
					{
						g_imgui_texture_editor.filter_query.push_back(i);
					}

					++i;
				}

			}
			else
			{
				if (g_imgui_texture_editor.filter_query.size() != g_imgui_texture_editor.textures.size())
				{
					g_imgui_texture_editor.filter_query.clear();

					u32 i = 0;

					for (const auto& t : g_imgui_texture_editor.textures)
					{
						g_imgui_texture_editor.filter_query.push_back(i);
						++i;
					}
				}
			}


			break;
		}
		case CImGuiTextureEditor::eFilterQueryType::kInvalidFirst:
		{
			g_imgui_texture_editor.is_filter_processing = true;

			g_imgui_texture_editor.filter_query.clear();

			u32 i = 0;
			for (const auto& t : g_imgui_texture_editor.textures)
			{
				if (t.is_valid() == false)
				{
					g_imgui_texture_editor.filter_query.push_back(i);
				}
				++i;
			}

			i = 0;
			for (const auto& t : g_imgui_texture_editor.textures)
			{
				if (t.is_valid())
				{
					g_imgui_texture_editor.filter_query.push_back(i);
				}
				++i;
			}

			break;
		}
		case CImGuiTextureEditor::eFilterQueryType::kInvalidFirstExisted:
		{
			g_imgui_texture_editor.is_filter_processing = true;

			std::sort(g_imgui_texture_editor.filter_query.begin(), g_imgui_texture_editor.filter_query.end(), [](const u32& left_id, const u32& right_id)->bool {

				const CImGuiTextureEditor::STextureEntry& left = g_imgui_texture_editor.textures[left_id];
				const CImGuiTextureEditor::STextureEntry& right = g_imgui_texture_editor.textures[right_id];

				return left.is_valid() == false && right.is_valid() == true;

				});

			break;
		}
		case CImGuiTextureEditor::eFilterQueryType::kNoFilter:
		{
			g_imgui_texture_editor.is_filter_processing = true;
			g_imgui_texture_editor.filter_query.clear();

			u32 i = 0;

			for (const auto& t : g_imgui_texture_editor.textures)
			{
				g_imgui_texture_editor.filter_query.push_back(i);
				++i;
			}

			break;
		}
		default:
		{
			R_ASSERT2(false, "some mistake was made by user or code execution...");
			break;
		}
		}

		g_imgui_texture_editor.is_filter_processing = false;

		break;
	}
	default:
	{
		R_ASSERT2(false, "some mistake was made by user or code execution...");
		break;
	}
	}
}

void PrintErrorStatus(bool status)
{
	ImGui::SameLine();
	ImVec4 color;
	const char* status_name = "";
	if (status)
	{
		color = ImVec4(0.1f, 0.9f, 0.1f, 1.0f);
		status_name = "OK";
	}
	else
	{
		color = ImVec4(1.0f, 0.0f, 0.0f, 1.0f);
		status_name = "ERROR";
	}

	ImGui::TextColored(color, status_name);
}

void DrawPreview(IRHISurface* pTexture, IRHIShaderResourceView* pView)
{
	if (pTexture)
	{
		ImVec2 preview_size = kTextureEditor_PreviewSizeHigh;

		u32 texture_size = std::max(pTexture->GetWidth(), pTexture->GetHeight());

		if (texture_size < preview_size.x && texture_size > kTextureEditor_PreviewSizeLow.x)
		{
			preview_size = kTextureEditor_PreviewSizeMid;
		}
		else if (texture_size < kTextureEditor_PreviewSizeMid.x)
		{
			preview_size = kTextureEditor_PreviewSizeLow;
		}

		if (GRHI->APILevel == D3D9)
		{
			if (pTexture->GetRawTexture())
			{
				ImGui::Image(pTexture->GetRawTexture(), preview_size);
			}
		}
		else if (GRHI->APILevel == D3D11)
		{
			if (pView && pView->GetRawSRV())
			{
				ImGui::Image(pView->GetRawSRV(), preview_size);
			}
		}
	}
}

void RenderTextureEditor()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_TextureEditor)])
		return;

	if (g_imgui_texture_editor.is_init == false)
	{
		g_imgui_texture_editor.search_input_buffer[0] = 0;

		constexpr u32 _kReserve = 4096 * 4;

		g_imgui_texture_editor.textures.reserve(_kReserve);
		g_imgui_texture_editor.filter_query.reserve(_kReserve);

		ime_request_t req;

		req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
		req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kReadSettings);

		g_imgui_editors_state.requests.push(req);

		req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kReadAll);
		g_imgui_editors_state.requests.push(req);

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
					ImGui::SetItemTooltip("analyzed: %d", g_imgui_texture_editor.current_analyzed_count.load());
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

					if (ImGui::Checkbox("Show invalid first", &g_imgui_texture_editor.settings.show_invalid_first))
					{
						if (g_imgui_texture_editor.settings.show_invalid_first)
						{
							if (g_imgui_texture_editor.search_input_buffer[0] != 0)
							{
								ime_request_t req;

								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kFilterQuery);
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kSearch);

								g_imgui_editors_state.requests.push(req);

								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kInvalidFirstExisted);
								g_imgui_editors_state.requests.push(req);
							}
							else
							{
								ime_request_t req;
								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kFilterQuery);

								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kInvalidFirst);

								g_imgui_editors_state.requests.push(req);
							}
						}
						else
						{
							ime_request_t req;

							req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
							req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kFilterQuery);

							req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kNoFilter);

							g_imgui_editors_state.requests.push(req);

							if (g_imgui_texture_editor.search_input_buffer[0] != 0)
							{
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kSearch);
								g_imgui_editors_state.requests.push(req);
							}
						}
					}

					ImGui::Checkbox("Show only dds and thm in stats", &g_imgui_texture_editor.settings.show_only_dds_and_thm);

					ImGui::SeparatorText("Search");

					if (ImGui::InputText("##SearchInput",
						g_imgui_texture_editor.search_input_buffer,
						sizeof(g_imgui_texture_editor.search_input_buffer)
					))
					{
						if (g_imgui_texture_editor.search_frame_count > 0)
						{
							g_imgui_texture_editor.search_frame_count = 0;
						}

						if (g_imgui_texture_editor.search_frame_count == 0)
						{
							++g_imgui_texture_editor.search_frame_count;
						}
					}

					if (g_imgui_texture_editor.search_frame_count > 0)
					{
						++g_imgui_texture_editor.search_frame_count;

						if (g_imgui_texture_editor.search_frame_count > 5)
						{
							g_imgui_texture_editor.search_frame_count = 0;

							ime_request_t req;

							req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
							req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kFilterQuery);

							req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kSearch);

							g_imgui_editors_state.requests.push(req);

							if (g_imgui_texture_editor.settings.show_invalid_first)
							{
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kInvalidFirstExisted);
								g_imgui_editors_state.requests.push(req);
							}
						}
					}

					if (g_imgui_texture_editor.is_settings_applied == false)
					{
						if (g_imgui_texture_editor.settings.show_invalid_first)
						{
							ime_request_t req;

							req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
							req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kFilterQuery);

							req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kInvalidFirst);

							g_imgui_editors_state.requests.push(req);
						}

						g_imgui_texture_editor.is_settings_applied = true;
					}

					ImGui::SameLine();

					ImGui::Separator();

					if (g_imgui_texture_editor.is_filter_processing == false)
					{
						constexpr const char* _kColumnNames[] = {
							"Name",
							"Status"
						};

						constexpr u32 _kColumnsSize = sizeof(_kColumnNames) / sizeof(_kColumnNames[0]);

						ImGui::BeginTable("##TETV", _kColumnsSize);

						for (u32 i = 0; i < _kColumnsSize; ++i)
						{
							ImGui::TableSetupColumn(_kColumnNames[i]);
						}

						ImGui::TableHeadersRow();

						ImGuiListClipper clipper;

						clipper.Begin(u32(g_imgui_texture_editor.filter_query.size()));

						bool was_tooltip_shown = false;

						while (clipper.Step())
						{
							for (u32 row = clipper.DisplayStart; row < clipper.DisplayEnd; ++row)
							{
								ImGui::TableNextRow();


								for (size_t column = 0; column < _kColumnsSize; ++column)
								{
									ImGui::TableSetColumnIndex((int)column);


									const CImGuiTextureEditor::STextureEntry& texture = g_imgui_texture_editor.textures[g_imgui_texture_editor.filter_query[row]];

									switch (column)
									{
									case 0:
									{
										char sel_name[sizeof(CImGuiTextureEditor::STextureEntry::path)];
										std::sprintf(sel_name, "[%d] %s", row + 1, texture.filename);

										bool selected_status = g_imgui_texture_editor.selected_index == g_imgui_texture_editor.filter_query[row];

										if (ImGui::Selectable(sel_name, selected_status))
										{
											g_imgui_texture_editor.window_selected_name[0] = 0;
											g_imgui_texture_editor.selected_index = g_imgui_texture_editor.filter_query[row];

											g_imgui_texture_editor.is_selected_metadata_loaded = false;
											g_imgui_texture_editor.is_selected_preview_loaded = false;
											g_imgui_texture_editor.is_selected_thm_data_loaded = false;

											ime_request_t req;

											req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
											req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kLoadMetadataOfSelected);
											req.payload = g_imgui_texture_editor.filter_query[row];

											g_imgui_editors_state.requests.push(req);

											req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kLoadPreviewOfSelected);
											req.payload = g_imgui_texture_editor.filter_query[row];

											g_imgui_editors_state.requests.push(req);

											std::strcat(g_imgui_texture_editor.window_selected_name, "Selected - ");

											std::filesystem::path temp = g_imgui_texture_editor.textures[g_imgui_texture_editor.filter_query[row]].path;

											std::strcat(g_imgui_texture_editor.window_selected_name, temp.filename().string().c_str());
										}

										if (ImGui::BeginItemTooltip())
										{
											was_tooltip_shown = true;

											constexpr const char* _kColumnNamesTooltipTable[] = {
												"File name",
												"Folder path",
												"Preview",
												//												"Full path"
											};

											constexpr u32 _kColumnsSizeTooltipTable = sizeof(_kColumnNamesTooltipTable) / sizeof(_kColumnNamesTooltipTable[0]);

											if (_kColumnsSizeTooltipTable < 4)
												ImGui::Text("Full path: %s", texture.path);

											ImGui::BeginTable("##TTTETV", _kColumnsSizeTooltipTable);

											for (u32 i_tt = 0; i_tt < _kColumnsSizeTooltipTable; ++i_tt)
											{
												ImGui::TableSetupColumn(_kColumnNamesTooltipTable[i_tt]);
											}

											ImGui::TableHeadersRow();

											ImGui::TableNextRow();

											for (u32 column_tt = 0; column_tt < _kColumnsSizeTooltipTable; ++column_tt)
											{
												ImGui::TableSetColumnIndex((int)column_tt);

												switch (column_tt)
												{
												case 0:
												{
													ImGui::Text("%s", texture.filename);
													break;
												}
												case 1:
												{
													if (texture.subpath[0] != 0)
													{
														ImGui::Text("%s", texture.subpath);
													}
													break;
												}
												case 2:
												{
#if 1
													if (g_imgui_texture_editor.is_preview_tooltip_image_loaded)
													{
														DrawPreview(g_imgui_texture_editor.pTexturePreview, g_imgui_texture_editor.pTexturePreviewSRV);
													}
													else
													{
														if (g_imgui_texture_editor.is_preview_tooltip_image_load_started == false)
														{
															ime_request_t req;

															req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
															req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kLoadTooltipPreview);
															req.payload = g_imgui_texture_editor.filter_query[row];

															g_imgui_editors_state.requests.push(req);

															g_imgui_texture_editor.is_preview_tooltip_image_load_started = true;
														}

														ImGui::Text("Loading. . .");
													}
#endif

													break;
												}
												case 3:
												{
													ImGui::Text("%s", texture.path);

													break;
												}
												}
											}

											ImGui::EndTable();

											ImGui::EndTooltip();
										}

										break;
									}
									case 1:
									{

										ImVec4 status_color = _kColorValid;
										bool is_valid = true;
										const char* pStatusName = "valid";

										if (
											(
												texture.analyze_status_result_flags == 0 ||
												(

													(texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kTooLongPath) == CImGuiTextureEditor::eAnalyzedStatus::kTooLongPath ||
													(texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid) == CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid ||
													(texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2) == CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2 ||
													(texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps) == CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps))
											&&
											(!((texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kIgnoreTHM) == CImGuiTextureEditor::eAnalyzedStatus::kIgnoreTHM))
											)
										{
											status_color = _kColorInvalid;
											is_valid = false;
											pStatusName = "invalid";
										}

										ImGui::TextColored(status_color, "%s", pStatusName);
										break;
									}
									}
								}
							}
						}

						clipper.End();

						if (was_tooltip_shown == false)
						{
							if (g_imgui_texture_editor.is_preview_tooltip_image_loaded)
								g_imgui_texture_editor.is_preview_tooltip_image_loaded = false;

							if (g_imgui_texture_editor.is_preview_tooltip_image_load_started)
								g_imgui_texture_editor.is_preview_tooltip_image_load_started = false;
						}

						ImGui::EndTable();
					}
					else
					{
						ImGui::Text("Filtering...");
					}



				}
				else
				{
					if (g_imgui_texture_editor.total_files_in_folder == 0)
					{
						ImGui::Text("Preparing...");
					}
					else if (g_imgui_texture_editor.current_analyzed_count == 0)
					{
						ImGui::Text("Found: [%s]",
							g_imgui_texture_editor.wt_current_analyzing_texture.data()
						);
					}
					else
					{
						ImGui::Text("Analyzing: %zu/%zu",
							g_imgui_texture_editor.current_analyzed_count.load(),
							g_imgui_texture_editor.textures.size()
						);

						ImGui::Text("[%s]",
							g_imgui_texture_editor.wt_current_analyzing_texture.data()
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
			using texture_t = CImGuiTextureEditor::STextureEntry;

			const texture_t& selected = g_imgui_texture_editor.textures[g_imgui_texture_editor.selected_index];


			if (ImGui::Button("Save - THM"))
			{

			}

			ImGui::Separator();

			ImGui::Text("Validation status:");

			ImGui::Text("\t- Has THM: ");

			PrintErrorStatus(true);


			ImGui::Text("\t- THM is valid: ");

			PrintErrorStatus(true);

			ImGui::Text("\t- Dimensions power of 2: ");


			PrintErrorStatus(true);

			ImGui::Text("\t- Has mip maps: ");


			PrintErrorStatus(true);

			ImGui::Separator();

			ImGui::Text("%s", selected.path);

			constexpr const char* _kColumnTableSelectedNames[] = {
				"Info",
				"Settings",
				"Preview"
			};

			constexpr u32 _kColumnTableSelectedSize = sizeof(_kColumnTableSelectedNames) / sizeof(_kColumnTableSelectedNames[0]);

			ImGui::BeginTable("##TETV2", _kColumnTableSelectedSize);

			for (u32 i = 0; i < _kColumnTableSelectedSize; ++i)
			{
				ImGui::TableSetupColumn(_kColumnTableSelectedNames[i]);
			}

			ImGui::TableHeadersRow();

			ImGui::TableNextRow();

			for (u32 column_id = 0; column_id < _kColumnTableSelectedSize; ++column_id)
			{
				ImGui::TableSetColumnIndex(u32(column_id));

				switch (column_id)
				{
				case 0:
				{

					if (g_imgui_texture_editor.is_selected_metadata_loaded)
					{
						ImGui::Text("Width: %d", g_imgui_texture_editor.selected_metadata.width);
						ImGui::Text("Height: %d", g_imgui_texture_editor.selected_metadata.height);

						if (GRHI->APILevel == D3D9 || GRHI->APILevel == D3D11)
						{
#ifdef IXR_WINDOWS
							xr_string_view casted_enum = magic_enum::enum_name((DXGI_FORMAT)g_imgui_texture_editor.selected_metadata.format);
							ImGui::Text("Format: %s", casted_enum.data());
#endif
						}
						else
						{
							R_ASSERT(false, "todo: others -> provide implemenetation");
						}
					}
					else
					{
						ImGui::Text("Loading...");
					}

					break;
				}
				case 1:
				{

					if (g_imgui_texture_editor.is_selected_thm_data_loaded)
					{

					}
					else
					{
						ImGui::Text("Loading...");
					}

					break;
				}
				case 2:
				{
					if (g_imgui_texture_editor.is_selected_preview_loaded)
					{
						DrawPreview(g_imgui_texture_editor.pTextureSelected, g_imgui_texture_editor.pTextureSelectedSRV);
					}
					else
					{
						ImGui::Text("Loading...");
					}

					break;
				}
				}
			}

			ImGui::EndTable();
		}

		ImGui::End();
	}



}
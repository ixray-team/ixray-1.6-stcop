
#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"
#include "../xrEngine/ETextureParams.h"
#include "../player_hud.h"
#include "ai_space.h"
#include "../../xrUI/ui_base.h"
#include "ImUtils.h"
#include "../game_news.h"

#ifdef IXR_WINDOWS
#include <dxgiformat.h>
#else
#endif


CImGuiTextureEditor g_imgui_texture_editor;

constexpr decltype(CImGuiTextureEditor::selected_index) _kInvalidSelectedID = decltype(CImGuiTextureEditor::selected_index)(-1);

constexpr u16 kTextureEditor_PreviewsVersion = sizeof(string_path) / sizeof(std::remove_extent_t<string_path>);
constexpr u16 kTextureEditor_PreviewsEntrySize = kTextureEditor_PreviewsVersion + sizeof(u64);

constexpr ImVec2 kTextureEditor_PreviewSizeHigh = ImVec2(512.0f, 512.0f);
constexpr ImVec2 kTextureEditor_PreviewSizeMid = ImVec2(256.0f, 256.0f);
constexpr ImVec2 kTextureEditor_PreviewSizeLow = ImVec2(64.0f, 64.0f);

void RequestHandler_TextureEditor(const SRequestData& req)
{
	if (!xr_FS)
		return;

	R_ASSERT2(static_cast<eImGuiEditorType>(req.editor_type) == eImGuiEditorType::kTextureEditor, "invalid data came you should debug code");

	if (static_cast<eImGuiEditorType>(req.editor_type) != eImGuiEditorType::kTextureEditor)
		return;

	switch (static_cast<eRequestType_TextureEditor>(req.request_type))
	{
	case eRequestType_TextureEditor::kReadSettings:
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
	case eRequestType_TextureEditor::kWriteSettings:
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
	case eRequestType_TextureEditor::kReadAll:
	{
		if (g_imgui_texture_editor.is_all_analyzed)
			g_imgui_texture_editor.is_all_analyzed = false;

		if (!g_imgui_texture_editor.is_all_analyzed)
		{
			static xr_vector<const char*> files_vec;
			static FS_Path* pTexturesFolder = FS.get_path(_game_textures_);

			if (!pTexturesFolder)
			{
				g_imgui_texture_editor.is_all_analyzed = true;
				Msg("[TextureEditor]: ! invalid filesystem was initialized on your side -> report to developers!");
				break;
			}
			else
			{
				PROF_EVENT("get_all_files");
				FS.get_all_files_in_dir(files_vec, pTexturesFolder->m_Path);
				g_imgui_texture_editor.total_files_in_folder = files_vec.size();
			}

			g_imgui_texture_editor.textures.clear();
			g_imgui_texture_editor.filter_query.clear();

			g_imgui_texture_editor.valid_count = 0;
			g_imgui_texture_editor.current_analyzed_count = 0;
			g_imgui_texture_editor.total_textures_in_folder = 0;
			g_imgui_texture_editor.total_thm_in_folder = 0;
			g_imgui_texture_editor.total_seq_in_folder = 0;
			g_imgui_texture_editor.total_png_in_folder = 0;
			g_imgui_texture_editor.total_svg_in_folder = 0;
			g_imgui_texture_editor.total_bmp_in_folder = 0;
			g_imgui_texture_editor.total_ogm_in_folder = 0;
			g_imgui_texture_editor.total_ini_in_folder = 0;
			g_imgui_texture_editor.total_unable_to_classify_files_in_folder = 0;
			g_imgui_texture_editor.total_other_in_folder = 0;

			{
				PROF_EVENT("get_all_textures_count");
				for (const char* path : files_vec)
				{
					std::string_view fn = path;
					size_t len = fn.length();
					if (len >= 4 && fn.compare(len - 4, 4, ".dds") == 0)
						++g_imgui_texture_editor.total_textures_in_folder;
				}
			}

			PROF_EVENT("load_and_validate_all");
			xr_parallel_for(0ULL, files_vec.size(),
				[](size_t index)
				{
					std::string_view fn = files_vec[index];
					size_t len = fn.length();

					if (len < 4)
					{
						++g_imgui_texture_editor.total_other_in_folder;
						return;
					}

					bool is_dds = (len >= 4 && fn.compare(len - 4, 4, ".dds") == 0);

					if (is_dds)
					{
						g_imgui_texture_editor.wt_current_analyzing_texture = fn;
						CImGuiTextureEditor::STextureEntry entry;
						entry.analyze_status_result_flags = 0;

						xr_strcpy(entry.path, fn.data());

						if (fn.size() > sizeof(string_path))
							entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kTooLongPath;
						else
						{
							size_t slash_pos = fn.find_last_of("/\\");
							if (slash_pos != xr_string::npos)
								xr_strcpy(entry.filename, fn.data() + slash_pos + 1);
							else
								xr_strcpy(entry.filename, fn.data());

							if (slash_pos != xr_string::npos)
							{
								std::filesystem::path relative = std::filesystem::relative(
									xr_string(fn.data()).substr(0, slash_pos).c_str(), pTexturesFolder->m_Path);

								if (relative != ".")
									xr_strcpy(entry.subpath, relative.string().c_str());
								else
									entry.subpath[0] = 0;
							}
							else
								entry.subpath[0] = 0;

							++g_imgui_texture_editor.valid_count;
						}

						//try to load thm
						*strext(entry.path) = 0;
						xr_strcat(entry.path, ".thm");
						if (FS.exist(entry.path))
						{
							entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kHasTHM;

							STextureParams temp_param;
							IReader* pReader = FS.r_open(entry.path);

							if (pReader)
							{
								bool thm_invalid = temp_param.Load(*pReader);
								if (thm_invalid)
									entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid;
								pReader->close();
							}
							else
								entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid;
						}
						else
						{
							std::string_view path_view = entry.path;

							if (path_view.find("bump#") != std::string_view::npos)
								entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kIgnoreTHM;
						}

						//try to load dds
						*strext(entry.path) = 0;
						xr_strcat(entry.path, ".dds");

						RHITextureMetadata mt;
						if (Render->get_texture_metadata(entry.path, &mt))
						{
							if (!(mt.width % 2 == 0 && mt.height % 2 == 0))
								entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2;

							if (mt.mipmap_count == 1)
								entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps;
						}
						else
						{
							entry.analyze_status_result_flags |= CImGuiTextureEditor::eAnalyzedStatus::kInvalidMetadata;
						}

						g_imgui_texture_editor.textures.push_back(std::move(entry));
						g_imgui_texture_editor.current_analyzed_count++;
					}
					else if (len >= 4 && fn.compare(len - 4, 4, ".thm") == 0)
						++g_imgui_texture_editor.total_thm_in_folder;
					else
					{
						bool classified = false;
						struct ExtensionInfo
						{
							const char* ext;
							std::atomic<u32>& counter;
						};

						const ExtensionInfo other_extensions[]
						{
							{".seq", g_imgui_texture_editor.total_seq_in_folder},
							{".png", g_imgui_texture_editor.total_png_in_folder},
							{".svg", g_imgui_texture_editor.total_svg_in_folder},
							{".bmp", g_imgui_texture_editor.total_bmp_in_folder},
							{".ogm", g_imgui_texture_editor.total_ogm_in_folder},
							{".ini", g_imgui_texture_editor.total_ini_in_folder}
						};

						for (const auto& ext_info : other_extensions)
						{
							size_t ext_len = strlen(ext_info.ext);
							if (len >= ext_len && fn.compare(len - ext_len, ext_len, ext_info.ext) == 0)
							{
								ext_info.counter++;
								classified = true;
								break;
							}
						}

						if (!classified)
						{
							if (fn.find(".dds") != xr_string::npos && fn.find(".thm") != xr_string::npos)
								g_imgui_texture_editor.total_unable_to_classify_files_in_folder++;
							else
								g_imgui_texture_editor.total_other_in_folder++;
						}
					}
				});

			if (!g_imgui_texture_editor.textures.empty())
			{
				std::sort(g_imgui_texture_editor.textures.begin(),
					g_imgui_texture_editor.textures.end(),
					[](const CImGuiTextureEditor::STextureEntry& a, const CImGuiTextureEditor::STextureEntry& b) {
						return xr_strcmp(a.path, b.path) < 0;
					});

				g_imgui_texture_editor.filter_query.resize(g_imgui_texture_editor.textures.size());
				for (u32 i = 0; i < g_imgui_texture_editor.textures.size(); ++i)
					g_imgui_texture_editor.filter_query[i] = i;
			}
			g_imgui_texture_editor.is_all_analyzed = true;

		}

		break;
	}
	case eRequestType_TextureEditor::kLoadTooltipPreview:
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
			R_ASSERT(g_imgui_texture_editor.is_preview_tooltip_image_loaded == false && "logic execution is corrupted, expected false. Debug your code");

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

			g_imgui_texture_editor.current_tooltip_texture_filename = tex.filename;

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
	case eRequestType_TextureEditor::kLoadTooltipMetadata:
	{
		if (g_imgui_texture_editor.is_init)
		{
			if (g_imgui_texture_editor.is_metadata_tooltip_loaded == false)
			{
				const CImGuiTextureEditor::STextureEntry& tex = g_imgui_texture_editor.textures[req.payload];

				if (FS.exist(tex.path))
				{
					bool failed_to_load = Render->get_texture_metadata(tex.path, &g_imgui_texture_editor.tooltip_metadata);
					if (!failed_to_load)
					{
						g_imgui_texture_editor.tooltip_metadata.width = _kInvalidSelectedID;
						g_imgui_texture_editor.tooltip_metadata.height = _kInvalidSelectedID;
						g_imgui_texture_editor.tooltip_metadata.mipmap_count = _kInvalidSelectedID;
						g_imgui_texture_editor.tooltip_metadata.format = _kInvalidSelectedID;
					}
				}

				g_imgui_texture_editor.is_metadata_tooltip_loaded = true;
			}
		}

		break;
	}
	case eRequestType_TextureEditor::kShutdown:
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
	case eRequestType_TextureEditor::kLoadMetadataOfSelected:
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
	case eRequestType_TextureEditor::kLoadPreviewOfSelected:
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
	case eRequestType_TextureEditor::kLoadTHMOfSelected:
	{
		if (g_imgui_texture_editor.is_init)
		{
			if (g_imgui_texture_editor.is_selected_thm_data_loaded == false)
			{
				if (g_imgui_texture_editor.pTHMSelected == nullptr)
				{
					g_imgui_texture_editor.pTHMSelected = new STextureParams();
				}

				if (g_imgui_texture_editor.pTHMSelected)
				{
					g_imgui_texture_editor.pTHMSelected->Clear();
				}

				u32 selected_id = req.payload;

				const CImGuiTextureEditor::STextureEntry& tex = g_imgui_texture_editor.textures[selected_id];

				R_ASSERT(std::string_view(tex.path).empty() == false && "must be valid here");

				std::filesystem::path thm_path = tex.path;
				thm_path.replace_extension(".thm");

				const auto& casted_thm_path = thm_path.string();
				if (FS.exist(casted_thm_path.c_str()))
				{
					IReader* pReader = FS.r_open(casted_thm_path.c_str());

					if (pReader)
					{
						if (g_imgui_texture_editor.pTHMSelected)
						{
							bool load_status = g_imgui_texture_editor.pTHMSelected->Load(*pReader);

							if (load_status == false)
							{
								g_imgui_texture_editor.pTHMSelected->Clear();
							}
						}


						pReader->close();
					}
				}

				g_imgui_texture_editor.is_selected_thm_data_loaded = true;
			}
		}

		break;
	}
	case eRequestType_TextureEditor::kDeselectCurrentSelected:
	{
		if (g_imgui_texture_editor.is_init)
		{
			bool is_main_only_visible = false;

			if (g_imgui_texture_editor.selected_index != _kInvalidSelectedID)
			{
				g_imgui_texture_editor.selected_index = _kInvalidSelectedID;
			}
			else
			{
				is_main_only_visible = true;
			}

			if (is_main_only_visible)
			{
				Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_TextureEditor)] = false;
			}

		}
	}
	case eRequestType_TextureEditor::kFilterQuery:
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

void PrintIgnoreStatus(const char* pName, const ImVec4& color)
{
	ImGui::SameLine();
	ImGui::TextColored(color, pName);
}

void PrintErrorStatus(
	bool status,
	const char* pOverrideValidStatus = "OK",
	const char* pOverrideInvalidStatus = "ERROR"
)
{
	ImGui::SameLine();
	ImVec4 color;
	const char* status_name = "";

	if (status)
	{
		color = ImVec4(0.1f, 0.9f, 0.1f, 1.0f);
		status_name = pOverrideValidStatus;
		R_ASSERT(pOverrideValidStatus);
	}
	else
	{
		color = ImVec4(1.0f, 0.0f, 0.0f, 1.0f);
		status_name = pOverrideInvalidStatus;
		R_ASSERT(pOverrideInvalidStatus);
	}

	ImGui::TextColored(color, status_name);
}

void PrintHelp()
{
	// Get available content region width
	float availableWidth = ImGui::GetContentRegionAvail().x;

	// Calculate text width
	constexpr const char* text = "(?)";
	float textWidth = ImGui::CalcTextSize(text).x;

	// Move cursor to right position
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + availableWidth - textWidth);

	// Draw text
	ImGui::Text("%s", text);

	if (ImGui::IsItemHovered())
	{
		if (ImGui::BeginItemTooltip())
		{
			ImGui::Text("Description:");
			ImGui::Text("\tEditor for previewing textures and editing thms");

			ImGui::EndTooltip();
		}
	}
}

template<typename T, u32 TokenSize>
void ListBoxToken(T(&p_token)[TokenSize], const char* pListBoxName, u32* p_data)
{
	if (p_token == nullptr)
		return;

	if (TokenSize == 0)
		return;

	if (p_data == nullptr)
		return;

	int current_item = 0;

	u32 arr_size = TokenSize - 1;
	// if slow just make lookup table in that case just make another argument where it will make index of p_token array by value of p_data (but shouldn't)
	for (u32 i = 0; i < arr_size; ++i)
	{
		const xr_token& token = p_token[i];

		if (token.id == static_cast<int>(*p_data))
		{
			current_item = i;
			break;
		}
	}

	const char* token_names[TokenSize];

	for (u32 i = 0; i < arr_size; ++i)
	{
		token_names[i] = p_token[i].name;
	}

	bool was_changed = ImGui::Combo(pListBoxName, &current_item, token_names, arr_size);

	if (was_changed)
	{
		R_ASSERT(p_data);
		if (p_data)
			(*p_data) = (u32)p_token[current_item].id;
	}
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

		AllEditors_SendRequests_Sequential(xr_array{
			SRequestData{.editor_type = u32(eImGuiEditorType::kTextureEditor),
			.request_type = u32(eRequestType_TextureEditor::kReadSettings)},
			SRequestData{.editor_type = u32(eImGuiEditorType::kTextureEditor), .request_type = u32(eRequestType_TextureEditor::kReadAll)}
			});

		g_imgui_texture_editor.window_selected_name[0] = 0;
		std::strcat(g_imgui_texture_editor.window_selected_name, "Selected##TE");

		g_imgui_texture_editor.is_init = true;
	}

	bool is_in_game = g_actor && ai().get_alife() && g_pGameLevel;

	bool checkbox_about_treat_settings_was_updated = false;

	if (ImGui::Begin("Texture Editor", 0, ImGuiWindowFlags_AlwaysAutoResize))
	{
		PrintHelp();

		if (ImGui::BeginTabBar("##TETB"))
		{
			if (ImGui::BeginTabItem("All"))
			{
				if (g_imgui_texture_editor.is_all_analyzed)
				{
					constexpr ImVec4 _kColorValid = ImVec4(0.0f, 0.8f, 0.0f, 1.0f);
					constexpr ImVec4 _kColorInvalid = ImVec4(0.8f, 0.0f, 0.0f, 1.0f);

					ImGui::SeparatorText("Stats");
					ImGui::Text("Total files: %d", g_imgui_texture_editor.total_files_in_folder.load());
					ImGui::Text("\t- .dds: %d", g_imgui_texture_editor.total_textures_in_folder.load());
					ImGui::SetItemTooltip("analyzed: %d", g_imgui_texture_editor.textures.size());
					ImGui::Text("\t- .thm: %d", g_imgui_texture_editor.total_thm_in_folder.load());

					if (g_imgui_texture_editor.settings.show_only_dds_and_thm == false)
					{
						ImGui::Text("\t- .seq: %d", g_imgui_texture_editor.total_seq_in_folder.load());
						ImGui::Text("\t- .png: %d", g_imgui_texture_editor.total_png_in_folder.load());
						ImGui::Text("\t- .svg: %d", g_imgui_texture_editor.total_svg_in_folder.load());
						ImGui::Text("\t- .bmp: %d", g_imgui_texture_editor.total_bmp_in_folder.load());
						ImGui::Text("\t- .ogm: %d", g_imgui_texture_editor.total_ogm_in_folder.load());
						ImGui::Text("\t- .ini: %d", g_imgui_texture_editor.total_ini_in_folder.load());
						ImGui::Text("\t- other: %d", g_imgui_texture_editor.total_other_in_folder.load());
						ImGui::SetItemTooltip("extensions that weren't recognizable by engine");
					}

					ImGui::Text("\t- unknown: %d", g_imgui_texture_editor.total_unable_to_classify_files_in_folder.load());
					ImGui::SetItemTooltip("filename contains .thm and .dds\nso we won't try to determine them as thm or dds files");

					ImGui::Text("Valid:");
					ImGui::Text("\t- textures: %d", g_imgui_texture_editor.valid_count.load());
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
								AllEditors_SendRequests_Sequential(xr_array{
									SRequestData{
										.editor_type = u32(eImGuiEditorType::kTextureEditor),
										.request_type = u32(eRequestType_TextureEditor::kFilterQuery),
										.payload = u32(CImGuiTextureEditor::eFilterQueryType::kSearch)
									},
									SRequestData{
										.editor_type = u32(eImGuiEditorType::kTextureEditor),
										.request_type = u32(eRequestType_TextureEditor::kFilterQuery),
										.payload = u32(CImGuiTextureEditor::eFilterQueryType::kInvalidFirstExisted)
									}
									});
							}
							else
							{
								SRequestData req;

								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(eRequestType_TextureEditor::kFilterQuery);
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kInvalidFirst);

								AllEditors_SendRequest(req);
							}
						}
						else
						{
							if (g_imgui_texture_editor.search_input_buffer[0] != 0)
							{
								SRequestData req;

								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(eRequestType_TextureEditor::kFilterQuery);
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kNoFilter);

								AllEditors_SendRequests_Sequential(xr_array{
									req,
									SRequestData{.editor_type = u32(eImGuiEditorType::kTextureEditor),
									.request_type = u32(eRequestType_TextureEditor::kFilterQuery),
									.payload = u32(CImGuiTextureEditor::eFilterQueryType::kSearch)}
									});
							}
							else
							{
								SRequestData req;

								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(eRequestType_TextureEditor::kFilterQuery);
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kNoFilter);

								AllEditors_SendRequest(req);
							}
						}
					}

					ImGui::Checkbox("Show only dds and thm in stats", &g_imgui_texture_editor.settings.show_only_dds_and_thm);


					constexpr std::string_view _kNoMipMap = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps);
					constexpr std::string_view _kNotEvenDimensions = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2);


					{
						char checkbox_nomipmap_name[32];
						std::sprintf(checkbox_nomipmap_name, "Treat %s as invalid", _kNoMipMap.data());
						if (ImGui::Checkbox(checkbox_nomipmap_name, &g_imgui_texture_editor.settings.treat_nomipmap_as_invalid))
						{
							checkbox_about_treat_settings_was_updated = true;
						}
					}

					{
						char checkbox_notevendimensions_name[32];
						std::sprintf(checkbox_notevendimensions_name, "Treat %s as invalid", _kNotEvenDimensions.data());
						if (ImGui::Checkbox(checkbox_notevendimensions_name, &g_imgui_texture_editor.settings.treat_notpowerof2dimensions_as_invalid))
						{
							checkbox_about_treat_settings_was_updated = true;
						}
					}

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



							if (g_imgui_texture_editor.settings.show_invalid_first)
							{
								SRequestData req;

								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(eRequestType_TextureEditor::kFilterQuery);
								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kSearch);

								AllEditors_SendRequests_Sequential(xr_array{
									req,
									SRequestData{
										.editor_type = u32(eImGuiEditorType::kTextureEditor),
										.request_type = u32(eRequestType_TextureEditor::kFilterQuery),
										.payload = u32(CImGuiTextureEditor::eFilterQueryType::kInvalidFirstExisted)}
									});
							}
							else
							{
								SRequestData req;

								req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
								req.request_type = static_cast<u32>(eRequestType_TextureEditor::kFilterQuery);

								req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kSearch);

								AllEditors_SendRequest(req);
							}
						}
					}

					if (g_imgui_texture_editor.is_settings_applied == false)
					{
						if (g_imgui_texture_editor.settings.show_invalid_first)
						{
							SRequestData req;

							req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
							req.request_type = static_cast<u32>(eRequestType_TextureEditor::kFilterQuery);
							req.payload = static_cast<u32>(CImGuiTextureEditor::eFilterQueryType::kInvalidFirst);

							AllEditors_SendRequest(req);
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
						ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(ImGui::GetStyle().CellPadding.x, 0.0f));
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
											//g_imgui_texture_editor.window_selected_name[0] = 0;
											g_imgui_texture_editor.selected_index = g_imgui_texture_editor.filter_query[row];

											g_imgui_texture_editor.is_selected_metadata_loaded = false;
											g_imgui_texture_editor.is_selected_preview_loaded = false;
											g_imgui_texture_editor.is_selected_thm_data_loaded = false;

											SRequestData req;

											req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
											req.request_type = static_cast<u32>(eRequestType_TextureEditor::kLoadMetadataOfSelected);
											req.payload = g_imgui_texture_editor.filter_query[row];

											AllEditors_SendRequest(req);

											req.request_type = static_cast<u32>(eRequestType_TextureEditor::kLoadPreviewOfSelected);
											req.payload = g_imgui_texture_editor.filter_query[row];

											AllEditors_SendRequest(req);

											req.request_type = static_cast<u32>(eRequestType_TextureEditor::kLoadTHMOfSelected);
											req.payload = g_imgui_texture_editor.filter_query[row];

											AllEditors_SendRequest(req);

											g_imgui_texture_editor.last_window_selected_state.Capture(g_imgui_texture_editor.window_selected_name);

										//	std::strcat(g_imgui_texture_editor.window_selected_name, "Selected");

										//	std::filesystem::path temp = g_imgui_texture_editor.textures[g_imgui_texture_editor.filter_query[row]].path;

										//	std::strcat(g_imgui_texture_editor.window_selected_name, temp.filename().string().c_str());
										}

										if (ImGui::BeginItemTooltip())
										{
											was_tooltip_shown = true;

											constexpr const char* _kColumnNamesTooltipTable[] = {
												"Info",
												"Preview"
												//												"Full path"
											};

											constexpr u32 _kColumnsSizeTooltipTable = sizeof(_kColumnNamesTooltipTable) / sizeof(_kColumnNamesTooltipTable[0]);

											if (_kColumnsSizeTooltipTable < 3)
												ImGui::Text("Full path: %s", texture.path);

											ImGui::BeginTable("##TTTETV", _kColumnsSizeTooltipTable);

											for (u32 i_tt = 0; i_tt < _kColumnsSizeTooltipTable; ++i_tt)
											{
												ImGui::TableSetupColumn(_kColumnNamesTooltipTable[i_tt]);
											}

											ImGui::TableHeadersRow();

											ImGui::TableNextRow();

											if (g_imgui_texture_editor.current_tooltip_texture_filename != texture.filename)
											{
												g_imgui_texture_editor.is_preview_tooltip_image_load_started = false;
												g_imgui_texture_editor.is_preview_tooltip_image_loaded = false;
												g_imgui_texture_editor.is_metadata_tooltip_loaded = false;
											}

											for (u32 column_tt = 0; column_tt < _kColumnsSizeTooltipTable; ++column_tt)
											{
												ImGui::TableSetColumnIndex((int)column_tt);

												switch (column_tt)
												{
												case 0:
												{
													ImGui::Text("name: %s", texture.filename);
													ImGui::Text("folders: %s", texture.subpath);

													if (g_imgui_texture_editor.is_metadata_tooltip_loaded)
													{
														ImGui::Text("width: %d", g_imgui_texture_editor.tooltip_metadata.width);
														ImGui::Text("height: %d", g_imgui_texture_editor.tooltip_metadata.height);
														ImGui::Text("mipmap count: %d", g_imgui_texture_editor.tooltip_metadata.mipmap_count);

#ifdef IXR_WINDOWS
														if (g_imgui_texture_editor.tooltip_metadata.format != _kInvalidSelectedID)
														{
															std::string_view format_name = magic_enum::enum_name((DXGI_FORMAT)g_imgui_texture_editor.tooltip_metadata.format);
															ImGui::Text("format: %s", format_name.data());
														}
#endif
													}

													break;
												}
												case 1:
												{
#if 1
													if (g_imgui_texture_editor.is_preview_tooltip_image_loaded)
													{
														R_ASSERT(g_imgui_texture_editor.current_tooltip_texture_filename.empty() == false && "can't be report to developers");

														DrawPreview(g_imgui_texture_editor.pTexturePreview, g_imgui_texture_editor.pTexturePreviewSRV);
													}
													else
													{
														if (g_imgui_texture_editor.is_preview_tooltip_image_load_started == false)
														{
															SRequestData req;

															req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
															req.request_type = static_cast<u32>(eRequestType_TextureEditor::kLoadTooltipPreview);
															req.payload = g_imgui_texture_editor.filter_query[row];

															AllEditors_SendRequest(req);

															req.request_type = static_cast<u32>(eRequestType_TextureEditor::kLoadTooltipMetadata);

															AllEditors_SendRequest(req);

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

										bool has_dim2 = ((texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2) == CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2);
										bool has_thmisnotvalid = ((texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid) == CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid);
										bool has_nomipmaps = ((texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps) == CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps);
										bool has_hasthm = ((texture.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kHasTHM) == CImGuiTextureEditor::eAnalyzedStatus::kHasTHM);

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

										if (is_valid == false && ImGui::BeginItemTooltip())
										{
											ImGui::SeparatorText("Report");

											std::string_view estr_hasthm = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kHasTHM);
											ImGui::Text("%s: ", estr_hasthm.data());
											PrintErrorStatus(has_hasthm);

											ImGui::BeginDisabled(!has_hasthm);
											std::string_view estr_tnv = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid);
											ImGui::Text("%s: ", estr_tnv.data());
											PrintErrorStatus(!has_thmisnotvalid);
											ImGui::EndDisabled();

											std::string_view estr_p2 = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2);
											ImGui::Text("%s:", estr_p2.data());
											PrintErrorStatus(!has_dim2);

											std::string_view estr_nmm = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps);
											ImGui::Text("%s: ", estr_nmm.data());
											PrintErrorStatus(!has_nomipmaps);

											ImGui::EndTooltip();
										}

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

							if (g_imgui_texture_editor.is_metadata_tooltip_loaded)
								g_imgui_texture_editor.is_metadata_tooltip_loaded = false;

							if (g_imgui_texture_editor.is_preview_tooltip_image_load_started)
								g_imgui_texture_editor.is_preview_tooltip_image_load_started = false;
						}

						ImGui::EndTable();

						ImGui::PopStyleVar();
					}
					else
					{
						ImGui::Text("Filtering...");
					}



				}
				else
				{
					char progressbar_content[32];
					std::sprintf(progressbar_content, "%d/%d",
						g_imgui_texture_editor.current_analyzed_count.load(), 
						g_imgui_texture_editor.total_textures_in_folder.load()
					);
					float progress = float(g_imgui_texture_editor.current_analyzed_count.load()) / float(g_imgui_texture_editor.total_textures_in_folder.load());
					clamp(progress, 0.0f, 1.0f);
					ImGui::ProgressBar(progress, ImVec2(0.0f, 0.0f), progressbar_content);

					ImGui::SameLine(0.0f, ImGui::GetStyle().ItemInnerSpacing.x);
					ImGui::Text("[%s]", g_imgui_texture_editor.wt_current_analyzing_texture.data());
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

	if (checkbox_about_treat_settings_was_updated)
	{
		SRequestData req;

		req.editor_type = u32(eImGuiEditorType::kTextureEditor);
		req.request_type = u32(eRequestType_TextureEditor::kReadAll);

		AllEditors_SendRequest(req);

		return;
	}

	if (g_imgui_texture_editor.selected_index != _kInvalidSelectedID)
	{
		g_imgui_texture_editor.last_window_selected_state.Apply(g_imgui_texture_editor.window_selected_name);

		if (ImGui::Begin(g_imgui_texture_editor.window_selected_name, 0, ImGuiWindowFlags_AlwaysAutoResize))
		{
			using texture_t = CImGuiTextureEditor::STextureEntry;
		
			const texture_t& selected = g_imgui_texture_editor.textures[g_imgui_texture_editor.selected_index];


			if (ImGui::Button("Save - THM"))
			{

			}
			ImGui::SameLine();
			if (ImGui::Button("View - mipmaps"))
			{

			}
			ImGui::SameLine();
			if (ImGui::Button("View - color channels"))
			{

			}

			ImGui::Separator();

			ImGui::Text("Validation status:");


			bool has_dim2 = ((selected.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2) == CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2);
			bool has_thmisnotvalid = ((selected.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid) == CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid);
			bool has_nomipmaps = ((selected.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps) == CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps);
			bool has_hasthm = ((selected.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kHasTHM) == CImGuiTextureEditor::eAnalyzedStatus::kHasTHM);
			bool has_ignorethm = ((selected.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kIgnoreTHM) == CImGuiTextureEditor::eAnalyzedStatus::kIgnoreTHM);

			std::string_view field_name = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kHasTHM);
			ImGui::Text("\t- %s: ", field_name.data());
			PrintErrorStatus((has_ignorethm ? true : has_hasthm));

			field_name = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kTHMIsNotValid);
			ImGui::Text("\t- %s: ", field_name.data());
			PrintErrorStatus(has_thmisnotvalid == false);

			field_name = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kDimensionsNotPowerOf2);
			ImGui::Text("\t- %s: ", field_name.data());
			PrintErrorStatus(has_dim2 == false);

			field_name = magic_enum::enum_name(CImGuiTextureEditor::eAnalyzedStatus::kNoMipMaps);
			ImGui::Text("\t- %s: ", field_name.data());
			PrintErrorStatus(has_nomipmaps == false);

			ImGui::Text("Validation result: ");

			bool is_valid = (has_ignorethm ? true : has_hasthm) &&
				(has_thmisnotvalid == false) &&
				(has_dim2 == false) &&
				(has_nomipmaps == false);

			PrintErrorStatus(is_valid, "ALL GOOD", "INVALID");

			ImGui::Separator();

			ImGui::Text("Full path: [%s]", selected.path);

			constexpr const char* _kColumnTableSelectedNames[] = {
				"Info",
				"THM - Editing",
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
						bool is_invalid_metadata = ((selected.analyze_status_result_flags & CImGuiTextureEditor::eAnalyzedStatus::kInvalidMetadata) == CImGuiTextureEditor::eAnalyzedStatus::kInvalidMetadata);

						if (is_invalid_metadata)
						{
							R_ASSERT(false && "report to developers!");
							ImGui::TextColored(ImVec4(1.0f, 0.1f, 0.1f, 1.0f), "FAILED TO LOAD METADATA, REPORT TO DEVELOPERS!");
						}
						else
						{
							ImGui::Text("Width: %d", g_imgui_texture_editor.selected_metadata.width);
							ImGui::Text("Height: %d", g_imgui_texture_editor.selected_metadata.height);
							ImGui::Text("MipMap Count: %d", g_imgui_texture_editor.selected_metadata.mipmap_count);

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
						R_ASSERT(g_imgui_texture_editor.pTHMSelected);
						STextureParams* pTHM = g_imgui_texture_editor.pTHMSelected;

						ImGui::Separator();

						ListBoxToken(ttype_token, "Type", reinterpret_cast<u32*>(&pTHM->type));
						ListBoxToken(tfmt_token, "Format", reinterpret_cast<u32*>(&pTHM->fmt));

						ImGui::SeparatorText("Flags");

						bool mipmaps_enabled = pTHM->flags.test(STextureParams::flGenerateMipMaps);
						if (ImGui::Checkbox("\tUse Mipmaps", &mipmaps_enabled))
						{
							pTHM->flags.set(STextureParams::flGenerateMipMaps, mipmaps_enabled);
						}

						bool dither_enabled = pTHM->flags.test(STextureParams::flDitherColor);
						if (ImGui::Checkbox("\tUse Dither", &dither_enabled))
						{
							pTHM->flags.set(STextureParams::flDitherColor, dither_enabled);
						}

						bool dither_each_mip_enabled = pTHM->flags.test(STextureParams::flDitherEachMIPLevel);
						if (ImGui::Checkbox("\tUse Dither each mip", &dither_each_mip_enabled))
						{
							pTHM->flags.set(STextureParams::flDitherEachMIPLevel, dither_each_mip_enabled);
						}

						bool implicit_lighted_enabled = pTHM->flags.test(STextureParams::flImplicitLighted);
						if (ImGui::Checkbox("\tImplicit lighted", &implicit_lighted_enabled))
						{
							pTHM->flags.set(STextureParams::flImplicitLighted, implicit_lighted_enabled);
						}

						bool fade_to_color_enabled = pTHM->flags.test(STextureParams::flFadeToColor);
						if (ImGui::Checkbox("\tFade to Color", &fade_to_color_enabled))
						{
							pTHM->flags.set(STextureParams::flFadeToColor, fade_to_color_enabled);
						}

						bool fade_to_alpha_enabled = pTHM->flags.test(STextureParams::flFadeToAlpha);
						if (ImGui::Checkbox("\tFade to Alpha", &fade_to_alpha_enabled))
						{
							pTHM->flags.set(STextureParams::flFadeToAlpha, fade_to_alpha_enabled);
						}

						ImGui::BeginDisabled(!mipmaps_enabled);
						ImGui::SeparatorText("MipMaps");
						ListBoxToken(tparam_token, "Filter", &pTHM->mip_filter);
						ImGui::EndDisabled();

						ImGui::SeparatorText("Bump");

						ImGui::BeginDisabled(pTHM->type != STextureParams::ETType::ttBumpMap);
						ListBoxToken(tbmode_token, "Mode", (u32*)(&pTHM->bump_mode));
							ImGui::BeginDisabled(pTHM->bump_mode != STextureParams::ETBumpMode::tbmNone);
							char button_bump_name[256];
							std::sprintf(button_bump_name, "%s##TE_BUMP", pTHM->bump_name.c_str());
							if (ImGui::Button(button_bump_name))
							{

							}
							ImGui::EndDisabled();
						ImGui::EndDisabled();

						ImGui::SeparatorText("Details");

						ImGui::BeginDisabled(false);
						
						char button_detail_name[256];
						std::sprintf(button_detail_name, "%s##TE_DETAIL", pTHM->detail_name.c_str());
						if (ImGui::Button(button_detail_name))
						{

						}

						ImGui::DragFloat("Scale##TE_DETAIL", &pTHM->detail_scale, 0.1f, 0.1f, 10000.0f);

						ImGui::EndDisabled();

						ImGui::SeparatorText("Material");

						ListBoxToken(tmtl_token, "Base", (u32*)(&pTHM->material));
						ImGui::DragFloat("Weight##TE_MATERIAL", &pTHM->material_weight, 0.01f, 0.0f, 1.0f);

						ImGui::SeparatorText("Fade");

						ImGui::SeparatorText("Border");
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

void TextureEditor_OnPressed(int key)
{
	// todo: replace with input events (don't hardcode with direct device-key mappings
	switch (key)
	{
	case SDL_Scancode::SDL_SCANCODE_ESCAPE:
	{
		if (Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_TextureEditor)])
		{
			SRequestData req;
			req.editor_type = (u32)eImGuiEditorType::kTextureEditor;
			req.request_type = (u32)eRequestType_TextureEditor::kDeselectCurrentSelected;

			AllEditors_SendRequest(req);
		}
		break;
	}
	}
}

void TextureEditor_OnReleased(int key)
{

}
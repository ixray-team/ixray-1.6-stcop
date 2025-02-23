#include "stdafx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

#include "ImUtils.h"

struct OMFEditorState
{
	bool is_file_loaded;


	string_path path;
} g_omf_editor;

OMFEditorState* pEditor = &g_omf_editor;

void OMFEditor_LoadFile(OMFEditorState* p_state)
{
	if (p_state)
	{
		if (xr_EFS)
		{
			wstring_path p_fa = { 0 };
			
		}


#ifdef IXR_WINDOWS
		OPENFILENAME ofn;       // common dialog box structure
		TCHAR szFile[1024] = { 0 };       // if using TCHAR macros

		// Initialize OPENFILENAME
		ZeroMemory(&ofn, sizeof(ofn));
		ofn.lStructSize = sizeof(ofn);
		ofn.hwndOwner = nullptr;
		ofn.lpstrFile = szFile;
		ofn.nMaxFile = sizeof(szFile);
		ofn.lpstrFilter = _T("OMF file\0*.omf\0");
		ofn.nFilterIndex = 1;
		ofn.lpstrFileTitle = NULL;
		ofn.nMaxFileTitle = 0;
		ofn.lpstrInitialDir = NULL;
		ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

		if (GetOpenFileName(&ofn) == TRUE)
		{
			int size_needed = WideCharToMultiByte(CP_UTF8, 0, ofn.lpstrFile, sizeof(szFile) / sizeof(szFile[0]), NULL, 0,
				NULL, NULL);

			_ASSERTE(size_needed > sizeof(p_state->path) / sizeof(p_state->path[0]) && "report to developer please");

			constexpr auto _kFilePathLength = sizeof(p_state->path) / sizeof(p_state->path[0]);
			if (size_needed <= _kFilePathLength)
			{
				WideCharToMultiByte(CP_UTF8, 0, ofn.lpstrFile, sizeof(p_state->path) / sizeof(p_state->path[0]), p_state->path, size_needed, NULL, NULL);

				g_omf_editor.is_file_loaded = true;
			}
		}
#elif defined(IXR_LINUX)
#error not implemented
#else
#error unknown platform
#endif
	}
}

void RenderToolsOMFEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
		return;


	if (ImGui::Begin("Editor - [OMF]##ToolsInGameImGui", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)]))
	{
		if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MainTable", 5))
		{
			for (unsigned char row = 0; row < 1; ++row)
			{
				ImGui::TableNextRow();
				
				ImGui::TableSetColumnIndex(0);
				if (ImGui::Button("Load##ToolsInGameImGui_OMFEditor"))
				{
					OMFEditor_LoadFile(&g_omf_editor);
				}


				if (g_omf_editor.is_file_loaded)
				{
					ImGui::TableSetColumnIndex(1);
					if (ImGui::Button("Close##ToolsInGameImGui_OMFEditor"))
					{
						g_omf_editor.is_file_loaded = false;
					}

					ImGui::TableSetColumnIndex(2);
					if (ImGui::Button("Save##ToolsInGameImGui_OMFEditor"))
					{

					}

					ImGui::TableSetColumnIndex(3);
					if (ImGui::Button("Save As...##ToolsInGameImGui_OMFEditor"))
					{

					}
				}
			}

			ImGui::EndTable();
		}


		if (g_omf_editor.is_file_loaded)
		{

			ImGui::TextWrapped("Loaded file: [%s]", g_omf_editor.path);
			ImGui::Separator();
		}

		ImGui::End();
	}
}
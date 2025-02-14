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
	xr_stack_string<sizeof(string_path) * 2> path;
} g_omf_editor;

OMFEditorState* pEditor = &g_omf_editor;

void OMFEditor_LoadFile(OMFEditorState* p_state)
{
	if (p_state)
	{
		if (xr_EFS)
		{
			xr_stack_wstring<sizeof(string_path)> local_path;
			bool status = xr_EFS->GetOpenName(local_path, L"OMF file\0*.omf\0");
			p_state->is_file_loaded = status;

			if (p_state->is_file_loaded)
			{
				status = Platform::WCHAR_TO_CHAR(local_path, p_state->path);
				R_ASSERT2(status, "report to developers! Unable to convert your path to multibyte string");

				p_state->is_file_loaded = status;
			}
		}
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
						pEditor->path[0] = 0;
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
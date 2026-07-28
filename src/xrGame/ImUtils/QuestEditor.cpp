#include "StdAfx.h"

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

#include "../GameTask.h"
#include "../GametaskManager.h"

struct CImGuiQuestEditor
{
};

CImGuiQuestEditor* g_pQuestEditor = nullptr;

void RequestHandler_QuestEditor(const SRequestData& req)
{
	if (!xr_FS)
		return;

	R_ASSERT2(
		static_cast<eImGuiEditorType>(req.editor_type) == eImGuiEditorType::kQuestEditor,
		"invalid data came you should debug code"
	);

	if (static_cast<eImGuiEditorType>(req.editor_type) != eImGuiEditorType::kQuestEditor)
		return;

	switch (static_cast<eRequestType_QuestEditor>(req.request_type))
	{
	case eRequestType_QuestEditor::kReadSettings:
	{
		break;
	}
	case eRequestType_QuestEditor::kWriteSettings:
	{
		break;
	}
	case eRequestType_QuestEditor::kLoadCurrentQuests:
	{
		break;
	}
	case eRequestType_QuestEditor::kDeselectCurrentSelectedOrHideWindow:
	{
		if (g_pQuestEditor)
		{
			Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_QuestEditor)] = false;
		}

		break;
	}
	case eRequestType_QuestEditor::kShutdown:
	{
		if (g_pQuestEditor)
		{
			delete g_pQuestEditor;
			g_pQuestEditor = nullptr;
		}

		break;
	}
	}
}

void QuestEditor_OnPressed(int key)
{
	switch (key)
	{
	case SDL_Scancode::SDL_SCANCODE_ESCAPE:
	{
		if (Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_QuestEditor)])
		{
			if (g_pQuestEditor)
			{
				SRequestData req;
				req.editor_type = (u32)eImGuiEditorType::kQuestEditor;
				req.request_type = (u32)eRequestType_QuestEditor::kDeselectCurrentSelectedOrHideWindow;

				AllEditors_SendRequest(req);
			}
		}
		break;
	}
	}
}

void QuestEditor_OnReleased(int key)
{
}

void InitImGuiQuestEditor()
{
}

void DestroyQuestEditorWindow()
{

}

void RenderQuestEditor_Draw_QuestList()
{
	if (g_pQuestEditor)
	{
		ImGui::TableNextColumn();

		ImGui::Text("Quest!");

	}
}

void RenderQuestEditor_Draw_NodeEd()
{
	if (g_pQuestEditor)
	{
		ImGui::TableNextColumn();

		ImGui::Text("Node");

	}
}

void RenderQuestEditor_Draw_Headings(const char* const* pNames, unsigned char total_count)
{
	if (pNames == nullptr)
		return;

	if (total_count == 0)
		return;

	for (unsigned char i = 0; i < total_count; ++i)
	{
		const char* pName = pNames[i];

		ImGui::TableSetupColumn(pName);
	}

	ImGui::TableHeadersRow();
}

void RenderQuestEditor()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_QuestEditor)])
		return;

	if (g_pGameLevel == nullptr)
		return;

	if (ai().get_alife() == nullptr)
		return;

	if (g_pClsidManager == nullptr)
		return;

	if (g_pQuestEditor == nullptr)
	{
		g_pQuestEditor = new CImGuiQuestEditor();
	}


	if (g_pQuestEditor)
	{
		constexpr const char* _kQuestEditor_TableHeadings[] = {
			"Locations",
			"Node editor"
		};

		constexpr unsigned char _kQuestEditor_TableHeadingsCount = static_cast<unsigned char>(sizeof(_kQuestEditor_TableHeadings)) / static_cast<unsigned char>(sizeof(_kQuestEditor_TableHeadings[0]));


		if (ImGui::Begin("Quest Editor", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_QuestEditor)]))
		{
			if (ImGui::BeginTable("##QE_Table", _kQuestEditor_TableHeadingsCount))
			{
				RenderQuestEditor_Draw_Headings(
					_kQuestEditor_TableHeadings,
					_kQuestEditor_TableHeadingsCount
				);

				ImGui::TableNextRow();

				RenderQuestEditor_Draw_QuestList();

				RenderQuestEditor_Draw_NodeEd();

				ImGui::EndTable();
			}
		}

		ImGui::End();
	}
}
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

}
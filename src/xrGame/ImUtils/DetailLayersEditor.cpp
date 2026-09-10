#include "StdAfx.h"
#include "ImUtils.h"
#include "../../xrEngine/Render.h"

void RenderDetailLayersEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)])
		return;

	Render->renderImGuiDebugWindow_DetailLayersEditor();
}
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

void RenderToolsRenderDebugSVGStorageViewerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_RenderDebug_SVGStorageViewer)])
		return;

	Render->renderImGuiDebugWindow_SVGStorage();
}
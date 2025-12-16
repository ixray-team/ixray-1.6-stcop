
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

void RenderPPEEditor()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_PostProcessEffectorEditor)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	if (!g_actor)
		return;

	if (ImGui::Begin("Post-Process Effector"))
	{

	}

	ImGui::End();
}
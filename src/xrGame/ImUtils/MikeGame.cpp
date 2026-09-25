#include "StdAfx.h"
#include "ImUtils.h"
#include "MikeGame/Game.h"

void RenderMikeGame()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::MikeGame)])
	{
		return;
	}

	static v_obj::Game game;

	float title_bar_height = ImGui::GetFrameHeight();

	ImGui::SetNextWindowSize({700, 700 + title_bar_height});
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	ImGui::Begin("The Incredible Adventures of the Green Pixel Thing", 0, ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings);
	game.Draw();
	ImGui::End();
	ImGui::PopStyleVar();
}
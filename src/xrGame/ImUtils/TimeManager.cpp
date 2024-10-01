#include "stdafx.h"
#include "../Level.h"
#include "../GamePersistent.h"
#include "../game_sv_single.h"
#include "../alife_simulator.h"
#include "../alife_time_manager.h"
#include "../date_time.h"

static struct 
{
	u32 nTime = {};
	char hour[3] = {};
}
imgui_time_manager;

void RenderTimeManagerWindow()
{
	if (!g_pGameLevel)
		return;

	if (Level().Server == nullptr)
		return;

	if (Level().Server->game == nullptr)
		return;

	game_sv_Single* pGame = dynamic_cast<game_sv_Single*>(Level().Server->game);

	if (!pGame)
		return;

	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_TimeManager)])
		return;

	if (!ImGui::Begin("Time Manager##InGame", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_TimeManager)]))
	{
		ImGui::End();
		return;
	}

	u64 time = pGame->alife().time_manager().game_time();
	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;
	split_time(time, year, month, day, hours, mins, secs, milisecs);

	ImGui::Text("Current date: %zu/%zu/%zu (day/month/year)", day, month, year);
	ImGui::Text("Current time: %zu [h] : %zu [min] : %zu [sec]", hours, mins, secs);

	ImGui::Text("Add hour:");
	ImGui::SameLine();
	ImGui::InputText("##InputHour_GameTimeManager", imgui_time_manager.hour, sizeof(imgui_time_manager.hour));
	ImGui::SameLine();

	if (ImGui::Button("add##InputHour_GameTimeManager"))
	{
		size_t len = strlen(imgui_time_manager.hour);

		bool valid{ true };
		for (int i = 0; i < len; ++i)
		{
			if (imgui_time_manager.hour[i] == '\0')
				break;

			if (!((imgui_time_manager.hour[i] >= '0' && imgui_time_manager.hour[i] <= '9')))
			{
				valid = false;
				break;
			}
		}

		if (valid)
		{
			u32 casted = atoi(imgui_time_manager.hour);
			clamp(casted, u32(0), u32(24));
			u32 value = casted * 3600 * 1000;
			float fv = casted * 3600;
			g_pGamePersistent->Environment().ChangeGameTime(fv);
			pGame->alife().time_manager().change_game_time(value);
		}
	}



	ImGui::End();
}

#include "UIEditorMain.h"
#include "../xrScripts/stdafx.h"
#include "../xrEngine/IGame_Actor.h"
#include "../xrScripts/lua_ext.h"
#include "../xrEngine/IGame_Level.h"
#include "../xrEngine/string_table.h"

struct StatisticHashMapEntry
{
	int Counter;
	float Time;
	const char* Name;
};

void RenderActorInfos()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::ActorInfos)])
		return;

	if (g_pIGameActor == nullptr) {
		return;
	}

	if (g_pGameLevel == nullptr) {
		return;
	}

	if (!g_pGameLevel->bReady) {
		return;
	}

	if (!ImGui::Begin("Actor InfoPortions", &Engine.External.EditorStates[static_cast<u8>(EditorUI::ActorInfos)])) {
		ImGui::End();
		return;
	}

	const auto& Data = g_pIGameActor->GetKnowedPortions();

	static char buffer[128]{};
	ImGui::Text("Filter:");
	ImGui::SameLine();
	ImGui::InputText("##FilterAP", buffer, sizeof(buffer));

	static char add_info[128]{};
	ImGui::Text("Add info:");
	ImGui::SameLine();
	ImGui::InputText("##AddInfoAP", add_info, sizeof(add_info));
	ImGui::SameLine();

	if (ImGui::Button("add"))
	{
		if (g_pIGameActor)
		{
			g_pIGameActor->GiveInfoPortion(add_info);
		}
	}

	for (const auto& Str : Data)
	{
		bool is_need_to_show = true;

		if (buffer[0]!='\0' || strlen(buffer))
		{
			if (Str.find(buffer) == xr_string::npos)
			{
				is_need_to_show = false;
			}
		}

		if (is_need_to_show)
		{
			if (ImGui::Button(Str.c_str(), { 200, 30 })) {
				g_pIGameActor->DisableInfoPortion(Str.c_str());
			}
		}
	}

	ImGui::End();
}

xr_vector<xr_string> parse_params(const char* params, char divider) {
	xr_string data = params;
	return std::move(data.Split(divider));
}

static bool checkTeleport = true;
static bool checkSpawnItems = true;

void OnOpenSceneClicked(xr_map<xr_string, xr_vector<xr_string>> scenesTable, const char* key, const char* value) {
	if (!scenesTable[key][0].empty()) {
		if (scenesTable[key][0] != "nil") {
			g_pIGameActor->GiveInfoPortion(scenesTable[key][0].c_str());
		}
	}

	Fvector point = {};
	if (checkTeleport) {
		IGame_Patrol* patrol = nullptr;

		if (scenesTable[key].size() > 1 && !scenesTable[key][1].empty()) {
			patrol = g_pGameLevel->CreatePatrol(scenesTable[key][1].c_str());
			point = patrol->point(0u);
			g_pIGameActor->SetActorPosition(point);
		}

		if (patrol == nullptr) {
			return;
		}

		if (scenesTable[key].size() > 2 && !scenesTable[key][2].empty()) {
			auto look = g_pGameLevel->CreatePatrol(scenesTable[key][2].c_str());
			point = look->point(0u);
			float dir = point.sub(patrol->point(0u)).getH();
			g_pIGameActor->SetActorDirection(-dir);

			xr_delete(look);
		}
		xr_delete(patrol);
	}

	if (checkSpawnItems) {
		if (scenesTable[key].size() > 3 && !scenesTable[key][3].empty())
		{
			auto spawnItems = parse_params(scenesTable[key][3].c_str(), ',');
			for (auto& item : spawnItems) {
				if (item == "nil") {
					continue;
				}
				g_pGameLevel->SpawnItem(item.c_str(), point, 0, 0);
			}
		}
	}
}

void RenderScenesViewer() {
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::ScenesViewer)] || g_pGameLevel == nullptr) {
		return;
	}

	if (g_pIGameActor == nullptr) {
		return;
	}

	if (!g_pGameLevel->bReady) {
		return;
	}

	if (!ImGui::Begin("Scenes Viewer", &Engine.External.EditorStates[static_cast<u8>(EditorUI::ScenesViewer)])) {
		ImGui::End();
		return;
	}
	string_path file_name = {};
	static auto scenesIni = CInifile(
			FS.update_path(
				file_name,
				"$game_config$",
				"scripts\\scenes.ltx"
			),
			TRUE,
			TRUE,
			FALSE
		);

	auto levelName = g_pGameLevel->name();
	if (levelName.size() == 0 || !scenesIni.section_exist(levelName)) {
		ImGui::End();
		return;
	}

	auto lineCount = scenesIni.line_count(levelName);
	const char* key = nullptr;
	const char* value = nullptr;
	xr_map<xr_string, xr_vector<xr_string>> scenesTable;

	ImGui::Checkbox("Teleport to scene", &checkTeleport);
	ImGui::Checkbox("Spawn items", &checkSpawnItems);

	for (u32 i = 0; i < lineCount; i++) {
		scenesIni.r_line(levelName, i, &key, &value);
		scenesTable[key] = parse_params(value, '|');

		if (ImGui::Button(g_pStringTable->translate(key).c_str(), {200, 30})) {
			OnOpenSceneClicked(scenesTable, key, value);
		}
	}

	ImGui::End();
}

void RenderUI()
{
	CImGuiManager::Instance().Subscribe("Editor Weather Draw", CImGuiManager::ERenderPriority::eMedium, RenderUIWeather);
	CImGuiManager::Instance().Subscribe("Actor InfoPortions", CImGuiManager::ERenderPriority::eMedium, RenderActorInfos);
	CImGuiManager::Instance().Subscribe("Scenes Viewer", CImGuiManager::ERenderPriority::eMedium, RenderScenesViewer);
}

bool ImGui_ListBox(const char* label, int* current_item, bool(*items_getter)(void*, int, const char**), void* data,
	int items_count, const ImVec2& size_arg = ImVec2(0, 0))
{
	if (!ImGui::BeginListBox(label, size_arg))
	{
		ImGui::End();
		return false;
	}

	bool value_changed = false;
	// Assume all items have even height (= 1 line of text). If you need items of different or variable sizes you can
	// create a custom version of ListBox() in your code without using the clipper.
	{
		ImGuiListClipper clipper;
		clipper.Begin(items_count, ImGui::GetTextLineHeightWithSpacing()); // We know exactly our line height
		// here so we pass it as a minor
		// optimization, but generally you
		// don't need to.
		while (clipper.Step())
		{
			for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; i++)
			{
				const bool item_selected = (i == *current_item);
				const char* item_text;
				if (!items_getter(data, i, &item_text))
					item_text = "*Unknown item*";

				ImGui::PushID(i);
				if (ImGui::Selectable(item_text, item_selected))
				{
					*current_item = i;
					value_changed = true;
				}
				ImGui::PopID();
			}
		}
	}
	ImGui::EndListBox();

	return value_changed;
}
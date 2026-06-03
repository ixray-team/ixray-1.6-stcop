#include "UIEditorMain.h"
#include "../xrScripts/stdafx.h"
#include "../xrEngine/IGame_Actor.h"
#include "../xrScripts/lua_ext.h"
#include "../xrEngine/IGame_Level.h"
#include "../xrEngine/string_table.h"
#include "../xrSound/New/SoundMixer.h"
#include "../xrSound/New/SoundMixerInternal.h"

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

	if (!GActorInterface || !(g_pGameLevel && g_pGameLevel->bReady))
	{
		return;
	}

	if (!ImGui::Begin("Actor InfoPortions", &Engine.External.EditorStates[static_cast<u8>(EditorUI::ActorInfos)])) {
		ImGui::End();
		return;
	}

	const auto& Data = GActorInterface->GetKnownPortions();

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
		if (GActorInterface)
		{
			GActorInterface->GiveInfoPortion(add_info);
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
			if (ImGui::Button(Str.c_str())) 
			{
				GActorInterface->DisableInfoPortion(Str.c_str());
			}
			if (ImGui::IsItemHovered())
			{
				xr_string hint = "";

				auto dialogsVec = GActorInterface->GetKnownPortionDialogs(Str.c_str());
				if (dialogsVec.size())
					hint += "Dialogs:";

				for (const auto& dialogStr : dialogsVec)
				{
					hint += "\n";
					hint += dialogStr;
				}

				auto disablePortionsVec = GActorInterface->GetKnownPortionDisable(Str.c_str());
				if (disablePortionsVec.size())
					hint += "Disables portions:";

				for (const auto& disablePortion : disablePortionsVec)
				{
					hint += "\n";
					hint += disablePortion;
				}

				auto articlesVec = GActorInterface->GetKnownPortionArticles(Str.c_str());
				if (articlesVec.size())
					hint += "Articles:";

				for (const auto& articleStr : articlesVec)
				{
					hint += "\n";
					hint += articleStr;
				}
				
				auto articlesDisableVec = GActorInterface->GetKnownPortionArticlesDisable(Str.c_str());
				if (articlesDisableVec.size())
					hint += "Disables articles:";

				for (const auto& articleDisableStr : articlesDisableVec)
				{
					hint += "\n";
					hint += articleDisableStr;
				}

				auto tasksVec = GActorInterface->GetKnownPortionTasks(Str.c_str());
				if (tasksVec.size())
					hint += "Tasks:";

				for (const auto& taskStr : tasksVec)
				{
					hint += "\n";
					hint += taskStr;
				}

				if (hint.empty())
					hint = "No properties for this info portion";

				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip(hint.c_str());
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
			GActorInterface->GiveInfoPortion(scenesTable[key][0].c_str());
		}
	}

	Fvector point = {};
	if (checkTeleport) {
		IGame_Patrol* patrol = nullptr;

		if (scenesTable[key].size() > 1 && !scenesTable[key][1].empty()) {
			patrol = g_pGameLevel->CreatePatrol(scenesTable[key][1].c_str());
			point = patrol->point(0u);
			GActorInterface->SetActorPosition(point);
		}

		if (patrol == nullptr) {
			return;
		}

		if (scenesTable[key].size() > 2 && !scenesTable[key][2].empty()) {
			auto look = g_pGameLevel->CreatePatrol(scenesTable[key][2].c_str());
			point = look->point(0u);
			float dir = point.sub(patrol->point(0u)).getH();
			GActorInterface->SetActorDirection(-dir);

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

	if (GActorInterface == nullptr) {
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
				_game_config_,
				"scripts\\scenes.ltx"
			),
			true,
			true,
			false
		);

	auto levelName = g_pGameLevel->name();
	if (levelName.size() == 0 || !scenesIni.section_exist(levelName)) {
		ImGui::End();
		return;
	}

	auto lineCount = scenesIni.line_count(levelName);
	str_c key = nullptr;
	str_c value = nullptr;
	xr_map<xr_string, xr_vector<xr_string>> scenesTable;

	ImGui::Checkbox("Teleport to scene", &checkTeleport);
	ImGui::Checkbox("Spawn items", &checkSpawnItems);

	for (u32 i = 0; i < lineCount; i++) {
		scenesIni.r_line(levelName, i, key, value);
		scenesTable[key] = parse_params(value, '|');

		if (ImGui::Button(g_pStringTable->translate(key).c_str())) {
			OnOpenSceneClicked(scenesTable, key, value);
		}
	}

	ImGui::End();
}

void RenderUI()
{
	PROF_EVENT("Render ImGui");
	CImGuiManager::Instance().Subscribe("Editor Weather Draw", CImGuiManager::ERenderPriority::eMedium, RenderUIWeather);
	CImGuiManager::Instance().Subscribe("Actor InfoPortions", CImGuiManager::ERenderPriority::eMedium, RenderActorInfos);
	CImGuiManager::Instance().Subscribe("Scenes Viewer", CImGuiManager::ERenderPriority::eMedium, RenderScenesViewer);
	CImGuiManager::Instance().Subscribe("Editor Weather Draw", CImGuiManager::ERenderPriority::eMedium, []() {
        if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Audio_General)]) {
            return;
        }

		if (!ImGui::Begin("Audio Viewer", &Engine.External.EditorStates[static_cast<u8>(EditorUI::ScenesViewer)])) {
			ImGui::End();
			return;
		}

		if (ImGui::BeginTabBar("TabBarAudio"))
		{
			xrSRWLockGuard g1(XRay::Sound::Mixer::GetUpdateMutex());
			auto stats = XRay::Sound::Mixer::GetStats();
			u64 free_time_micros = (stats->frame_time_micros - stats->precache_time_micros - stats->render_time_micros);

			if (ImGui::BeginTabItem("Overall")) 
			{
				auto& slots = XRay::Sound::Mixer::GetSlots();

				u32 free_slots = 0;
				u32 playing_slots = 0;
				u32 stopped_slots = 0;
				u32 paused_slots = 0;
				for (size_t i = 0; i < slots.size(); i++)
				{
					const auto& slot = slots[i];
					if (slot.sound_name.size() == 0)
					{
						free_slots++;
						continue;
					}

					switch (slot.state)
					{
						case XRay::Sound::Mixer::State::Stopped: stopped_slots++; break;
						case XRay::Sound::Mixer::State::Playing: playing_slots++; break;
						case XRay::Sound::Mixer::State::Paused: paused_slots++; break;
					}
				}

				static float min_volume = -80.0f;
				static float max_volume = 0.0f;

#ifdef DEBUG_DRAW
				for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
				{
					ImGui::Text("Volume %i: %.2fdB", i, stats->channel_volumes[i]);
				}

				ImGui::SliderFloat("Min Volume", &min_volume, -180.0f, -18.0f, "%.2fdB");
				ImGui::SliderFloat("Max Volume", &max_volume, -18.0f, 18.0f, "%.2fdB");

				ImGui::PushStyleColor(ImGuiCol_FrameBg, 0xBCBF8A6E);
				ImGui::PushStyleColor(ImGuiCol_PlotHistogram, 0xFF0F0F0F);
				ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));

				float AvailWidth = ImGui::GetContentRegionAvail().x;
				ImGui::PlotHistogram("##Spectral", stats->spectral_data, SND_BLOCKSIZE / 2, 0, nullptr, min_volume, max_volume, ImVec2(AvailWidth, 200));
				ImGui::PopStyleColor(2);
				ImGui::PopStyleVar();
				ImGui::SameLine();

				ImGui::Separator();
#endif

				ImGui::Text("Slots: %d", slots.size() - free_slots);
				ImGui::Text("    free:          %d", free_slots);
				ImGui::Text("    possibly free: %d", stats->possible_free_count);
				ImGui::Text("    playing:       %d", playing_slots);
				ImGui::Text("    stopped:       %d", stopped_slots);
				ImGui::Text("    paused:        %d", paused_slots);
				ImGui::Text("Cache: miss %.1f%%", ((float)stats->cache_miss_count / (((float)stats->cache_hit_count) + EPS)) * 100.0f);
				ImGui::Text("    hits:   %d", stats->cache_hit_count);
				ImGui::Text("    misses: %d", stats->cache_miss_count);
				ImGui::Text("    free:   %d", stats->cache_lines_free);
				ImGui::Text("    total:  %d", stats->cache_lines_total);
				ImGui::Text("    render: %d", stats->render_cache_miss);
				ImGui::Text("Timers: free %.1f%%", ((float)free_time_micros / (float)stats->frame_time_micros) * 100.0f);
				ImGui::Text("    update:   %.2fms", (float)stats->update_time_micros / 1000.0f);
				ImGui::Text("    frame:    %.2fms", (float)stats->frame_time_micros / 1000.0f);
				ImGui::Text("    precache: %.2fms", (float)stats->precache_time_micros / 1000.0f);
				ImGui::Text("    render:   %.2fms", (float)stats->render_time_micros / 1000.0f);
				static u32 filter = 0;
				if (ImGui::BeginListBox("Filter", { 0, 6 * 22 })) {
					if (ImGui::Selectable("None", filter == 0)) {
						filter = 0;
					} if (ImGui::Selectable("Stopped", filter == 1)) {
						filter = 1;
					} if (ImGui::Selectable("Paused", filter == 2)) {
						filter = 2;
					} if (ImGui::Selectable("Simulated", filter == 3)) {
						filter = 3;
					} if (ImGui::Selectable("Playing", filter == 4)) {
						filter = 4;
					} if (ImGui::Selectable("Delay", filter == 5)) {
						filter = 5;
					}
					ImGui::EndListBox();
				}

				ImGui::Separator();

				for (size_t i = 0; i < slots.size(); i++) {
					const auto& slot = slots[i];
					if (/*slot.sound == nullptr || */slot.sound_name.size() == 0) {
						continue;
					}

					if (filter == 1 && (slot.state != XRay::Sound::Mixer::State::Stopped)) {
						continue;
					} else if (filter == 2 && (slot.state != XRay::Sound::Mixer::State::Paused)) {
						continue;
					} else if (filter == 3 && (slot.state != XRay::Sound::Mixer::State::Playing)) {
						continue;
					} else if (filter == 4 && (slot.state != XRay::Sound::Mixer::State::Playing || !XRay::Sound::Mixer::SlotIsRelated(i + 1))) {
						continue;
					} else if (filter == 5 && (slot.state != XRay::Sound::Mixer::State::Delay))
					{
						continue;
					}

					const char* name = slot.sound_name.c_str();
					const char* state_name = magic_enum::enum_name(slot.state).data();
					Fvector* parameters = XRay::Sound::Mixer::GetParameters(i + 1);
					Fvector& pos = parameters[(u32)XRay::Sound::Mixer::ParameterId::Position];
					Fvector& volumes = parameters[(u32)XRay::Sound::Mixer::ParameterId::VolumePerChannel];
					float playtime = XRay::Sound::Mixer::GetPlaytime(i + 1);
					float duration = XRay::Sound::Mixer::GetDuration(i + 1);

					float color_shift = 1.0f - (playtime / duration);
					ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, color_shift, color_shift, 1.0f));

					if (slot.flags & (u32)XRay::Sound::Mixer::Flags::Spatial) {
						ImGui::Text("[%3i:%s:%.3fs/%.3fs]: (%.1f%% %.1f%%, %.1f%%), (%.3f,%.3f,%.3f), %s, %d flags", 
							i + 1, name, playtime, duration, volumes.x * 100.0f, volumes.y * 100.0f, volumes.z * 100.0f, pos.x, pos.y, pos.z, state_name, slot.flags);
					} else {
						ImGui::Text("[%3i:%s:%.3fs/%.3fs]: %.1f%%, 2D, %s, %d flags", i + 1, name, playtime, duration, volumes.x * 100.0f, state_name, slot.flags);
					}

					ImGui::PopStyleColor(1);
				}
				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Sources")) {
				ImGui::Text("Sounds: %d", XRay::Sound::Mixer::GetSourceCount());
				for (size_t i = 0; i < XRay::Sound::Mixer::GetSourceCount(); i++) {
					auto source = XRay::Sound::Mixer::GetSource(i);
					if (source == nullptr) {
						continue;
					}

					float source_time = (float)source->frames_total / (float)SND_SAMPLERATE;
					ImGui::Text("    [%s:%.2f]: refs - %d", source->name.c_str(), source_time, source->ref_count.load());
				}
				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Zones"))
			{
				const auto& zones = XRay::Sound::Mixer::GetZones();
				size_t Iter = 0;
				for (auto& zone : zones)
				{
					ImGui::Text("Zone \"%s\"", zone.name.c_str());
					ImGui::Text("    pos:  (%.2f, %.2f, %.2f)",  zone.center.x, zone.center.y, zone.center.z);
					ImGui::Text("    size: (%.2f, %.2f, %.2f)", zone.size.x, zone.size.y, zone.size.z);
					ImGui::Text("    min:  (%.2f, %.2f, %.2f)", zone.min.x, zone.min.y, zone.min.z);
					ImGui::Text("    max:  (%.2f, %.2f, %.2f)", zone.max.x, zone.max.y, zone.max.z);
					ImGui::Text("    settings: ");
					ImGui::Text("        reverb:        %.2fs", zone.settings.reverb);
					ImGui::Text("        decay:         %.2fs", zone.settings.decay_time);
					ImGui::Text("        reverb  delay: %.2fs", zone.settings.reverb_delay);
					ImGui::Text("        reflect delay: %.2fs", zone.settings.reflections_delay);
					ImGui::Text("        room:          %.2fs", zone.settings.room);
					ImGui::Separator();

					Iter++;
				}
				ImGui::EndTabItem();
			}

			ImGui::EndTabBar();
		}

		ImGui::End();
    });
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
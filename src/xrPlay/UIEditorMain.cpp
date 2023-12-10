#include "UIEditorMain.h"
#include "../xrEngine/stdafx.h"
#include <imgui.h>

struct StatisticHashMapEntry
{
	int Counter;
	float Time;
	const char* Name;
};

void RenderUI()
{
	static bool FirstDraw = true;

	Device.AddUICommand("Editor Weather Draw", 2, RenderUIWeather);

	Device.AddUICommand("Profiler", 2, []() {
		if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Profiler)])
			return;

		if (!ImGui::Begin("Profiler", &Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Profiler)])) {
			ImGui::End();
			return;
		}

		static u32 SelectedThread = 0;
		const auto& Statistics = Profile::GetThreadStatistics(SelectedThread);
		if (ImGui::BeginCombo("Select thread:", Statistics.Name.c_str())) {
			for (u32 i = 0; i < Profile::GetThreadCount(); i++) {
				if (ImGui::Selectable(Profile::GetThreadStatistics(i).Name.c_str(), i == SelectedThread)) {
					SelectedThread = i;
				}
			}
			ImGui::EndCombo();
		}

		const ImVec2 LineSize = ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().y / (Statistics.StackLevels));
		const auto& Events = Statistics.Events;

		auto WindowPosition = ImGui::GetWindowPos();
		auto& DrawList = *ImGui::GetWindowDrawList();
		WindowPosition.y += 70;
#if 0
		for (const auto& Event : Events) {
			double xBeginT = ilerp((double)(Statistics.TimestampFrameBegin / 1000), (double)(Statistics.TimestampFrameEnd / 1000), (double)(Event.BeginTimestamp / 1000));
			double xEndT = ilerp((double)(Statistics.TimestampFrameBegin / 1000), (double)(Statistics.TimestampFrameEnd / 1000), (double)(Event.EndTimestamp / 1000));
			ImVec2 Min = ImVec2(WindowPosition.x + LineSize.x * xBeginT, WindowPosition.y + LineSize.y * Event.StackLevel);
			ImVec2 Max = ImVec2(WindowPosition.x + LineSize.x * xEndT, WindowPosition.y + LineSize.y * (Event.StackLevel + 1));
			DrawList.AddRectFilled(Min, Max, Event.Color);
		}
#else
		for (const auto& Event : Events) {
			float BeginT = Statistics.BeginSmoothTimers.at(Event.GetHash()).GetRMS();
			float EndT = Statistics.EndSmoothTimers.at(Event.GetHash()).GetRMS();
			ImVec2 Min = ImVec2(WindowPosition.x + LineSize.x * BeginT, WindowPosition.y + LineSize.y * Event.StackLevel);
			ImVec2 Max = ImVec2(WindowPosition.x + LineSize.x * EndT, WindowPosition.y + LineSize.y * (Event.StackLevel + 1));
			DrawList.AddRectFilled(Min, Max, Event.Color);
			DrawList.AddText(ImVec2(Min.x + ((Max.x - Min.x) * 0.5f), Min.y + (LineSize.y * 0.5f)), 0xFFFFFFFF, Event.Name);
		}
#endif

		ImGui::End();
	});

	Device.AddUICommand("Statistics", 2, []() {
		if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Statistics)])
			return;

		constexpr float PAD = 10.0f;
		constexpr ImGuiWindowFlags window_flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav;
		const ImGuiViewport* viewport = ImGui::GetMainViewport();
		ImVec2 work_pos = viewport->WorkPos;
		ImVec2 work_size = viewport->WorkSize;
		ImVec2 window_pos, window_pos_pivot;
		window_pos.x = (work_pos.x + PAD);
		window_pos.y = (work_pos.y + PAD);
		window_pos_pivot.x = 0.0f;
		window_pos_pivot.y = 0.0f;

		ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
		ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0, 0, 0, 1));
		ImGui::SetNextWindowPos(window_pos, ImGuiCond_Always, window_pos_pivot);
		ImGui::SetNextWindowViewport(viewport->ID);
		ImGui::SetNextWindowBgAlpha(0.75f);
		
		if (!ImGui::Begin("Statistics", &Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Statistics)], window_flags)) {
			ImGui::End();
			ImGui::PopStyleVar();
			ImGui::PopStyleColor();
			return;
		}

		auto TextPadding = ImGui::CalcTextSize("TEST");

		float CursorX = ImGui::GetCursorPosX();
		float CursorY = ImGui::GetCursorPosY();
		for (u32 i = 0; i < Profile::GetThreadCount(); i++) {
			const auto& Statistics = Profile::GetThreadStatistics(i);
			float LocalY = CursorY;
			for (const auto& Event : Statistics.Events) {
				if (Event.GroupHash == HashValue<u64>("Default")) {
					continue;
				}

				ImGui::SetCursorPosX(CursorX);
				ImGui::SetCursorPosY(LocalY);
				LocalY += TextPadding.y;
				double Time = double(Event.EndTimestamp - Event.BeginTimestamp) / 1000000.0;
				ImGui::Text("%s: %fms", Event.Name, Time);
			}
			CursorX += 300;
		}

		/*
		auto DrawCategory = [&TextPadding](const char* Name, float XOffset, float YOffset) {
			xr_hash_map<u64, int> EventMap;
			xr_vector<StatisticHashMapEntry> Events;
			int Counter = 0;
			int PreviousStack = 0;

			Profile::TraverseGroup(Name, [&Events, &EventMap, XOffset, &YOffset, &TextPadding, &Counter, &PreviousStack](const Profile::TraceEvent& Event) {
				if (Event.StackLevel > PreviousStack) {
					PreviousStack = Event.StackLevel;
					Counter++;
				} else if (Event.StackLevel < PreviousStack) {
					PreviousStack = Event.StackLevel;
					Counter--;
				}

				float Time = float(Event.EndTimestamp - Event.BeginTimestamp) / 1000000.0f;
				const auto Hash = HashValue<u64>(Event.Name);
				int Index = std::max(0, (int)Events.size() - 1);
				if (!EventMap.contains(Hash)) {
					Events.emplace_back(StatisticHashMapEntry{});
					EventMap[Hash] = Index;
				} else {
					Index = EventMap[Hash];
				}

				Events[Index].Name = Event.Name;
				Events[Index].Counter = Counter;
				Events[Index].Time += Time;
			});

			std::sort(Events.begin(), Events.end(), [](const StatisticHashMapEntry& Left, const StatisticHashMapEntry& Right) {
				return Left.Time > Right.Time;
			});

			for (const auto& Entry : Events) {
				ImGui::SetCursorPosX(XOffset);
				ImGui::SetCursorPosY(YOffset);
				YOffset += TextPadding.y;
				ImGui::Text("%s: %fms", Entry.Name, Entry.Time);
			}
		};

		DrawCategory("Engine", CursorX, CursorY);
		DrawCategory("Render", CursorX, CursorY);
		CursorX += 300;
		DrawCategory("Game", CursorX, CursorY);
		*/

		ImGui::End();
		ImGui::PopStyleVar();
		ImGui::PopStyleColor();
	});
};

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
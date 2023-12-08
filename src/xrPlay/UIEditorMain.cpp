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

	Device.AddUICommand("Editor Weather Draw", 3, RenderUIWeather);
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

		auto DrawCategory = [&TextPadding](const char* Name, u32 XOffset, u32 YOffset) {
			xr_hash_map<u64, StatisticHashMapEntry> Events;
			int Counter = 0;
			int PreviousStack = 0;
			Profile::TraverseGroup(Name, [&Events, XOffset, &YOffset, &TextPadding, &Counter, &PreviousStack](const Profile::TraceEvent& Event) {
				if (Event.StackLevel > PreviousStack) {
					PreviousStack = Event.StackLevel;
					Counter++;
				} else if (Event.StackLevel < PreviousStack) {
					PreviousStack = Event.StackLevel;
					Counter--;
				}

				float Time = float(Event.EndTimestamp - Event.BeginTimestamp) / 1000000.0f;
				Events[HashValue<u64>(Event.Name)].Name = Event.Name;
				Events[HashValue<u64>(Event.Name)].Counter = Counter;
				Events[HashValue<u64>(Event.Name)].Time += Time;
			});

			for (const auto& [Key, Entry] : Events) {
				ImGui::SetCursorPosX(XOffset);
				ImGui::SetCursorPosY(YOffset);
				YOffset += TextPadding.y;
				ImGui::Text("%s: %fms", Entry.Name, Entry.Time);
			}
		};

		u32 CursorX = ImGui::GetCursorPosX();
		u32 CursorY = ImGui::GetCursorPosY();
		DrawCategory("Engine", CursorX, CursorY);
		CursorX += 300;
		DrawCategory("Render", CursorX, CursorY);
		CursorX += 300;
		DrawCategory("Game", CursorX, CursorY);

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
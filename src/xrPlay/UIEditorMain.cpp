#include "UIEditorMain.h"
#include "../xrEngine/stdafx.h"
#include <imgui.h>

void RenderUI()
{
	static bool FirstDraw = true;

	Device.AddUICommand("Editor Main Draw", 2, []() {
		auto& States = Engine.External.EditorStates;

		if (ImGui::BeginMainMenuBar()) {
			if (ImGui::BeginMenu("File")) {
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Edit")) {
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("View")) {
				ImGui::EndMenu();
			}
			
			if (ImGui::BeginMenu("Tools")) {
				ImGui::MenuItem("Debug Render", nullptr, &States[static_cast<u8>(EditorUI::DebugDraw)]);
				ImGui::MenuItem("Shader Debug", nullptr, &States[static_cast<u8>(EditorUI::Shaders)]);
				ImGui::MenuItem("Weather Editor", nullptr, &States[static_cast<u8>(EditorUI::Weather)]);
				ImGui::MenuItem("Command line variables", nullptr, &States[static_cast<u8>(EditorUI::CmdVars)]);
				ImGui::EndMenu();
			}

			ImGui::EndMainMenuBar();
		}
	});

	Device.AddUICommand("Editor Weather Draw", 2, RenderUIWeather);
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
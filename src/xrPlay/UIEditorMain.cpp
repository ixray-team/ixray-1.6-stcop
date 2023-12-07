#include "UIEditorMain.h"
#include "../xrEngine/stdafx.h"
#include <imgui.h>

void RenderUI()
{
	static bool FirstDraw = true;

	Device.AddUICommand("Editor Main Draw", 100,
		[&]()
		{
			ImGui::SetNextWindowBgAlpha(0.9f);
			ImGui::SetNextWindowSizeConstraints({250, 120}, {500, 500});
			
			if (FirstDraw)
			{
				ImGui::SetNextWindowPos({ (float)Device.TargetWidth - 275, 25 });
				FirstDraw = false;
			}

			ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 4.0f);
			ImGui::Begin("Editor Main	(Alt+I)", nullptr, ImGuiWindowFlags_NoScrollbar);

			ImGui::Checkbox("Debug Render", &Engine.External.EditorStates[EditorUI::DebugDraw]);
			ImGui::Checkbox("Shader Debug", &Engine.External.EditorStates[EditorUI::Shaders]);
			ImGui::Checkbox("Weather Editor", &Engine.External.EditorStates[EditorUI::Weather]);

			ImGui::End();
			ImGui::PopStyleVar();
		});

	Device.AddUICommand("Editor Weather Draw", 100, RenderUIWeather);
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
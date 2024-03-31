#include "UIEditorMain.h"
#include "../xrScripts/stdafx.h"
#include "../xrScripts/lua_ext.h"

#include <imgui.h>
#include "../xrEngine/IGame_Actor.h"

struct StatisticHashMapEntry
{
	int Counter;
	float Time;
	const char* Name;
};

void RenderActorInfos()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::ActorInfos)] || g_pIGameActor == nullptr)
		return;

	if (!ImGui::Begin("Actor InfoPortions", &Engine.External.EditorStates[static_cast<u8>(EditorUI::ActorInfos)])) {
		ImGui::End();
		return;
	}

	auto Data = g_pIGameActor->GetKnowedPortions();

	for (auto Str : Data)
	{
		ImGui::Button(Str.c_str(), { 200, 30 });
	}

	ImGui::End();
}

void RenderUI()
{
	static bool FirstDraw = true;

	CImGuiManager::Instance().Subscribe("Editor Weather Draw", CImGuiManager::ERenderPriority::eMedium, RenderUIWeather);
	CImGuiManager::Instance().Subscribe("Actor InfoPortions", CImGuiManager::ERenderPriority::eMedium, RenderActorInfos);

	CImGuiManager::Instance().Subscribe("LuaDebug", CImGuiManager::ERenderPriority::eLow, []()
	{
		static bool Attach = false;

		if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::LuaDebug)])
			return;

		if (!Attach)
		{
			DebbugerAttach();
			Attach = true;
		}
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
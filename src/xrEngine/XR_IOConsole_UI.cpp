#include "stdafx.h"
#include "xr_ioc_cmd.h"

ICF void DrawFavoriteButton(IConsole_Command* cmd, xr_vector<IConsole_Command*>& fav_commands)
{
    ImGui::SameLine();
    bool is_fav = std::find(fav_commands.begin(), fav_commands.end(), cmd) != fav_commands.end();
	
    ImGui::PushID(cmd);
    if (ImGui::SmallButton(is_fav ? "Unfavorite" : "Favorite"))
    {
        if (is_fav)
        {
            auto it = std::find(fav_commands.begin(), fav_commands.end(), cmd);
            fav_commands.erase(it);
        }
        else
        {
            fav_commands.push_back(cmd);
        }
    }
    ImGui::PopID();
}

ICF void RenderCommandManipulator(IConsole_Command* Command, xr_vector<IConsole_Command*>& fav_commands)
{
	if (CCC_Mask16* ccc_mask16 = Command->dcast_mask16())
	{
		bool val = ccc_mask16->GetValue();
		if (ImGui::Checkbox(ccc_mask16->Name(), &val))
		{
			ccc_mask16->Execute(val ? "1" : "0");
		}
		DrawFavoriteButton(ccc_mask16, fav_commands);
		return;
	}

	if (CCC_Mask32* ccc_mask32 = Command->dcast_mask32())
	{
		bool val = ccc_mask32->GetValue();
		if (ImGui::Checkbox(ccc_mask32->Name(), &val))
		{
			ccc_mask32->Execute(val ? "1" : "0");
		}
		DrawFavoriteButton(ccc_mask32, fav_commands);
		return;
	}

	if (CCC_Mask64* ccc_mask64 = Command->dcast_mask64())
	{
		bool val = ccc_mask64->GetValue();
		if (ImGui::Checkbox(ccc_mask64->Name(), &val))
		{
			ccc_mask64->Execute(val ? "1" : "0");
		}
		DrawFavoriteButton(ccc_mask64, fav_commands);
		return;
	}

	if (CCC_Boolean* ccc_boolean = Command->dcast_bool())
	{
		if (ImGui::Checkbox(ccc_boolean->Name(), ccc_boolean->value))
		{
			ccc_boolean->Execute(*ccc_boolean->value ? "1" : "0");
		}
		DrawFavoriteButton(ccc_boolean, fav_commands);
		return;
	}

	if (CCC_Float* ccc_float = Command->dcast_float())
	{
		float test = ccc_float->GetValue();
		float min = std::clamp(ccc_float->min, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		float max = std::clamp(ccc_float->max, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		if (ImGui::SliderFloat(ccc_float->Name(), &test, min, max))
		{
			clamp(test, min, max);
			string32 String = {};
			xr_sprintf(String, "%.3f", test);
			ccc_float->Execute(String);
		}
		DrawFavoriteButton(ccc_float, fav_commands);
		return;
	}

	if (CCC_Integer* ccc_integer = Command->dcast_int())
	{
		int test = ccc_integer->GetValue();
		if (ImGui::SliderInt(ccc_integer->Name(), &test, ccc_integer->min, ccc_integer->max))
		{
			clamp(test, ccc_integer->min, ccc_integer->max);
			string32 String = {};
			xr_sprintf(String, "%i", test);
			ccc_integer->Execute(String);
		}
		DrawFavoriteButton(ccc_integer, fav_commands);
		return;
	}

	if (CCC_Token* ccc_token = Command->dcast_token())
	{
		int Id = (int)*ccc_token->value;
		xr_token* tok = ccc_token->GetToken();

		const char* Value = "?";
		while (tok->name)
		{
			if (tok->id == Id)
			{
				Value = tok->name;
				break;
			}
			tok++;
		}

		if (ImGui::BeginCombo(ccc_token->Name(), Value))
		{
			int Id = (int)*ccc_token->value;
			xr_token* tok = ccc_token->GetToken();
			while (tok->name)
			{
				if (ImGui::Selectable(tok->name, tok->id == Id))
				{
					ccc_token->Execute(tok->name);
				}
				tok++;
			}
			ImGui::EndCombo();
		}
		DrawFavoriteButton(ccc_token, fav_commands);
		return;
	}

	if (CCC_Vector3* ccc_vector3 = Command->dcast_vector())
	{
		auto& Val = *ccc_vector3->GetValuePtr();
		float min = std::clamp(ccc_vector3->min.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		float max = std::clamp(ccc_vector3->max.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		if (ImGui::SliderFloat3(ccc_vector3->Name(), &Val.x, min, max))
		{
			string64 str = {};
			xr_sprintf(str, sizeof(str), "(%.3f, %.3f, %.3f)", Val.x, Val.y, Val.z);
			ccc_vector3->Execute(str);
		}
		DrawFavoriteButton(ccc_vector3, fav_commands);
	}
}

ICF bool IsPrimitiveWrapperImplemented(IConsole_Command* cmd)
{
	// Нужно чтобы при вводе условного "help" мы не подсчитывали её в списке найденных команд, 
	// поскольку CCC_Help не реализует манипуляцию над командой в виде GUI.
	
	// Если выше добавляется GUI манипулятор над CCC_ обёрткой примитива, просто надо докинуть её в конец,
	// условия иначе команда она не будет подсчитываться и выводиться в поиске.
	return cmd->dcast_bool() ||
		   cmd->dcast_float() ||
		   cmd->dcast_int() ||
		   cmd->dcast_token() ||
		   cmd->dcast_vector() ||
		   cmd->dcast_mask16() ||
		   cmd->dcast_mask32() ||
		   cmd->dcast_mask64();
}

void CConsole::DrawUIConsoleVars()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::CmdVars)])
	{
		return;
	}

	if (!ImGui::Begin("DebugConsoleVars", &Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::CmdVars)]))
	{
		ImGui::End();
		return;
	}

	static u32 results_count = 0;
	static string64 search_query;
	static xr_vector<IConsole_Command*> fav_commands;
	xr_vector<IConsole_Command*> filtered;

	ImGui::Text("Search:");
	ImGui::SameLine();
	ImGui::InputText("", search_query, sizeof(search_query));

	for (const auto& [Name, Command] : Commands)
	{
		if (!IsPrimitiveWrapperImplemented(Command))
		{
			continue;
		}

		if (std::find(fav_commands.begin(), fav_commands.end(), Command) != fav_commands.end())
		{
			continue;
		}
		
		if (search_query[0] != '\0')
		{
			xr_string name_lower(Name);
			transform(name_lower.begin(), name_lower.end(), name_lower.begin(), tolower);
			
			xr_string filtered_lower(search_query);
			transform(filtered_lower.begin(), filtered_lower.end(), filtered_lower.begin(), tolower);
			
			if (name_lower.find(filtered_lower) == xr_string::npos)
			{
				continue;
			}
		}
		filtered.push_back(Command);
	}

	sort(filtered.begin(), filtered.end(), [](IConsole_Command* a, IConsole_Command* b)
	{
		xr_string name_a(a->Name());
		xr_string name_b(b->Name());
		
		transform(name_a.begin(), name_a.end(), name_a.begin(), tolower);
		transform(name_b.begin(), name_b.end(), name_b.begin(), tolower);
		
		return name_a < name_b; 
	});

	string64 search_count_text;
	results_count = filtered.size();
	xr_sprintf(search_count_text, "| Results: %u", results_count);

	ImGui::SameLine();
	ImGui::Text(search_count_text);

	ImGui::Separator();

	if (!fav_commands.empty())
	{
		ImGui::Text("Favorites:");
		for (auto Command : fav_commands)
		{
			RenderCommandManipulator(Command, fav_commands);
		}
		ImGui::Separator();
	}
	
	if (!filtered.empty())
	{
		ImGui::Text("Commands:");
		for (auto Command : filtered)
		{
			RenderCommandManipulator(Command, fav_commands);
		}
	}
	else
	{
		ImGui::Text("Nothing found.");
	}
	ImGui::End();
}

void CConsole::DrawUIConsole()
{
	ImGui::SetNextWindowSize(ImVec2(520, 600), ImGuiCond_FirstUseEver);
	if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::CmdConsole)]) {
		return;
	}

	if (!ImGui::Begin("DebugConsole", &Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::CmdConsole)], ImGuiWindowFlags_NoDecoration)) {
		ImGui::End();
		return;
	}

	xrCriticalSectionGuard guardLog(&m_log_history_guard);
	if (ImGui::BeginChild("DebugConsoleScrollingRegion", ImVec2(0, 0), ImGuiChildFlags_None, ImGuiWindowFlags_NoScrollbar)) {
		if (m_log_history.GetSize() != 0) {
			ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 1));
			shared_str logLine = m_log_history.GetLooped(m_log_history.GetTail());

			const float TextYSize = ImGui::CalcTextSize(logLine.c_str()).y;
			const int MaxTextCount = int(ImGui::GetContentRegionAvail().y / TextYSize) + 1;

			int CursorPos = std::max((int)(m_log_history.GetSize() - MaxTextCount - scroll_delta), 0);
			for (int i = CursorPos; i < (int)m_log_history.GetSize(); i++) {
				logLine = m_log_history.GetLooped(m_log_history.GetTail() - i);

				const char* ls = logLine.c_str();
				if (ls == nullptr) {
					continue;
				}

				bool has_color = false;
				switch (*ls) {
				case '!': ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0.1f, 0.1f, 1.0f)); has_color = true; break;
				case '*': ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.5f, 0.5f, 0.5f, 1.0f)); has_color = true; break;
				case '~': ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 1.0f, 0.2f, 1.0f)); has_color = true; break;
				case '-': ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.0f, 1.0f, 0.1f, 1.0f)); has_color = true; break;
				default:
					break;
				}

				ImGui::TextUnformatted(ls);
				if (has_color) {
					ImGui::PopStyleColor();
				}
			}

			if (scroll_delta == 0) {
				ImGui::SetScrollHereY(1.0f);
			}

			ImGui::PopStyleVar();
		}
	}

	ImGui::EndChild();
	ImGui::End();
}
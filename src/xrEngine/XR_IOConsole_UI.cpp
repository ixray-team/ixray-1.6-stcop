#include "stdafx.h"
#include "xr_ioc_cmd.h"

void CConsole::DrawUIConsoleVars()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::CmdVars)]) {
		return;
	}

	if (!ImGui::Begin("DebugConsoleVars", &Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::CmdVars)])) {
		ImGui::End();
		return;
	}

	static string64 search_query;
	
	ImGui::Text("Search:");
	ImGui::SameLine();
	ImGui::InputText("", search_query, sizeof(search_query));
	
	std::vector<IConsole_Command*> filtered;
	
	for (const auto& [Name, Command] : Commands)
	{
		// Aphile: да это ужасно, надо потом переделать.
		if (smart_cast<CCC_Boolean*>(Command) || 
			smart_cast<CCC_Float*>(Command) || 
			smart_cast<CCC_Integer*>(Command) ||
			smart_cast<CCC_Token*>(Command) ||
			smart_cast<CCC_Vector3*>(Command) ||
			smart_cast<CCC_Mask16*>(Command) ||
			smart_cast<CCC_Mask32*>(Command) ||
			smart_cast<CCC_Mask64*>(Command)
			)
		{
			if (search_query[0] != '\0')
			{
				xr_string name_lower(Name);
				transform(name_lower.begin(), name_lower.end(), name_lower.begin(), tolower);
				
				xr_string filtered_lower(search_query);
				transform(filtered_lower.begin(), filtered_lower.end(), filtered_lower.begin(), tolower);
				
				if (name_lower.find(filtered_lower) == std::string::npos)
				{
					continue;
				}
			}
			filtered.push_back(Command);
		}
	}

	std::sort(filtered.begin(), filtered.end(), [](IConsole_Command* a, IConsole_Command* b)
	{
		xr_string name_a(a->Name());
		xr_string name_b(b->Name());
		
		transform(name_a.begin(), name_a.end(), name_a.begin(), tolower);
		transform(name_b.begin(), name_b.end(), name_b.begin(), tolower);
		
		return name_a < name_b; 
	});

	static u32 results_count = 0;

	string64 search_count_text;
	results_count = filtered.size();
	xr_sprintf(search_count_text, "| Results: %u", results_count);
	
	ImGui::SameLine();
	ImGui::Text(search_count_text);
	ImGui::Separator();

	for (auto Command : filtered) 
	{
		if (auto Mask16 = Command->dcast_mask16())
		{
			bool val = Mask16->GetValue();
			if (ImGui::Checkbox(Mask16->Name(), &val))
			{
				Mask16->Execute(val ? "1" : "0");
			}
			continue;
		}

		if (auto Mask32 = Command->dcast_mask32())
		{
			bool val = Mask32->GetValue();
			if (ImGui::Checkbox(Mask32->Name(), &val))
			{
				Mask32->Execute(val ? "1" : "0");
			}
			continue;
		}

		if (auto Mask64 = Command->dcast_mask64())
		{
			bool val = Mask64->GetValue();
			if (ImGui::Checkbox(Mask64->Name(), &val))
			{
				Mask64->Execute(val ? "1" : "0");
			}
			continue;
		}

		if (auto Boolean = dynamic_cast<CCC_Boolean*>(Command))
		{
			if (ImGui::Checkbox(Boolean->Name(), Boolean->value))
			{
				Boolean->Execute(*Boolean->value ? "1" : "0");
			}
			continue;
		}

		if (auto Float = dynamic_cast<CCC_Float*>(Command))
		{
			float test = Float->GetValue();
			float min = std::clamp(Float->min, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			float max = std::clamp(Float->max, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			if (ImGui::SliderFloat(Float->Name(), &test, min, max))
			{
				string32 String = {};
				xr_sprintf(String, "%.3f", test);
				Float->Execute(String);
			}
			continue;
		}

		if (auto Integer = dynamic_cast<CCC_Integer*>(Command))
		{
			int test = Integer->GetValue();
			
			if (ImGui::SliderInt(Integer->Name(), &test, Integer->min, Integer->max))
			{
				string32 String = {};
				xr_sprintf(String, "%i", test);
				Integer->Execute(String);
			}
			continue;
		}

		if (auto Token = dynamic_cast<CCC_Token*>(Command))
		{
			int Id = (int)*Token->value;
			xr_token* tok = Token->GetToken();

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

			if (ImGui::BeginCombo(Token->Name(), Value))
			{
				int Id = (int)*Token->value;
				xr_token* tok = Token->GetToken();
				while (tok->name)
				{
					if (ImGui::Selectable(tok->name, tok->id == Id))
					{
						Token->Execute(tok->name);
					}
					tok++;
				}
				ImGui::EndCombo();
			}
			continue;
		}

		if (auto Vector = dynamic_cast<CCC_Vector3*>(Command))
		{
			auto& Val = *Vector->GetValuePtr();
			float min = std::clamp(Vector->min.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			float max = std::clamp(Vector->max.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			
			if (ImGui::SliderFloat3(Vector->Name(), &Val.x, min, max))
			{
				string64 str = {};
				xr_sprintf(str, sizeof(str), "(%.3f, %.3f, %.3f)", Val.x, Val.y, Val.z);
				Vector->Execute(str);
			}
		}
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
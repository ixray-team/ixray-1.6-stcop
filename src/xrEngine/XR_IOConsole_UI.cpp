#include "stdafx.h"
#include "xr_ioc_cmd.h"

#ifdef DEBUG_DRAW
#include <imgui.h>
#include <imgui_internal.h>

auto FindFavIterator(xr_vector<const char*>& FavCommands, const char* Name)
{
	return std::ranges::find_if(FavCommands, [Name](const char* Fav)
	{
		return xr_strcmp(Fav, Name) == 0;
	});
}

ICF void DrawFavoriteButton(IConsole_Command* Cmd, xr_vector<const char*>& FavCommands)
{
    ImGui::SameLine();
	
    auto It = FindFavIterator(FavCommands, Cmd->Name());
    bool IsFav = It != FavCommands.end();
	
    ImGui::PushID(Cmd);
    if (ImGui::SmallButton(IsFav ? "Unfavorite" : "Favorite"))
    {
        if (IsFav)
        {
            xr_free(*It);
            FavCommands.erase(It);
        }
        else
        {
            FavCommands.push_back(xr_strdup(Cmd->Name()));
        }
        ImGui::MarkIniSettingsDirty();
    }
    ImGui::PopID();
}

ICF void RenderCommandManipulator(IConsole_Command* Command, xr_vector<const char*>& FavCommands)
{
	if (CCC_Mask16* Mask16 = Command->dcast_mask16())
	{
		bool Val = Mask16->GetValue();
		if (ImGui::Checkbox(Mask16->Name(), &Val))
		{
			Mask16->Execute(Val ? "1" : "0");
		}
		DrawFavoriteButton(Mask16, FavCommands);
		return;
	}

	if (CCC_Mask32* Mask32 = Command->dcast_mask32())
	{
		bool Val = Mask32->GetValue();
		if (ImGui::Checkbox(Mask32->Name(), &Val))
		{
			Mask32->Execute(Val ? "1" : "0");
		}
		DrawFavoriteButton(Mask32, FavCommands);
		return;
	}

	if (CCC_Mask64* Mask64 = Command->dcast_mask64())
	{
		bool Val = Mask64->GetValue();
		if (ImGui::Checkbox(Mask64->Name(), &Val))
		{
			Mask64->Execute(Val ? "1" : "0");
		}
		DrawFavoriteButton(Mask64, FavCommands);
		return;
	}

	if (CCC_Boolean* Boolean = Command->dcast_bool())
	{
		if (ImGui::Checkbox(Boolean->Name(), Boolean->value))
		{
			Boolean->Execute(*Boolean->value ? "1" : "0");
		}
		DrawFavoriteButton(Boolean, FavCommands);
		return;
	}

	if (CCC_Float* Float = Command->dcast_float())
	{
		float Test = Float->GetValue();
		float Min = std::clamp(Float->min, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		float Max = std::clamp(Float->max, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		if (ImGui::SliderFloat(Float->Name(), &Test, Min, Max))
		{
			clamp(Test, Min, Max);
			string32 String = {};
			xr_sprintf(String, "%.3f", Test);
			Float->Execute(String);
		}
		DrawFavoriteButton(Float, FavCommands);
		return;
	}

	if (CCC_Integer* Integer = Command->dcast_int())
	{
		int Value = Integer->GetValue();
		if (ImGui::SliderInt(Integer->Name(), &Value, Integer->min, Integer->max))
		{
			clamp(Value, Integer->min, Integer->max);
			string32 String = {};
			xr_sprintf(String, "%i", Value);
			Integer->Execute(String);
		}
		DrawFavoriteButton(Integer, FavCommands);
		return;
	}

	if (CCC_Token* Token = Command->dcast_token())
	{
		int Id = (int)*Token->value;
		xr_token* TokenEntry = Token->GetToken();

		const char* Value = "?";
		while (TokenEntry->name)
		{
			if (TokenEntry->id == Id)
			{
				Value = TokenEntry->name;
				break;
			}
			TokenEntry++;
		}

		if (ImGui::BeginCombo(Token->Name(), Value))
		{
			int Id = (int)*Token->value;
			xr_token* TokenEntry = Token->GetToken();
			while (TokenEntry->name)
			{
				if (ImGui::Selectable(TokenEntry->name, TokenEntry->id == Id))
				{
					Token->Execute(TokenEntry->name);
				}
				TokenEntry++;
			}
			ImGui::EndCombo();
		}
		DrawFavoriteButton(Token, FavCommands);
		return;
	}

	if (CCC_Vector3* Vector3D = Command->dcast_vector())
	{
		auto& Val = *Vector3D->GetValuePtr();
		float Min = std::clamp(Vector3D->min.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		float Max = std::clamp(Vector3D->max.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
		if (ImGui::SliderFloat3(Vector3D->Name(), &Val.x, Min, Max))
		{
			string64 Str = {};
			xr_sprintf(Str, sizeof(Str), "(%.3f, %.3f, %.3f)", Val.x, Val.y, Val.z);
			Vector3D->Execute(Str);
		}
		DrawFavoriteButton(Vector3D, FavCommands);
	}
}

ICF bool IsPrimitiveWrapperImplemented(IConsole_Command* Cmd)
{
	return Cmd->dcast_bool() ||
		   Cmd->dcast_float() ||
		   Cmd->dcast_int() ||
		   Cmd->dcast_token() ||
		   Cmd->dcast_vector() ||
		   Cmd->dcast_mask16() ||
		   Cmd->dcast_mask32() ||
		   Cmd->dcast_mask64();
}

xr_vector<const char*> ImGuiConsoleDebugVarsSavedFavs;

void* CConsole::ImGuiReadOpenUIConsoleVars(ImGuiContext* Ctx, ImGuiSettingsHandler* Handler, const char* Name)
{
	if (strcmp(Name, "Favorites") == 0)
	{
		for (const char* Cmd : ImGuiConsoleDebugVarsSavedFavs)
		{
			xr_free(Cmd);
		}

		ImGuiConsoleDebugVarsSavedFavs.clear();
		return &ImGuiConsoleDebugVarsSavedFavs;
	}
	return nullptr;
}

void CConsole::ImGuiReadLineUIConsoleVars(ImGuiContext* Ctx, ImGuiSettingsHandler* Handler, void* Entry, const char* Line)
{
	if (Line != nullptr)
	{
		ImGuiConsoleDebugVarsSavedFavs.push_back(xr_strdup(Line));
	}
}

void CConsole::ImGuiWriteAllUIConsoleVars(ImGuiContext* Ctx, ImGuiSettingsHandler* Handler, ImGuiTextBuffer* OutBuf)
{
	OutBuf->appendf("[%s][Favorites]\n", Handler->TypeName);
	for (const char* cmd : ImGuiConsoleDebugVarsSavedFavs)
	{
		OutBuf->appendf("%s\n", cmd);
	}
	OutBuf->appendf("\n");  
}

void CConsole::ImGuiDrawUIConsoleVars()
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

	static u32 ResultsCount = 0;
	static string64 SearchQuery = "";
	xr_vector<IConsole_Command*> FilteredCommands;

	ImGui::Text("Search:");
	ImGui::SameLine();
	ImGui::InputText("", SearchQuery, sizeof(SearchQuery));

	for (const auto& [Name, Command] : Commands)
	{
		if (!IsPrimitiveWrapperImplemented(Command))
		{
			continue;
		}
		
		if (FindFavIterator(ImGuiConsoleDebugVarsSavedFavs, Name) != ImGuiConsoleDebugVarsSavedFavs.end())
		{
			continue;
		}
		
		if (SearchQuery[0] != '\0')
		{
			xr_string name_lower(Name);
			transform(name_lower.begin(), name_lower.end(), name_lower.begin(), tolower);
			
			xr_string filtered_lower(SearchQuery);
			transform(filtered_lower.begin(), filtered_lower.end(), filtered_lower.begin(), tolower);
			
			if (name_lower.find(filtered_lower) == xr_string::npos)
			{
				continue;
			}
		}
		FilteredCommands.push_back(Command);
	}

	sort(FilteredCommands.begin(), FilteredCommands.end(), [](IConsole_Command* a, IConsole_Command* b)
	{
		xr_string name_a(a->Name());
		xr_string name_b(b->Name());
		
		transform(name_a.begin(), name_a.end(), name_a.begin(), tolower);
		transform(name_b.begin(), name_b.end(), name_b.begin(), tolower);
		
		return name_a < name_b; 
	});

	string64 SearchResultsCount;
	ResultsCount = FilteredCommands.size();
	xr_sprintf(SearchResultsCount, "| Results: %u", ResultsCount);

	ImGui::SameLine();
	ImGui::Text(SearchResultsCount);

	ImGui::Separator();
	
	if (!ImGuiConsoleDebugVarsSavedFavs.empty())
	{
		ImGui::Text("Favorites:");
		
		for (const char* FavName : ImGuiConsoleDebugVarsSavedFavs)
		{
			if (auto It = Commands.find(FavName); It != Commands.end())
			{
				RenderCommandManipulator(It->second, ImGuiConsoleDebugVarsSavedFavs);
			}
		}
		ImGui::Separator();
	}
	
	if (!FilteredCommands.empty())
	{
		ImGui::Text("Commands:");
		for (auto Command : FilteredCommands)
		{
			RenderCommandManipulator(Command, ImGuiConsoleDebugVarsSavedFavs);
		}
	}
	else
	{
		ImGui::Text("Nothing found.");
	}
	ImGui::End();
}

void CConsole::ImGuiDrawUIConsole()
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

#endif
#include "stdafx.h"
#include "XR_IOConsole.h"
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

	for (const auto& [Name, Command] : Commands) {
		if (auto Mask = dynamic_cast<CCC_Mask*>(Command)) {
			continue;
		}

		if (auto Boolean = dynamic_cast<CCC_Boolean*>(Command)) {
			if (ImGui::Checkbox(Boolean->Name(), Boolean->value)) {
				Boolean->Execute(*Boolean->value ? "1" : "0");
			}
			continue;
		}

		if (auto Float = dynamic_cast<CCC_Float*>(Command)) {
			float test = Float->GetValue();
			float min = std::clamp(Float->min, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			float max = std::clamp(Float->max, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			if (ImGui::SliderFloat(Float->Name(), &test, min, max)) {
				string32 String = {};
				xr_sprintf(String, "%3.5f", test);
				Float->Execute(String);
			}

			continue;
		}

		if (auto Integer = dynamic_cast<CCC_Integer*>(Command)) {
			int test = Integer->GetValue();
			if (ImGui::SliderInt(Integer->Name(), &test, Integer->min, Integer->max)) {
				string32 String = {};
				xr_sprintf(String, "%i", test);
				Integer->Execute(String);
			}

			continue;
		}

		if (auto Token = dynamic_cast<CCC_Token*>(Command)) {
			int Id = (int)*Token->value;
			xr_token* tok = Token->GetToken();

			const char* Value = "?";
			while (tok->name) {
				if (tok->id == Id) {
					Value = tok->name;
					break;
				}
				tok++;
			}

			if (ImGui::BeginCombo(Token->Name(), Value)) {
				int Id = (int)*Token->value;
				xr_token* tok = Token->GetToken();
				while (tok->name) {
					if (ImGui::Selectable(tok->name, tok->id == Id)) {
						Token->Execute(tok->name);
					}
					tok++;
				}
				ImGui::EndCombo();
			}
			continue;
		}

		if (auto Vector = dynamic_cast<CCC_Vector3*>(Command)) {
			auto& Val = *Vector->GetValuePtr();
			float min = std::clamp(Vector->min.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			float max = std::clamp(Vector->max.x, -FLT_MAX / 2.0f, +FLT_MAX / 2.0f);
			if (ImGui::SliderFloat3(Vector->Name(), &Val.x, min, max)) {
				string64 str = {};
				xr_sprintf(str, sizeof(str), "(%f, %f, %f)", Val.x, Val.y, Val.z);
				Vector->Execute(str);
			}

			continue;
		}
	}
	ImGui::End();
}

void CConsole::DrawUIConsole()
{
	constexpr u32 MaxHintCommands = 5;
	char InputBuf[256] = {};

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

				LPCSTR ls = logLine.c_str();
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
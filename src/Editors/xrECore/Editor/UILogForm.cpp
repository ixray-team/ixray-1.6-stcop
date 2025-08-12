#include "stdafx.h"
#include "ELog.h"
#include "UILogForm.h"
#include "..\XrCore\os_clipboard.h"
#include "..\XrEngine\XR_IOConsole.h"
#include "..\xrEUI\xrUITheme.h"

#define MSG_ERROR 	0x00C4C4FF
#define MSG_INFO  	0x00E6FFE7
#define MSG_CONF 	0x00FFE6E7
#define MSG_DEF  	0x00E8E8E8

bool UILogForm::bAutoScroll = true;
bool UILogForm::bClearInPIE = false;
string_path UILogForm::m_Filter="";
string_path UILogForm::m_Exec="";
xr_vector<xr_string>* UILogForm::List = nullptr;
extern bool bAllowLogCommands;

static xrCriticalSection LogGuard;

void UILogForm::AddMessage( const xr_string& msg)
{
	xr_string M;
	for (int i = 0; i < msg.size(); i++)
	{
		if (msg[i] == '\r') continue;
		if (msg[i] == '\n') M += " ";
		else M += msg[i];
	}

	xrCriticalSectionGuard cs(LogGuard);
	GetList()->emplace_back(M);
}


void UILogForm::Show()
{
	bAllowLogCommands = true;
}

void UILogForm::SetActive()
{
	if (!bAllowLogCommands)
		bAllowLogCommands = true;

	//ImGui::SetWindowFocus("Log");
}

void UILogForm::Hide()
{
	bAllowLogCommands = false;
}

void UILogForm::Update()
{
	static bool FirstRun = false;
	if (bAllowLogCommands)
	{
		bool NeedCopy = false;
		if (!ImGui::Begin("Log", &bAllowLogCommands))
		{
			ImGui::End();
			return;
		}

		if (ImGui::Button("Clear")) 
		{
			Clear();
		}
		ImGui::SameLine();

		if (ImGui::Button("Copy"))
		{
			NeedCopy = true;
		}
		ImGui::SameLine();

		ImGui::Checkbox("Auto Scroll", &bAutoScroll); 
		ImGui::SameLine();
		ImGui::SetNextItemWidth(-1);
		ImGui::InputTextWithHint("##SearchFilter", "Search", m_Filter, sizeof(m_Filter));

		ImGui::Spacing();
		if (ImGui::BeginChild("Log##child",ImVec2(0, -ImGui::GetFrameHeightWithSpacing()),true))
		{
			xr_string CopyLog;
			for (int i = 0; i < GetList()->size(); i++)
			{
				CUIThemeManager& theme_manager = CUIThemeManager::Get();
				ImVec4 Color = theme_manager.log_color_default;
				const char* Str = GetList()->at(i).c_str();

				if (Str == nullptr || xr_strlen(Str) == 0)
					continue;

				if (m_Filter[0] && strstr(Str, m_Filter)==0)
				{
					continue;
				}
				if (strncmp(Str, "! ", 2) == 0)
				{
					Color = theme_manager.log_color_error;
				}
				if (strncmp(Str, "~ ", 2) == 0)
				{
					Color = theme_manager.log_color_warning;
				}
				if (strncmp(Str, "* ", 2) == 0)
				{
					Color = theme_manager.log_color_debug;
				}

				ImGui::PushStyleColor(ImGuiCol_Text, Color);
				if (strlen(Str) > 0 && ImGui::Selectable(Str))
				{
					os_clipboard::copy_to_clipboard(Str);
				}
				else
				{
					if (NeedCopy)
						CopyLog.append(Str).append("\r\n");
				}
				ImGui::PopStyleColor();
			}

			if (NeedCopy)
			{
				os_clipboard::copy_to_clipboard(CopyLog.c_str());
			}
			if (bAutoScroll && ImGui::GetScrollY() >= ImGui::GetScrollMaxY()|| FirstRun==false)
				ImGui::SetScrollHereY();
			FirstRun = true;
		}
		ImGui::EndChild();
		ImGuiInputTextFlags input_text_flags = ImGuiInputTextFlags_EnterReturnsTrue ;
		if (ImGui::InputText("Exec", m_Exec, IM_ARRAYSIZE(m_Exec), input_text_flags))
		{
			if (m_Exec[0])
			{
				Msg("~ Exec %s", m_Exec);
				Console->Execute(m_Exec);
			}
		
		}
		ImGui::SameLine();
		ImGui::Checkbox("Clear in PIE", &bClearInPIE);
		ImGui::End();
	}
	else
	{
		FirstRun = false;
	}
}

void UILogForm::Clear()
{
	GetList()->clear();
}

void UILogForm::Destroy()
{
	xr_delete(List);
}

bool UILogForm::ClearInPIE()
{
	return bClearInPIE;
}

xr_vector<xr_string>* UILogForm::GetList()
{
	if (!List)List = new xr_vector<xr_string>();
	return List;
}

#include "stdafx.h"
#include "ELog.h"
#include "UILogForm.h"
#include "..\XrCore\os_clipboard.h"
#include "..\XrEngine\XR_IOConsole.h"
#define MSG_ERROR 	0x00C4C4FF
#define MSG_INFO  	0x00E6FFE7
#define MSG_CONF 	0x00FFE6E7
#define MSG_DEF  	0x00E8E8E8
bool UILogForm::bAutoScroll = true;
string_path UILogForm::m_Filter="";
string_path UILogForm::m_Exec="";
xr_vector<xr_string>* UILogForm::List = nullptr;
extern bool bAllowLogCommands;
void UILogForm::AddMessage( const xr_string& msg)
{
	xr_string M;
	for (int i = 0; i < msg.size(); i++)
	{
		if (msg[i] == '\r') continue;
		if (msg[i] == '\n') M += " ";
		else M += msg[i];
	}

	GetList()->push_back(M);
}


void UILogForm::Show()
{
	bAllowLogCommands = true;
}

void UILogForm::Hide()
{
	bAllowLogCommands = false;
}

void UILogForm::Update()
{
	static bool FistRun = false;
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
			GetList()->clear();
		}ImGui::SameLine();
		if (ImGui::Button("Flush")) 
		{
			FlushLog();
		}ImGui::SameLine();
		if (ImGui::Button("Copy"))
		{
			NeedCopy = true;
		}ImGui::SameLine();
		ImGui::Checkbox("Auto Scroll", &bAutoScroll); 
		ImGui::SameLine();
		ImGui::InputText("Filter", m_Filter, sizeof(m_Filter));;
	

		ImGui::Spacing();
		if (ImGui::BeginChild("Log",ImVec2(0, -ImGui::GetFrameHeightWithSpacing()),true))
		{
			xr_string CopyLog;
			for (int i = 0; i < GetList()->size(); i++)
			{

				ImVec4 Color = { 1,1,1,1 };
				const char* Str = GetList()->at(i).c_str();
				if (m_Filter[0] && strstr(Str, m_Filter)==0)
				{
					continue;
				}
				if (strncmp(Str, "! ", 2) == 0)
				{
					Color = { 1,0,0,1 };
				}
				if (strncmp(Str, "~ ", 2) == 0)
				{
					Color = { 1,1,0,1 };
				}
				if (strncmp(Str, "* ", 2) == 0)
				{
					Color = { 0.5,0.5,0.5,1 };
				}

				ImGui::TextColored(Color, Str);
				if (NeedCopy)
					CopyLog.append(Str).append("\r\n");


			}

			if (NeedCopy)
			{
				os_clipboard::copy_to_clipboard(CopyLog.c_str());
			}
			if (bAutoScroll && ImGui::GetScrollY() >= ImGui::GetScrollMaxY()|| FistRun==false)ImGui::SetScrollHereY();

			FistRun = true;
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
		ImGui::End();
	}
	else
	{
		FistRun = false;
	}
}

void UILogForm::Destroy()
{
	xr_delete(List);
}

xr_vector<xr_string>* UILogForm::GetList()
{
	if (!List)List = xr_new<xr_vector<xr_string>>();
	return List;
}

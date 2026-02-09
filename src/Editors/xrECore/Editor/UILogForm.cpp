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
string_path UILogForm::Filter ="";
string_path UILogForm::Exec="";
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
}

void UILogForm::Hide()
{
	bAllowLogCommands = false;
}

void UILogForm::Update()
{
	static bool FirstRun = false;

	if (!bAllowLogCommands)
	{
		FirstRun = false;
		return;
	}

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(3, 3));
	bool NeedCopy = false;
	if (!ImGui::Begin("Log", &bAllowLogCommands))
	{
		ImGui::PopStyleVar();
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
	XRay::ImGui::ToggleButton("Auto Scroll", &bAutoScroll, { 0, 0 });
	ImGui::SameLine();
	XRay::ImGui::ToggleButton("Clear In PIE", &bClearInPIE, { 0, 0 });

	ImGui::SameLine();
	ImGui::SetNextItemWidth(-1);
	ImGui::InputTextWithHint("##SearchFilter", "Search", Filter, sizeof(Filter));

	if (GUIManager->SearchIcon)
	{
		static const ImVec2 IconSize = { 12, 12 };
		ImGui::SameLine();
		ImVec2 CursorPos = ImGui::GetCursorPos();
		ImGui::SetCursorPos(ImVec2(CursorPos.x - IconSize.x - 10.f, 1 + CursorPos.y + (IconSize.y / 4)));
		ImGui::Image(GUIManager->SearchIcon, IconSize);
	}
	ImGui::PopStyleVar();

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_ChildRounding, 0.0f);
	ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBackgroundTint).Value);
	if (ImGui::BeginChild("Log##Child", ImVec2(0, -ImGui::GetFrameHeightWithSpacing() - 4), true))
	{
		xrCriticalSectionGuard Cs(LogGuard);

		xr_vector<const char*> Visible;
		Visible.reserve(GetList()->size());

		for (const auto& S : *GetList())
		{
			const char* Str = S.c_str();
			if (!Str || !*Str)
			{
				continue;
			}

			if (Filter[0] && strstr(Str, Filter) == nullptr)
			{
				continue;
			}

			Visible.push_back(Str);
		}

		ImGuiListClipper Clipper;
		Clipper.Begin(static_cast<int>(Visible.size()));
		ImGui::Indent(10.f);
		xr_string CopyLog;

		while (Clipper.Step())
		{
			for (int I = Clipper.DisplayStart; I < Clipper.DisplayEnd; ++I)
			{
				const char* Str = Visible[I];
				CUIThemeManager& ThemeManager = CUIThemeManager::Get();
				ImVec4 Color = ThemeManager.log_color_default;

				if (strncmp(Str, "! ", 2) == 0)
				{
					Color = ThemeManager.log_color_error;
				}
				else if (strncmp(Str, "~ ", 2) == 0)
				{
					Color = ThemeManager.log_color_warning;
				}
				else if (strncmp(Str, "* ", 2) == 0)
				{
					Color = ThemeManager.log_color_debug;
				}

				ImGui::PushStyleColor(ImGuiCol_Text, Color);

				string256 StrLog = {};
				xr_sprintf(StrLog, "%s##%d", Str, I);

				if (ImGui::Selectable(StrLog))
				{
					os_clipboard::copy_to_clipboard(Str);
				}
				else if (NeedCopy)
				{
					CopyLog.append(Str).append("\r\n");
				}

				ImGui::PopStyleColor();
			}
		}

		if (NeedCopy)
		{
			os_clipboard::copy_to_clipboard(CopyLog.c_str());
		}

		if ((bAutoScroll && ImGui::GetScrollY() >= ImGui::GetScrollMaxY()) || !FirstRun)
		{
			ImGui::SetScrollHereY();
		}

		FirstRun = true;
	}
	ImGui::EndChild();

	ImGui::PopStyleColor();
	ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0f);
	if (ImGui::BeginChild("##LogExecChild"))
	{
		ImGuiInputTextFlags InputTextFlags = ImGuiInputTextFlags_EnterReturnsTrue;
		ImGui::SetNextItemWidth(-1);
		if (ImGui::InputTextWithHint("##Exec", "Execute console command", Exec, IM_ARRAYSIZE(Exec), InputTextFlags))
		{
			if (Exec[0])
			{
				Msg("~ Exec %s", Exec);
				Console->Execute(Exec);
			}
		}
	}
	ImGui::EndChild();
	ImGui::PopStyleVar(3);

	ImGui::End();
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

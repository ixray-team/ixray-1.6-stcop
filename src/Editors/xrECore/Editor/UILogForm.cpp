#include "stdafx.h"
#include "ELog.h"
#include "UILogForm.h"
#include "..\xrCore\Kernel/os_clipboard.h"
#include "..\xrEngine\XR_IOConsole.h"
#include "..\xrEngine\xr_ioc_cmd.h"
#include "..\xrEUI\xrUITheme.h"

#define MSG_ERROR 	0x00C4C4FF
#define MSG_INFO  	0x00E6FFE7
#define MSG_CONF 	0x00FFE6E7
#define MSG_DEF  	0x00E8E8E8

bool UILogForm::bAutoScroll = true;
bool UILogForm::bClearInPIE = false;
bool (*UILogForm::HideFunctor)() = nullptr;
string_path UILogForm::Filter ="";
string_path UILogForm::Exec="";
xr_vector<xr_string>* UILogForm::List = nullptr;
xr_vector<IConsole_Command*> UILogForm::Suggestions;
int UILogForm::SuggestionIndex = -1;
ImVec2 UILogForm::SuggestionsMin = ImVec2(0, 0);
ImVec2 UILogForm::SuggestionsMax = ImVec2(0, 0);

bool UILogForm::IsSuggestionsHovered()
{
	return SuggestionsMax.x > SuggestionsMin.x && ImGui::IsMouseHoveringRect(SuggestionsMin, SuggestionsMax, false);
}
bool UILogForm::IsExecFocusRequested = false;
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

	if (HideFunctor != nullptr && HideFunctor())
	{
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
		CollectSuggestions();

		ImGuiInputTextFlags InputTextFlags = ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CallbackCompletion | ImGuiInputTextFlags_CallbackHistory;
		ImGui::SetNextItemWidth(-1);
		if (IsExecFocusRequested)
		{
			// The input was inactive while a suggestion was clicked, so it reloads Exec on activation
			ImGui::SetKeyboardFocusHere();
			IsExecFocusRequested = false;
		}
		const bool IsEntered = ImGui::InputTextWithHint("##Exec", "Execute console command", Exec, IM_ARRAYSIZE(Exec), InputTextFlags, &UILogForm::ExecInputCallback);
		// Keep the list while the mouse is on it: clicking it takes the focus from the input
		const bool IsInputActive = ImGui::IsItemActive() || IsSuggestionsHovered();
		const ImVec2 InputMin = ImGui::GetItemRectMin();
		const float InputWidth = ImGui::GetItemRectSize().x;

		if (IsEntered)
		{
			if (Exec[0])
			{
				Msg("~ Exec %s", Exec);
				Console->Execute(Exec);
				Exec[0] = 0;
			}
			SuggestionIndex = -1;
			ImGui::SetKeyboardFocusHere(-1);
		}

		DrawSuggestions(IsInputActive, InputMin, InputWidth);
	}
	ImGui::EndChild();
	ImGui::PopStyleVar(3);

	ImGui::End();
}

void UILogForm::CollectSuggestions()
{
	Suggestions.clear();

	// Match only the command name: stop at the first space
	xr_string Prefix;
	for (const char* C = Exec; *C && *C != ' '; ++C)
	{
		Prefix += static_cast<char>(tolower(*C));
	}

	const bool HasArgs = strchr(Exec, ' ') != nullptr;
	if (Prefix.empty() || !Console)
	{
		SuggestionIndex = -1;
		return;
	}

	xr_vector<IConsole_Command*> Contains;
	for (const auto& [Name, Command] : Console->Commands)
	{
		if (HasArgs)
		{
			// Arguments typed: show only the exact command for its status and syntax
			if (Prefix == Name)
			{
				Suggestions.push_back(Command);
			}
			continue;
		}

		if (strncmp(Name, Prefix.c_str(), Prefix.size()) == 0)
		{
			Suggestions.push_back(Command);
		}
		else if (strstr(Name, Prefix.c_str()) != nullptr)
		{
			Contains.push_back(Command);
		}
	}

	Suggestions.insert(Suggestions.end(), Contains.begin(), Contains.end());
	if (Suggestions.size() > MaxSuggestions)
	{
		Suggestions.resize(MaxSuggestions);
	}

	if (SuggestionIndex >= static_cast<int>(Suggestions.size()))
	{
		SuggestionIndex = -1;
	}
}

void UILogForm::DrawSuggestions(bool IsInputActive, const ImVec2& InputMin, float InputWidth)
{
	if (!IsInputActive || Suggestions.empty())
	{
		SuggestionsMax = SuggestionsMin = ImVec2(0, 0);
		return;
	}

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(8, 4));
	const float RowHeight = ImGui::GetTextLineHeightWithSpacing();
	const float Height = RowHeight * static_cast<float>(Suggestions.size()) + ImGui::GetStyle().WindowPadding.y * 2.f;
	ImGui::SetNextWindowPos(ImVec2(InputMin.x, InputMin.y - Height - 2.f));
	ImGui::SetNextWindowSize(ImVec2(InputWidth, Height));

	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings |
		ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_Tooltip;

	if (ImGui::Begin("##LogExecSuggestions", nullptr, Flags))
	{
		// Align values and info into columns
		constexpr float ColumnGap = 16.f;
		float NameWidth = 0.f;
		float StatusWidth = 0.f;
		for (IConsole_Command* Command : Suggestions)
		{
			IConsole_Command::TStatus Status = {};
			Command->Status(Status);
			NameWidth = std::max(NameWidth, ImGui::CalcTextSize(Command->Name()).x);
			StatusWidth = std::max(StatusWidth, ImGui::CalcTextSize(Status).x);
		}
		const float StatusX = ImGui::GetCursorPosX() + NameWidth + ColumnGap;
		const float InfoX = StatusX + StatusWidth + ColumnGap;

		for (int I = 0; I < static_cast<int>(Suggestions.size()); ++I)
		{
			IConsole_Command* Command = Suggestions[I];

			IConsole_Command::TStatus Status = {};
			Command->Status(Status);
			IConsole_Command::TInfo Info = {};
			Command->Info(Info);

			ImGui::PushID(I);
			ImGui::Selectable(Command->Name(), I == SuggestionIndex, ImGuiSelectableFlags_SpanAllColumns);
			// Test the click by rect: the press deactivates the input, and the list window may not get the hover
			const ImVec2 RowMin(SuggestionsMin.x, ImGui::GetItemRectMin().y);
			const ImVec2 RowMax(SuggestionsMax.x, ImGui::GetItemRectMax().y);
			if (ImGui::IsMouseClicked(ImGuiMouseButton_Left) && ImGui::IsMouseHoveringRect(RowMin, RowMax, false))
			{
				xr_sprintf(Exec, "%s ", Command->Name());
				SuggestionIndex = -1;
				IsExecFocusRequested = true;
			}
			ImGui::PopID();

			if (Status[0])
			{
				ImGui::SameLine(StatusX);
				ImGui::TextColored(CUIThemeManager::Get().log_color_warning, "%s", Status);
			}
			ImGui::SameLine(InfoX);
			ImGui::TextDisabled("%s", Info);

			if (I == SuggestionIndex)
			{
				ImGui::SetScrollHereY();
			}
		}
	}
	SuggestionsMin = ImGui::GetWindowPos();
	SuggestionsMax = ImVec2(SuggestionsMin.x + ImGui::GetWindowWidth(), SuggestionsMin.y + ImGui::GetWindowHeight());
	ImGui::End();
	ImGui::PopStyleVar();
}

int UILogForm::ExecInputCallback(ImGuiInputTextCallbackData* Data)
{
	const int Count = static_cast<int>(Suggestions.size());

	if (Data->EventFlag == ImGuiInputTextFlags_CallbackHistory && Count > 0)
	{
		if (Data->EventKey == ImGuiKey_UpArrow)
		{
			SuggestionIndex = SuggestionIndex <= 0 ? Count - 1 : SuggestionIndex - 1;
		}
		else if (Data->EventKey == ImGuiKey_DownArrow)
		{
			SuggestionIndex = SuggestionIndex >= Count - 1 ? 0 : SuggestionIndex + 1;
		}
		return 0;
	}

	if (Data->EventFlag == ImGuiInputTextFlags_CallbackCompletion && Count > 0)
	{
		const int Index = SuggestionIndex >= 0 ? SuggestionIndex : 0;
		string_path NewText;
		xr_sprintf(NewText, "%s ", Suggestions[Index]->Name());
		Data->DeleteChars(0, Data->BufTextLen);
		Data->InsertChars(0, NewText);
		SuggestionIndex = -1;
	}

	return 0;
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

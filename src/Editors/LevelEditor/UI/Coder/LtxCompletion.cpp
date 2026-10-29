#include "stdafx.h"
#include "LtxCompletion.h"
#include "CodeEditorUtils.h"
#include "../../../xrECore/Editor/EditorContext.h"
#include "IconsFontAwesome7.h"
#include "imgui_internal.h"

static constexpr int MaxDepth = 32;
static constexpr u32 MaxItems = 64;

static bool IsKeyChar(char Char)
{
	return isalnum((u8)Char) || Char == '_' || Char == '$' || Char == '.';
}

static bool StartsWithNoCase(const xr_string& Text, const xr_string& Prefix)
{
	if (Prefix.size() > Text.size())
	{
		return false;
	}
	for (size_t Index = 0; Index < Prefix.size(); ++Index)
	{
		if (tolower((u8)Text[Index]) != tolower((u8)Prefix[Index]))
		{
			return false;
		}
	}
	return true;
}

static bool IsBoolType(const xr_string& Type)
{
	return Type == "bool" || Type == "BOOL";
}

bool CLtxCompletion::FindDefinition(const xr_string& Name, xr_string& Path, int& Line)
{
	auto Own = Local.find(Name);
	if (Own != Local.end() && !Own->second.Override)
	{
		Path = LocalPath;
		Line = Own->second.Line;
		return true;
	}
	return EContext.Visor->FindDefinition(Name, Path, Line);
}

void CLtxCompletion::UpdateMarkers(TextEditor& View, const xr_string& Path, const xr_string& Text)
{
	CXVisor& Visor = *EContext.Visor;
	if (!Visor.Load() || !Visor.IsSchemaLoaded())
	{
		return;
	}

	Local.clear();
	LocalLines.clear();
	LocalPath = Path;
	CXVisor::ParseLtx(Text, Local, &LocalLines);

	View.ClearErrorMarkers();
	for (const auto& [Name, Section] : Local)
	{
		xr_string Missing;
		for (const CXVisor::SLtxKey& Key : Visor.GetKeys(Name, &Local))
		{
			// The If keys depend on the values, they are not checked
			if (Key.Required && Key.If.empty() && !Visor.HasKey(Name, Key.Key, &Local))
			{
				Missing += (Missing.empty() ? "" : ", ") + Key.Key;
			}
		}
		if (!Missing.empty())
		{
			View.AddErrorMarker(Section.Line + 1, ("Missing required keys: " + Missing).c_str());
		}
	}
}

void CLtxCompletion::BeforeRender(TextEditor& View)
{
	Move = 0;
	AcceptRequest = false;
	CloseRequest = false;
	KeysTaken = false;

	// A click focuses the list before it is drawn
	const ImGuiWindow* Focused = GImGui->NavWindow;
	const bool ListFocused = Open && Focused != nullptr && Focused->RootWindow != nullptr && strstr(Focused->RootWindow->Name, "##ltx_completion") != nullptr;
	if (!ListFocused && !ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows))
	{
		Open = false;
		return;
	}
	if (ListFocused)
	{
		return;
	}
	EditorWindow = GImGui->NavWindow;

	if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Space))
	{
		ForceOpen = true;
		KeysTaken = true;
	}

	if (Open && !Items.empty())
	{
		if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
		{
			Move = 1;
		}
		else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
		{
			Move = -1;
		}
		else if (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter) || ImGui::IsKeyPressed(ImGuiKey_Tab))
		{
			AcceptRequest = true;
		}
		else if (ImGui::IsKeyPressed(ImGuiKey_Escape))
		{
			CloseRequest = true;
		}
		KeysTaken |= Move != 0 || AcceptRequest || CloseRequest;
	}

	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(false);
	}
}

void CLtxCompletion::Accept(TextEditor& View)
{
	if (Current < 0 || Current >= (int)Items.size())
	{
		return;
	}

	const TextEditor::Coordinates Cursor = View.GetCursorPosition();
	if (!Word.empty())
	{
		View.SetSelection({ Cursor.mLine, std::max(0, Cursor.mColumn - (int)Word.size()) }, Cursor);
	}
	View.ReplaceSelection((Items[Current].Name + (AppendAssign ? " = " : "")).c_str());
}

void CLtxCompletion::AfterRender(TextEditor& View, const xr_string& Path, const xr_string& Text)
{
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(true);
	}

	CXVisor& Visor = *EContext.Visor;
	const bool IsTokens = TextKind != EText::Ltx;
	if (!Visor.Load() || (!IsTokens && !Visor.IsSchemaLoaded()))
	{
		Open = false;
		return;
	}

	if (CloseRequest)
	{
		Open = false;
		return;
	}
	if (AcceptRequest)
	{
		Accept(View);
		Open = false;
		return;
	}

	if (!IsTokens && (View.IsTextChanged() || LocalPath != Path))
	{
		UpdateMarkers(View, Path, Text);
	}

	char Buffer[512];
	View.CopyTextBeforeCursor(Buffer, sizeof(Buffer));
	const xr_string Prefix = Buffer;
	const int CursorLine = View.GetCursorPosition().mLine;

	size_t WordStart = Prefix.size();
	while (WordStart > 0 && IsKeyChar(Prefix[WordStart - 1]))
	{
		--WordStart;
	}
	Word = Prefix.substr(WordStart).c_str();

	const bool InComment = Prefix.find(';') != xr_string::npos;
	const size_t Assign = Prefix.find('=');
	const bool IsValue = Assign != xr_string::npos;
	const bool InHeader = Prefix.Trim().starts_with("[");
	// A key starts the line: only spaces before it
	const bool IsKey = !IsTokens && !IsValue && xr_string(Prefix.substr(0, WordStart)).Trim().empty();

	// The functions of xr_conditions after =name and !name in {}, the ones of xr_effects after =name in %%
	const char Before = WordStart > 0 ? Prefix[WordStart - 1] : ' ';
	bool IsCondition = false;
	bool IsEffect = false;
	if (IsTokens)
	{
		IsCondition = TextKind == EText::Conditions && (Before == '=' || Before == '!');
		IsEffect = TextKind == EText::Effects && Before == '=';
	}
	else if (IsValue && WordStart > Assign + 1 && (Before == '=' || Before == '!'))
	{
		const xr_string Value = Prefix.substr(Assign + 1).c_str();
		const bool InPercents = std::count(Value.begin(), Value.end(), '%') % 2 == 1;
		const size_t BraceOpen = Value.find_last_of('{');
		const size_t BraceClose = Value.find_last_of('}');
		const bool InBraces = BraceOpen != xr_string::npos && (BraceClose == xr_string::npos || BraceClose < BraceOpen);
		IsCondition = InBraces && !InPercents;
		IsEffect = InPercents && Before == '=';
	}
	const bool IsFunction = IsCondition || IsEffect;

	if (View.IsTextChanged() || ForceOpen)
	{
		const bool Typed = !Prefix.empty() && (IsKeyChar(Prefix.back()) || (IsFunction && Prefix.back() == '!') || ((IsValue || IsFunction) && (Prefix.back() == '=' || Prefix.back() == ' ')));
		Open = (IsTokens ? IsFunction : !InComment && !InHeader && (IsKey || IsValue)) && (ForceOpen || Typed);
		Current = 0;
		Line = CursorLine;
	}
	ForceOpen = false;

	if (Open && (CursorLine != Line || (IsTokens ? !IsFunction : InComment || InHeader || !(IsKey || IsValue))))
	{
		Open = false;
	}

	if (Open)
	{
		const xr_string Section = CursorLine >= 0 && CursorLine < (int)LocalLines.size() ? LocalLines[CursorLine] : xr_string();
		xr_vector<SItem> All;
		AppendAssign = !IsValue && !IsTokens;

		if (IsFunction)
		{
			for (const xr_string& Name : IsCondition ? Visor.GetConditions() : Visor.GetEffects())
			{
				All.push_back({ Name, IsCondition ? "xr_conditions" : "xr_effects", false });
			}
		}
		else if (IsValue)
		{
			const xr_string Key = xr_string(Prefix.substr(0, Assign)).Trim();
			if (Key == "class")
			{
				for (const auto& [Id, Names] : Visor.GetClassIds())
				{
					xr_string Detail;
					for (const xr_string& Name : Names)
					{
						Detail += (Detail.empty() ? "" : ", ") + Name;
					}
					All.push_back({ Id, Detail, false });
				}
			}
			else
			{
				for (const CXVisor::SLtxKey& Field : Visor.GetKeys(Section, &Local))
				{
					if (Field.Key == Key && IsBoolType(Field.Type))
					{
						All.push_back({ "true", "bool", false });
						All.push_back({ "false", "bool", false });
						break;
					}
				}
			}
		}
		else if (!Section.empty())
		{
			auto Own = Local.find(Section);
			for (const CXVisor::SLtxKey& Field : Visor.GetKeys(Section, &Local))
			{
				// The keys of the section itself are written already, the inherited ones can be overridden
				if (Own != Local.end() && Own->second.Keys.contains(Field.Key))
				{
					continue;
				}
				All.push_back({ Field.Key, Field.Type + (Field.Required ? ", required" : "") + (Field.If.empty() ? "" : ", if " + Field.If) + " (" + Field.Schema + ")", true });
			}
		}

		Items.clear();
		for (SItem& Item : All)
		{
			if (StartsWithNoCase(Item.Name, Word) && Item.Name != Word)
			{
				Items.push_back(std::move(Item));
			}
		}
		std::sort(Items.begin(), Items.end(), [](const SItem& Left, const SItem& Right) { return Left.Name < Right.Name; });
		if (Items.size() > MaxItems)
		{
			Items.resize(MaxItems);
		}
		Open = !Items.empty();
		Current = std::clamp(Current + Move, 0, std::max(0, (int)Items.size() - 1));
	}

	if (!Open)
	{
		return;
	}

	const ImVec2 CursorPos = View.GetCursorScreenPos();
	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing
		| ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_Tooltip;

	static constexpr int MaxVisible = 14;
	const float IconWidth = ImGui::GetFrameHeight();
	float Width = 280.0f;
	for (const SItem& Item : Items)
	{
		Width = std::max(Width, IconWidth + ImGui::CalcTextSize(Item.Name.c_str()).x);
	}
	const ImGuiStyle& Style = ImGui::GetStyle();
	const int Rows = std::min((int)Items.size(), MaxVisible);
	const ImVec2 ListSize = { std::min(Width + Style.WindowPadding.x * 2.0f + Style.ScrollbarSize, 700.0f), Rows * ImGui::GetTextLineHeightWithSpacing() + Style.WindowPadding.y * 2.0f };
	bool Clicked = false;
	bool Insert = false;

	// A modal popup takes the clicks of the other windows: in a popup the list is its child
	const bool InPopup = GImGui->BeginPopupStack.Size > 0;
	const ImVec2 LayoutCursor = ImGui::GetCursorScreenPos();
	bool Drawn = false;
	if (InPopup)
	{
		ImGui::SetCursorScreenPos(CursorPos);
		ImGui::PushStyleColor(ImGuiCol_ChildBg, ImGui::GetStyleColorVec4(ImGuiCol_PopupBg));
		Drawn = ImGui::BeginChild("##ltx_completion", ListSize, ImGuiChildFlags_Borders, ImGuiWindowFlags_NoNav);
		ImGui::PopStyleColor();
	}
	else
	{
		ImGui::SetNextWindowPos(CursorPos);
		ImGui::SetNextWindowSize(ListSize);
		Drawn = ImGui::Begin("##ltx_completion", nullptr, Flags & ~ImGuiWindowFlags_NoInputs);
	}
	if (Drawn)
	{
		for (int Index = 0; Index < (int)Items.size(); ++Index)
		{
			const SItem& Item = Items[Index];
			const bool Selected = Index == Current;
			const ImVec2 Start = ImGui::GetCursorScreenPos();
			if (ImGui::Selectable(("##" + Item.Name).c_str(), Selected, ImGuiSelectableFlags_AllowDoubleClick))
			{
				// A click puts the hint into the text
				Current = Index;
				Clicked = true;
				Insert = true;
			}
			ImDrawList* DrawList = ImGui::GetWindowDrawList();
			DrawList->AddText({ Start.x + 2.0f, Start.y }, Item.IsKey ? IM_COL32(100, 180, 255, 255) : IM_COL32(255, 175, 80, 255), Item.IsKey ? ICON_FA_TAG : ICON_FA_HASHTAG);
			DrawList->AddText({ Start.x + IconWidth, Start.y }, ImGui::GetColorU32(ImGuiCol_Text), Item.Name.c_str());
			if (Selected && Move != 0)
			{
				ImGui::SetScrollHereY();
			}
		}
	}
	if (InPopup)
	{
		ImGui::EndChild();
		ImGui::SetCursorScreenPos(LayoutCursor);
	}
	else
	{
		ImGui::End();
	}

	// The text view keeps the focus: the list stays open and the typing goes on
	if (Clicked && EditorWindow != nullptr)
	{
		ImGui::FocusWindow(EditorWindow);
	}
	if (Insert)
	{
		Accept(View);
		Open = false;
		return;
	}

	if (!Items[Current].Detail.empty())
	{
		ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y - ImGui::GetTextLineHeightWithSpacing() * 2.0f });
		if (ImGui::Begin("##ltx_completion_detail", nullptr, Flags | ImGuiWindowFlags_AlwaysAutoResize))
		{
			ImGui::TextUnformatted(Items[Current].Detail.c_str());
		}
		ImGui::End();
	}
}

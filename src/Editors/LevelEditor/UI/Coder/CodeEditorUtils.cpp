#include "stdafx.h"
#include "CodeEditorUtils.h"
#include "IconsFontAwesome7.h"
#include "../../../xrEUI/xrUITheme.h"
#include "../../../xrEUI/ModernUI.h"

namespace CodeEditor
{
	static ImU32 WithAlpha(ImColor Color, float Alpha)
	{
		Color.Value.w = Alpha;
		return Color;
	}

	static TextEditor::Palette MakePalette(int ThemeID)
	{
		using XRay::ImGui::EEditorColors;
		using XRay::ImGui::GetEditorColor;
		using EIndex = TextEditor::PaletteIndex;

		// ABGR
		TextEditor::Palette Palette = { {
			0xffd4d4d4,	// Default
			0xffd69c56,	// Keyword
			0xffa8ceb5,	// Number
			0xff7891ce,	// String
			0xff7891ce,	// Char literal
			0xffb4b4b4,	// Punctuation
			0xffc086c5,	// Preprocessor
			0xfffedc9c,	// Identifier
			0xffb0c94e,	// Known identifier: API classes and namespaces
			0xffaadcdc,	// Preproc identifier: function calls
			0xff55996a,	// Comment
			0xff55996a,	// Multi line comment
			0xff1e1e1e,	// Background
			0xffaeafad,	// Cursor
			0x80784f26,	// Selection
			0x800020ff,	// Error marker
			0x40f08000,	// Breakpoint
			0xff858585,	// Line number
			0x40303030,	// Current line fill
			0x40202020,	// Current line fill, inactive
			0x40464646,	// Current line edge
		} };

		auto SetSyntax = [&Palette](std::initializer_list<ImU32> Colors)
		{
			std::copy(Colors.begin(), Colors.end(), Palette.begin());
		};

		switch (ThemeID)
		{
		case 4: // Everforest
			SetSyntax({ IM_COL32(211, 198, 170, 255), IM_COL32(230, 126, 128, 255), IM_COL32(214, 153, 182, 255), IM_COL32(167, 192, 128, 255),
				IM_COL32(167, 192, 128, 255), IM_COL32(157, 169, 160, 255), IM_COL32(230, 152, 117, 255), IM_COL32(211, 198, 170, 255),
				IM_COL32(219, 188, 127, 255), IM_COL32(131, 192, 146, 255), IM_COL32(133, 146, 137, 255), IM_COL32(133, 146, 137, 255) });
			break;
		case 5: // Catppuccin Mocha
			SetSyntax({ IM_COL32(205, 214, 244, 255), IM_COL32(203, 166, 247, 255), IM_COL32(250, 179, 135, 255), IM_COL32(166, 227, 161, 255),
				IM_COL32(166, 227, 161, 255), IM_COL32(147, 153, 178, 255), IM_COL32(245, 194, 231, 255), IM_COL32(205, 214, 244, 255),
				IM_COL32(249, 226, 175, 255), IM_COL32(137, 180, 250, 255), IM_COL32(108, 112, 134, 255), IM_COL32(108, 112, 134, 255) });
			break;
		case 7: // Tokyo Night
			SetSyntax({ IM_COL32(192, 202, 245, 255), IM_COL32(187, 154, 247, 255), IM_COL32(255, 158, 100, 255), IM_COL32(158, 206, 106, 255),
				IM_COL32(158, 206, 106, 255), IM_COL32(137, 221, 255, 255), IM_COL32(125, 207, 255, 255), IM_COL32(192, 202, 245, 255),
				IM_COL32(42, 195, 222, 255), IM_COL32(122, 162, 247, 255), IM_COL32(86, 95, 137, 255), IM_COL32(86, 95, 137, 255) });
			break;
		case 2: // Dark Orange: warm gruvbox-like syntax
		case 6: // Gruvbox
			SetSyntax({ IM_COL32(235, 219, 178, 255), IM_COL32(251, 73, 52, 255), IM_COL32(211, 134, 155, 255), IM_COL32(184, 187, 38, 255),
				IM_COL32(184, 187, 38, 255), IM_COL32(168, 153, 132, 255), IM_COL32(142, 192, 124, 255), IM_COL32(235, 219, 178, 255),
				IM_COL32(250, 189, 47, 255), IM_COL32(131, 165, 152, 255), IM_COL32(146, 131, 116, 255), IM_COL32(146, 131, 116, 255) });
			break;
		case 3: // Nordic: Nord syntax colors
			SetSyntax({ IM_COL32(216, 222, 233, 255), IM_COL32(129, 161, 193, 255), IM_COL32(180, 142, 173, 255), IM_COL32(163, 190, 140, 255),
				IM_COL32(163, 190, 140, 255), IM_COL32(236, 239, 244, 255), IM_COL32(94, 129, 172, 255), IM_COL32(216, 222, 233, 255),
				IM_COL32(143, 188, 187, 255), IM_COL32(136, 192, 208, 255), IM_COL32(97, 110, 136, 255), IM_COL32(97, 110, 136, 255) });
			break;
		}

		// Editor surface follows the theme
		const ImColor Accent = GetEditorColor(EEditorColors::Accent);
		Palette[(int)EIndex::Background] = GetEditorColor(EEditorColors::TableTint);
		Palette[(int)EIndex::Cursor] = Palette[(int)EIndex::Default];
		Palette[(int)EIndex::Selection] = WithAlpha(Accent, 0.4f);
		Palette[(int)EIndex::Breakpoint] = WithAlpha(Accent, 0.25f);
		Palette[(int)EIndex::LineNumber] = ImGui::GetColorU32(ImGuiCol_TextDisabled);
		Palette[(int)EIndex::CurrentLineFill] = WithAlpha(GetEditorColor(EEditorColors::PanelTint), 0.6f);
		Palette[(int)EIndex::CurrentLineFillInactive] = WithAlpha(GetEditorColor(EEditorColors::PanelTint), 0.3f);
		Palette[(int)EIndex::CurrentLineEdge] = WithAlpha(GetEditorColor(EEditorColors::PanelBorderTint), 0.8f);
		return Palette;
	}

	const TextEditor::Palette& GetPalette()
	{
		static TextEditor::Palette Palette;
		static int PaletteTheme = -1;

		const int ThemeID = CUIThemeManager::Get().ThemeID;
		if (PaletteTheme != ThemeID)
		{
			PaletteTheme = ThemeID;
			Palette = MakePalette(ThemeID);
		}
		return Palette;
	}

	void SyncPalette(TextEditor& View)
	{
		if (View.GetPalette() != GetPalette())
		{
			View.SetPalette(GetPalette());
		}
	}

	const TextEditor::LanguageDefinition& GetXmlLanguage()
	{
		static TextEditor::LanguageDefinition Language;
		static bool Initialized = false;
		if (Initialized)
		{
			return Language;
		}
		Initialized = true;

		Language.mName = "XML";
		Language.mCommentStart = "<!--";
		Language.mCommentEnd = "-->";
		Language.mSingleLineComment = "";
		Language.mPreprocChar = 0;
		Language.mAutoIndentation = true;
		Language.mCaseSensitive = true;

		using EIndex = TextEditor::PaletteIndex;
		Language.mTokenRegexStrings.emplace_back("<[?!]?/?[A-Za-z_][-A-Za-z0-9_:.]*", EIndex::Keyword);
		Language.mTokenRegexStrings.emplace_back("/?[?]?>", EIndex::Keyword);
		Language.mTokenRegexStrings.emplace_back("\"[^\"]*\"", EIndex::String);
		Language.mTokenRegexStrings.emplace_back("'[^']*'", EIndex::String);
		Language.mTokenRegexStrings.emplace_back("[A-Za-z_][-A-Za-z0-9_:.]*(?=[ \\t]*=)", EIndex::KnownIdentifier);
		Language.mTokenRegexStrings.emplace_back("[+-]?[0-9]+([.][0-9]*)?", EIndex::Number);
		Language.mTokenRegexStrings.emplace_back("[A-Za-z_][A-Za-z0-9_]*", EIndex::Identifier);
		Language.mTokenRegexStrings.emplace_back("[=&;]", EIndex::Punctuation);
		return Language;
	}

	const TextEditor::LanguageDefinition& GetIniLanguage()
	{
		static TextEditor::LanguageDefinition Language;
		static bool Initialized = false;
		if (Initialized)
		{
			return Language;
		}
		Initialized = true;

		Language.mName = "LTX";
		Language.mCommentStart = "";
		Language.mCommentEnd = "";
		Language.mSingleLineComment = ";";
		// #include "file.ltx"
		Language.mPreprocChar = '#';
		Language.mAutoIndentation = false;
		Language.mCaseSensitive = false;

		using EIndex = TextEditor::PaletteIndex;
		// [section] and [section]:parent
		Language.mTokenRegexStrings.emplace_back("\\[[^\\]]*\\]", EIndex::Keyword);
		Language.mTokenRegexStrings.emplace_back("\"[^\"]*\"", EIndex::String);
		Language.mTokenRegexStrings.emplace_back("[A-Za-z_$][-A-Za-z0-9_.$]*(?=[ \\t]*=)", EIndex::KnownIdentifier);
		Language.mTokenRegexStrings.emplace_back("[+-]?[0-9]+([.][0-9]*)?([eE][+-]?[0-9]+)?", EIndex::Number);
		Language.mTokenRegexStrings.emplace_back("[A-Za-z_$][-A-Za-z0-9_.$]*", EIndex::Identifier);
		Language.mTokenRegexStrings.emplace_back("[=:,!]", EIndex::Punctuation);
		return Language;
	}

	const TextEditor::LanguageDefinition& GetLogicLanguage()
	{
		static TextEditor::LanguageDefinition Language;
		static bool Initialized = false;
		if (Initialized)
		{
			return Language;
		}
		Initialized = true;

		Language.mName = "Logic";
		Language.mCommentStart = "";
		Language.mCommentEnd = "";
		Language.mSingleLineComment = ";";
		Language.mPreprocChar = 0;
		Language.mAutoIndentation = false;
		Language.mCaseSensitive = true;

		using EIndex = TextEditor::PaletteIndex;
		Language.mTokenRegexStrings.emplace_back("[=!][A-Za-z_][A-Za-z0-9_.]*", EIndex::KnownIdentifier);
		Language.mTokenRegexStrings.emplace_back("\\([^)]*\\)", EIndex::String);
		Language.mTokenRegexStrings.emplace_back("[+][A-Za-z_][A-Za-z0-9_.]*", EIndex::Keyword);
		Language.mTokenRegexStrings.emplace_back("[-][A-Za-z_][A-Za-z0-9_.]*", EIndex::Preprocessor);
		Language.mTokenRegexStrings.emplace_back("~[0-9]+", EIndex::Number);
		Language.mTokenRegexStrings.emplace_back("[A-Za-z_@][A-Za-z0-9_@.]*", EIndex::Identifier);
		Language.mTokenRegexStrings.emplace_back("[{}%|,]", EIndex::Punctuation);
		return Language;
	}

	xr_string GetText(const TextEditor& Editor)
	{
		xr_string Text;
		Editor.VisitText([](const char* Data, size_t Size, void* Out)
		{
			static_cast<xr_string*>(Out)->assign(Data, Size);
		}, &Text);
		std::erase(Text, '\r');
		return Text;
	}

	void SetText(TextEditor& Editor, const xr_string& Text)
	{
		const TextEditor::Coordinates Cursor = Editor.GetCursorPosition();
		Editor.SetText(Text.c_str());
		Editor.SetCursorPosition(Cursor);
	}
}

static bool ToggleButton(const char* Label, bool& Value, const char* Tooltip)
{
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(Value ? ImGuiCol_ButtonActive : ImGuiCol_Button));
	const bool Pressed = ImGui::Button(Label, { ImGui::GetFrameHeight() * 1.2f, 0.0f });
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("%s", Tooltip);
	if (Pressed)
	{
		Value = !Value;
	}
	return Pressed;
}

int CCodeSearch::ReplaceAll(TextEditor& View) const
{
	if (View.IsReadOnly())
	{
		return 0;
	}

	// A replacement may contain the searched text: never go over the matches that were there
	const int Count = View.CountHighlights();
	const TextEditor::Coordinates Start(0, 0);
	View.SetCursorPosition(Start);
	View.SetSelection(Start, Start);

	int Done = 0;
	for (; Done < Count && View.Find(Text, true, CaseSensitive, WholeWord); ++Done)
	{
		View.ReplaceSelection(ReplaceText);
	}
	return Done;
}

bool CCodeSearch::Draw(TextEditor& View, const xr_vector<TextEditor*>& Views, const ImVec2& Min, const ImVec2& Max)
{
	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows))
	{
		const bool Find = ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_F);
		const bool Replace = !Find && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_H);
		if (Find || Replace)
		{
			Visible = true;
			ReplaceVisible |= Replace;
			Focus = Find;
			FocusReplace = Replace;

			char Selected[sizeof(Text)] = {};
			if (View.CopySelectedLineText(Selected, sizeof(Selected)) > 0)
			{
				xr_strcpy(Text, Selected);
			}
		}
	}

	const char* Highlight = Visible ? Text : nullptr;
	for (TextEditor* Other : Views)
	{
		Other->SetHighlight(Highlight, CaseSensitive, WholeWord);
	}

	if (!Visible)
	{
		return false;
	}

	const ImGuiStyle& Style = ImGui::GetStyle();
	const float Button = ImGui::GetFrameHeight();
	const float Toggle = Button * 0.8f;
	const float Width = std::min(ImGui::GetFontSize() * 28.0f, Max.x - Min.x - Style.ScrollbarSize - 8.0f);
	if (Width <= Button * 6.0f)
	{
		return false;
	}

	const ImVec2 Cursor = ImGui::GetCursorScreenPos();
	ImGui::SetCursorScreenPos({ Max.x - Style.ScrollbarSize - Width - 4.0f, Min.y + 2.0f });
	ImGui::PushStyleColor(ImGuiCol_ChildBg, ImGui::GetStyleColorVec4(ImGuiCol_PopupBg));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, { 4.0f, 4.0f });
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, { 3.0f, 3.0f });
	ImGui::BeginChild("##code_search_box", { Width, 0.0f }, ImGuiChildFlags_Borders | ImGuiChildFlags_AutoResizeY | ImGuiChildFlags_AlwaysUseWindowPadding, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);

	if (ImGui::Button(ReplaceVisible ? ICON_FA_CHEVRON_DOWN "##search_mode" : ICON_FA_CHEVRON_RIGHT "##search_mode", { Toggle, 0.0f }))
	{
		ReplaceVisible = !ReplaceVisible;
		FocusReplace = ReplaceVisible;
		Focus = !ReplaceVisible;
	}
	ImGui::SetItemTooltip("Toggle replace (Ctrl+H)");

	const int Matches = View.CountHighlights();
	const int Current = View.GetSelectedHighlight();
	const bool Missed = Text[0] != 0 && Matches == 0;
	string32 MatchesText = {};
	if (Missed)
	{
		xr_strcpy(MatchesText, "No results");
	}
	else if (Text[0] != 0 && Current != 0)
	{
		xr_sprintf(MatchesText, "%d of %d", Current, Matches);
	}
	else if (Text[0] != 0)
	{
		xr_sprintf(MatchesText, "? of %d", Matches);
	}
	const float MatchesWidth = ImGui::CalcTextSize(MatchesText).x;

	ImGui::SameLine();
	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - MatchesWidth - Button * 3.0f - Style.ItemSpacing.x * (MatchesText[0] != 0 ? 4.0f : 3.0f));
	if (Focus)
	{
		ImGui::SetKeyboardFocusHere();
		Focus = false;
	}
	if (Missed)
	{
		ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.45f, 0.15f, 0.12f, 1.0f));
	}
	const bool Submitted = ImGui::InputTextWithHint("##code_search", "Find", Text, sizeof(Text), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll);
	if (Missed)
	{
		ImGui::PopStyleColor();
	}
	const bool Edited = ImGui::IsItemEdited();
	bool Close = ImGui::IsItemFocused() && ImGui::IsKeyPressed(ImGuiKey_Escape);

	if (MatchesText[0] != 0)
	{
		ImGui::SameLine();
		ImGui::AlignTextToFramePadding();
		ImGui::TextDisabled("%s", MatchesText);
	}

	bool Forward = true;
	bool Search = false;
	if (Edited && Text[0] != 0)
	{
		const TextEditor::Coordinates From = View.GetSelectionStart();
		View.SetCursorPosition(From);
		View.SetSelection(From, From);
		View.Find(Text, true, CaseSensitive, WholeWord);
	}
	if (Submitted)
	{
		Search = true;
		Forward = !ImGui::GetIO().KeyShift;
		// Enter takes the focus away: keep typing in the field
		Focus = true;
	}

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_ARROW_UP "##search_prev", { Button, 0.0f }))
	{
		Search = true;
		Forward = false;
	}
	ImGui::SetItemTooltip("Previous match (Shift+Enter)");

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_ARROW_DOWN "##search_next", { Button, 0.0f }))
	{
		Search = true;
	}
	ImGui::SetItemTooltip("Next match (Enter)");

	ImGui::SameLine();
	Close |= ImGui::Button(ICON_FA_XMARK "##search_close", { Button, 0.0f });
	ImGui::SetItemTooltip("Close (Escape)");

	bool ReplaceOne = false;
	bool ReplaceEvery = false;
	if (ReplaceVisible)
	{
		ImGui::Dummy({ Toggle, 0.0f });
		ImGui::SameLine();
		ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - Button * 2.0f - Style.ItemSpacing.x * 2.0f);
		if (FocusReplace)
		{
			ImGui::SetKeyboardFocusHere();
			FocusReplace = false;
		}
		if (ImGui::InputTextWithHint("##code_replace", "Replace", ReplaceText, sizeof(ReplaceText), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll))
		{
			ReplaceOne = true;
			FocusReplace = true;
		}
		Close |= ImGui::IsItemFocused() && ImGui::IsKeyPressed(ImGuiKey_Escape);

		ImGui::SameLine();
		ReplaceOne |= ImGui::Button(ICON_FA_RIGHT_LEFT "##replace_next", { Button, 0.0f });
		ImGui::SetItemTooltip("Replace next (Enter)");

		ImGui::SameLine();
		ReplaceEvery = ImGui::Button(ICON_FA_CHECK_DOUBLE "##replace_all", { Button, 0.0f });
		ImGui::SetItemTooltip("Replace all");
	}

	ImGui::Dummy({ Toggle, 0.0f });
	ImGui::SameLine();
	ToggleButton("Aa##search_case", CaseSensitive, "Match case");
	ImGui::SameLine();
	ToggleButton("ab##search_word", WholeWord, "Match whole word");
	{
		const ImVec2 From = ImGui::GetItemRectMin();
		const ImVec2 To = ImGui::GetItemRectMax();
		const float Y = To.y - Style.FramePadding.y + 1.0f;
		ImGui::GetWindowDrawList()->AddLine({ From.x + 5.0f, Y }, { To.x - 5.0f, Y }, ImGui::GetColorU32(ImGuiCol_Text));
	}

	ImGui::SameLine();
	const char* Scopes[] = { "Current document", "All open documents" };
	int ScopeIndex = static_cast<int>(Scope);
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (ImGui::Combo("##search_scope", &ScopeIndex, Scopes, IM_ARRAYSIZE(Scopes)))
	{
		Scope = static_cast<EScope>(ScopeIndex);
	}
	ImGui::SetItemTooltip("Where Replace all works");

	ImGui::EndChild();
	ImGui::PopStyleVar(2);
	ImGui::PopStyleColor();
	ImGui::SetCursorScreenPos(Cursor);

	if (Close)
	{
		Visible = false;
		Focus = false;
		FocusReplace = false;
		return false;
	}

	if (Text[0] == 0)
	{
		return false;
	}

	bool Changed = false;
	if (ReplaceEvery)
	{
		if (Scope == EScope::AllOpen)
		{
			for (TextEditor* Other : Views)
			{
				Changed |= ReplaceAll(*Other) != 0;
			}
		}
		else
		{
			Changed = ReplaceAll(View) != 0;
		}
	}
	else if (ReplaceOne)
	{
		if (View.GetSelectedHighlight() != 0)
		{
			View.ReplaceSelection(ReplaceText);
			Changed = true;
		}
		Search = true;
	}

	if (Search)
	{
		View.Find(Text, Forward, CaseSensitive, WholeWord);
	}
	return Changed;
}

#include "stdafx.h"
#include "CodeEditorUtils.h"
#include "IconsFontAwesome6.h"

namespace CodeEditor
{
	const TextEditor::Palette& GetPalette()
	{
		// ABGR
		static const TextEditor::Palette Palette = { {
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
		return Palette;
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

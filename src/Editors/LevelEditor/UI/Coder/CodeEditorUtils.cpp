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

void CCodeSearch::Draw(TextEditor& View, std::initializer_list<TextEditor*> Views)
{
	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_F))
	{
		Visible = true;
		Focus = true;
		Missed = false;
	}

	const char* Highlight = Visible ? Text : nullptr;
	for (TextEditor* Other : Views)
	{
		Other->SetHighlight(Highlight, CaseSensitive);
	}

	if (!Visible)
	{
		return;
	}

	if (Focus)
	{
		ImGui::SetKeyboardFocusHere();
		Focus = false;
	}

	const int Matches = View.CountHighlights();
	Missed = Text[0] != 0 && Matches == 0;

	string32 MatchesText;
	xr_sprintf(MatchesText, Text[0] != 0 ? "%d" : "", Matches);
	const float MatchesWidth = ImGui::CalcTextSize("0000").x;
	const float ButtonsWidth = MatchesWidth + ImGui::GetFrameHeight() * 4.0f + ImGui::GetStyle().ItemSpacing.x * 5.0f;
	ImGui::SetNextItemWidth(-ButtonsWidth);
	if (Missed)
	{
		ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.45f, 0.15f, 0.12f, 1.0f));
	}
	const bool Submitted = ImGui::InputTextWithHint("##code_search", ICON_FA_MAGNIFYING_GLASS " Find (Ctrl+F)", Text, sizeof(Text), ImGuiInputTextFlags_EnterReturnsTrue);
	if (Missed)
	{
		ImGui::PopStyleColor();
	}
	bool Close = ImGui::IsItemFocused() && ImGui::IsKeyPressed(ImGuiKey_Escape);

	ImGui::SameLine();
	ImGui::AlignTextToFramePadding();
	ImGui::TextDisabled("%s", MatchesText);
	ImGui::SameLine(0.0f, 0.0f);
	ImGui::Dummy({ std::max(0.0f, MatchesWidth - ImGui::CalcTextSize(MatchesText).x), 0.0f });
	ImGui::SetItemTooltip("Matches");

	bool Forward = true;
	bool Search = false;
	if (Submitted)
	{
		Search = true;
		Forward = !ImGui::GetIO().KeyShift;
		// Enter takes the focus away: keep typing in the field
		Focus = true;
	}

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_ARROW_UP "##search_prev", { ImGui::GetFrameHeight(), 0.0f }))
	{
		Search = true;
		Forward = false;
	}
	ImGui::SetItemTooltip("Previous match (Shift+Enter)");

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_ARROW_DOWN "##search_next", { ImGui::GetFrameHeight(), 0.0f }))
	{
		Search = true;
	}
	ImGui::SetItemTooltip("Next match (Enter)");

	ImGui::SameLine();
	ImGui::PushStyleColor(ImGuiCol_Button, CaseSensitive ? ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive) : ImGui::GetStyleColorVec4(ImGuiCol_Button));
	if (ImGui::Button("Aa##search_case", { ImGui::GetFrameHeight(), 0.0f }))
	{
		CaseSensitive = !CaseSensitive;
	}
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("Match case");

	ImGui::SameLine();
	Close |= ImGui::Button(ICON_FA_XMARK "##search_close", { ImGui::GetFrameHeight(), 0.0f });
	ImGui::SetItemTooltip("Close (Escape)");

	if (Close)
	{
		Visible = false;
		Focus = false;
		Missed = false;
		return;
	}

	if (Search && Text[0] != 0)
	{
		View.Find(Text, Forward, CaseSensitive);
	}
}

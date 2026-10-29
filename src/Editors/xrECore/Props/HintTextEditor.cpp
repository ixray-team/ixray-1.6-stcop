#include "stdafx.h"
#include "HintTextEditor.h"
#include "HintInput.h"


// The script calls of the properties: module.function, +info -info =function !function, (args), ~50, %effects%
static const TextEditor::LanguageDefinition& GetScriptLanguage()
{
	static TextEditor::LanguageDefinition Language;
	static bool Initialized = false;
	if (Initialized)
	{
		return Language;
	}
	Initialized = true;

	Language.mName = "Script";
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
	Language.mTokenRegexStrings.emplace_back("~?[0-9]+(\\.[0-9]+)?", EIndex::Number);
	// The module of module.function
	Language.mTokenRegexStrings.emplace_back("[A-Za-z_][A-Za-z0-9_]*\\.", EIndex::KnownIdentifier);
	Language.mTokenRegexStrings.emplace_back("[A-Za-z_@][A-Za-z0-9_@]*", EIndex::Identifier);
	Language.mTokenRegexStrings.emplace_back("[{}%|,:]", EIndex::Punctuation);
	return Language;
}

CHintTextEditor::CHintTextEditor()
{
	View.SetPalette(TextEditor::GetDarkPalette());
	View.SetLanguageDefinition(GetScriptLanguage());
	View.SetShowWhitespaces(false);
}

void CHintTextEditor::SetText(const xr_string& Text)
{
	View.SetText(Text.c_str());
	Items.clear();
	Forced = false;
}

xr_string CHintTextEditor::GetText() const
{
	// A std::string of imgui.dll is not freed here: another allocator. The text is copied in the visitor
	xr_string Text;
	View.VisitText([](const char* Data, size_t Size, void* Out)
	{
		static_cast<xr_string*>(Out)->assign(Data, Size);
	}, &Text);
	std::erase(Text, '\r');
	// The view ends the text with a new line
	while (!Text.empty() && Text.back() == '\n')
	{
		Text.pop_back();
	}
	return Text;
}

void CHintTextEditor::Accept()
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
	View.ReplaceSelection(Items[Current].c_str());
	Items.clear();
	Forced = false;
}

bool CHintTextEditor::Render(const char* Label, const ImVec2& Size, const TOnHintEvent& Hints)
{
	// The keys of the list are not given to the text
	int Move = 0;
	bool AcceptRequest = false;
	bool CloseRequest = false;
	const bool Focused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	if (Focused && !Hints.empty())
	{
		if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Space))
		{
			Forced = true;
		}
		if (!Items.empty())
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
		}
	}
	const bool KeysTaken = Move != 0 || AcceptRequest || CloseRequest || Forced;
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(false);
	}
	View.Render(Label, Size, true);
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(true);
	}

	bool Changed = View.IsTextChanged();
	if (Hints.empty() || !Focused)
	{
		Items.clear();
		return Changed;
	}
	if (CloseRequest)
	{
		Items.clear();
		return Changed;
	}
	if (AcceptRequest)
	{
		Accept();
		return true;
	}

	char Buffer[1024];
	View.CopyTextBeforeCursor(Buffer, sizeof(Buffer));
	const xr_string Before = Buffer;
	size_t WordStart = Before.size();
	while (WordStart > 0 && HintInput::IsHintChar(Before[WordStart - 1]))
	{
		--WordStart;
	}
	Word = Before.substr(WordStart).c_str();

	// The list follows the typing: it is made again when the text or the word changes
	if (Changed || Forced || !Items.empty())
	{
		xr_vector<xr_string> All;
		if (!Word.empty() || Forced)
		{
			Hints(Before, All);
		}
		Items.clear();
		for (xr_string& Item : All)
		{
			if (HintInput::StartsWithNoCase(Item, Word) && Item != Word)
			{
				Items.push_back(std::move(Item));
			}
		}
		std::sort(Items.begin(), Items.end());
		Items.erase(std::unique(Items.begin(), Items.end()), Items.end());
		if (Items.size() > HintInput::MaxHints)
		{
			Items.resize(HintInput::MaxHints);
		}
		if (Changed || Forced)
		{
			Current = 0;
		}
	}
	Forced = false;
	if (Items.empty())
	{
		return Changed;
	}
	Current = std::clamp(Current + Move, 0, (int)Items.size() - 1);

	// A tooltip window does not take the focus: the popup of the editor stays open
	const ImGuiStyle& Style = ImGui::GetStyle();
	const int Rows = std::min((int)Items.size(), HintInput::MaxVisible);
	float Width = 220.0f;
	for (const xr_string& Item : Items)
	{
		Width = std::max(Width, ImGui::CalcTextSize(Item.c_str()).x + Style.ScrollbarSize);
	}
	const ImVec2 CursorPos = View.GetCursorScreenPos();
	ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y + ImGui::GetTextLineHeightWithSpacing() });
	ImGui::SetNextWindowSize({ std::min(Width + Style.WindowPadding.x * 2.0f, 600.0f), Rows * ImGui::GetTextLineHeightWithSpacing() + Style.WindowPadding.y * 2.0f });
	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_Tooltip;
	if (ImGui::Begin("##hint_text_list", nullptr, Flags))
	{
		for (int Index = 0; Index < (int)Items.size(); ++Index)
		{
			const bool Selected = Index == Current;
			ImGui::Selectable(Items[Index].c_str(), Selected);
			if (Selected && Move != 0)
			{
				ImGui::SetScrollHereY();
			}
		}
	}
	ImGui::End();
	return Changed;
}

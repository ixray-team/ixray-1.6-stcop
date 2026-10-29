#pragma once
#include "plugins/TextEditor.h"

namespace CodeEditor
{
	const TextEditor::Palette& GetPalette();
	const TextEditor::LanguageDefinition& GetXmlLanguage();
	const TextEditor::LanguageDefinition& GetIniLanguage();

	// Condlists of the logic: {+info -info =function(a:b) !function ~50} %=effect +info -info%
	const TextEditor::LanguageDefinition& GetLogicLanguage();

	xr_string GetText(const TextEditor& Editor);
	void SetText(TextEditor& Editor, const xr_string& Text);
}

class CCodeSearch
{
public:
	enum class EScope
	{
		Current,
		AllOpen
	};

	bool Draw(TextEditor& View, const xr_vector<TextEditor*>& Views, const ImVec2& Min, const ImVec2& Max);

private:
	int ReplaceAll(TextEditor& View) const;

	bool Visible = false;
	bool ReplaceVisible = false;
	char Text[128] = {};
	char ReplaceText[128] = {};
	bool CaseSensitive = false;
	bool WholeWord = false;
	EScope Scope = EScope::Current;
	bool Focus = false;
	bool FocusReplace = false;
};

#pragma once
#include "plugins/TextEditor.h"

namespace CodeEditor
{
	// Visual Studio Code Dark+ colors
	const TextEditor::Palette& GetPalette();
	const TextEditor::LanguageDefinition& GetXmlLanguage();
	// ltx and ini: [sections], key = value, ; comments
	const TextEditor::LanguageDefinition& GetIniLanguage();

	// imgui.dll allocates GetText() with its own heap: the copy is taken inside the dll
	xr_string GetText(const TextEditor& Editor);
	void SetText(TextEditor& Editor, const xr_string& Text);
}

// Ctrl+F opens it, Enter and Shift+Enter go to the next and previous match, Escape closes it
class CCodeSearch
{
public:
	// Views keep the marks of the matches over a tab switch
	void Draw(TextEditor& View, std::initializer_list<TextEditor*> Views);

private:
	bool Visible = false;
	char Text[128] = {};
	bool CaseSensitive = false;
	bool Missed = false;
	bool Focus = false;
};

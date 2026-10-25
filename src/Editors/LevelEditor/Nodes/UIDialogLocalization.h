#pragma once

#include "../../xrEUI/EditorWnd.h"

// Registers a dialog phrase text in the string tables of all languages
class CUIDialogLocalization final : public IEditorWnd
{
public:
	CUIDialogLocalization();

	// NodeName is the phrase that receives StringId once the text is registered
	void Open(const char* StringId, const char* NodeName, const char* FileName);
	void Draw() override;

private:
	struct LanguageText
	{
		xr_string Language;
		bool IsCurrent = false;
		char Text[2048] = {};
	};

	bool Register();
	bool CanRegister(xr_string& Reason) const;
	bool AppendString(const char* Language, const char* Utf8Text);

	static bool IsValidId(const char* Id);
	static xr_string EscapeText(const char* Text);

	char StringId[256] = {};
	char FileName[256] = {};
	xr_string NodeName;
	xr_vector<LanguageText> Texts;
	xr_string Error;
	bool FocusText = false;
};

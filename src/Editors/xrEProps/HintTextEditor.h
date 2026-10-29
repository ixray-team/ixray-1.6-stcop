#pragma once
#include "plugins/TextEditor.h"

// A multiline text view with the highlight of script calls and condlists, and the hints of TOnHintEvent
// for the word at the cursor. Up and Down select a hint, Tab or Enter accepts it, Escape closes the list,
// Ctrl+Space shows the list with no word typed
class XREPROPS_API CHintTextEditor
{
public:
	CHintTextEditor();

	void SetText(const xr_string& Text);
	xr_string GetText() const;
	bool IsHintsOpen() const { return !Items.empty(); }

	// Returns true when the text changed
	bool Render(const char* Label, const ImVec2& Size, const TOnHintEvent& Hints);

private:
	void Accept();

	TextEditor View;
	xr_vector<xr_string> Items;
	xr_string Word;
	int Current = 0;
	bool Forced = false;
};

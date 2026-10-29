#pragma once

// A text input with the hints of TOnHintEvent for the word at the cursor.
// Tab accepts the selected hint, Up and Down select it in a single line input, Ctrl+Space shows the list with no word typed
namespace HintInput
{
	// Size.y > 0 makes a multiline input. Returns true when the text changed
	XREPROPS_API bool InputText(const char* Label, char* Buffer, size_t BufferSize, const TOnHintEvent& Hints, const ImVec2& Size = { 0.0f, 0.0f }, ImGuiInputTextFlags Flags = 0);
}

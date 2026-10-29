#pragma once

// A text input with the hints of TOnHintEvent for the word at the cursor.
// Tab accepts the selected hint, Up and Down select it in a single line input, Ctrl+Space shows the list with no word typed
namespace HintInput
{
	constexpr u32 MaxHints = 64;
	constexpr int MaxVisible = 12;

	inline bool IsHintChar(char Char)
	{
		return isalnum((u8)Char) || Char == '_' || Char == '.';
	}

	inline bool StartsWithNoCase(const xr_string& Text, const xr_string& Prefix)
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

	// Size.y > 0 makes a multiline input. Returns true when the text changed
	ECORE_API bool InputText(const char* Label, char* Buffer, size_t BufferSize, const TOnHintEvent& Hints, const ImVec2& Size = { 0.0f, 0.0f }, ImGuiInputTextFlags Flags = 0);
}

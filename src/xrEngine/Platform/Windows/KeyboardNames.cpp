#include "stdafx.h"

#ifdef IXR_WINDOWS
#include "KeyboardNames.h"

bool Platform::GetKeyboardDisplayName(SDL_Scancode Scancode, char* Destination, int Capacity)
{
	struct ScanCodePair
	{
		SDL_Scancode Scancode;
		LONG WindowsCode;
	};

	// Windows scan codes with the extended-key flag in bit 8.
	static constexpr ScanCodePair NamedKeys[] = {
		{ SDL_SCANCODE_ESCAPE, 0x001 },
		{ SDL_SCANCODE_BACKSPACE, 0x00E },
		{ SDL_SCANCODE_TAB, 0x00F },
		{ SDL_SCANCODE_RETURN, 0x01C },
		{ SDL_SCANCODE_LCTRL, 0x01D },
		{ SDL_SCANCODE_LSHIFT, 0x02A },
		{ SDL_SCANCODE_RSHIFT, 0x036 },
		{ SDL_SCANCODE_KP_MULTIPLY, 0x037 },
		{ SDL_SCANCODE_LALT, 0x038 },
		{ SDL_SCANCODE_SPACE, 0x039 },
		{ SDL_SCANCODE_CAPSLOCK, 0x03A },
		{ SDL_SCANCODE_PAUSE, 0x045 },
		{ SDL_SCANCODE_SCROLLLOCK, 0x046 },
		{ SDL_SCANCODE_KP_7, 0x047 },
		{ SDL_SCANCODE_KP_8, 0x048 },
		{ SDL_SCANCODE_KP_9, 0x049 },
		{ SDL_SCANCODE_KP_MINUS, 0x04A },
		{ SDL_SCANCODE_KP_4, 0x04B },
		{ SDL_SCANCODE_KP_5, 0x04C },
		{ SDL_SCANCODE_KP_6, 0x04D },
		{ SDL_SCANCODE_KP_PLUS, 0x04E },
		{ SDL_SCANCODE_KP_1, 0x04F },
		{ SDL_SCANCODE_KP_2, 0x050 },
		{ SDL_SCANCODE_KP_3, 0x051 },
		{ SDL_SCANCODE_KP_0, 0x052 },
		{ SDL_SCANCODE_KP_PERIOD, 0x053 },
		{ SDL_SCANCODE_KP_ENTER, 0x11C },
		{ SDL_SCANCODE_RCTRL, 0x11D },
		{ SDL_SCANCODE_KP_DIVIDE, 0x135 },
		{ SDL_SCANCODE_PRINTSCREEN, 0x137 },
		{ SDL_SCANCODE_RALT, 0x138 },
		{ SDL_SCANCODE_NUMLOCKCLEAR, 0x145 },
		{ SDL_SCANCODE_HOME, 0x147 },
		{ SDL_SCANCODE_UP, 0x148 },
		{ SDL_SCANCODE_PAGEUP, 0x149 },
		{ SDL_SCANCODE_LEFT, 0x14B },
		{ SDL_SCANCODE_RIGHT, 0x14D },
		{ SDL_SCANCODE_END, 0x14F },
		{ SDL_SCANCODE_DOWN, 0x150 },
		{ SDL_SCANCODE_PAGEDOWN, 0x151 },
		{ SDL_SCANCODE_INSERT, 0x152 },
		{ SDL_SCANCODE_DELETE, 0x153 },
		{ SDL_SCANCODE_LGUI, 0x15B },
		{ SDL_SCANCODE_RGUI, 0x15C },
		{ SDL_SCANCODE_APPLICATION, 0x15D },
	};

	for (const ScanCodePair& Key : NamedKeys)
	{
		if (Key.Scancode != Scancode)
		{
			continue;
		}

		wchar_t Name[64];
		if (GetKeyNameTextW(Key.WindowsCode << 16, Name, _countof(Name)) == 0)
		{
			return false;
		}
		return WideCharToMultiByte(CP_UTF8, 0, Name, -1, Destination, Capacity, nullptr, nullptr) > 0;
	}

	return false;
}
#endif

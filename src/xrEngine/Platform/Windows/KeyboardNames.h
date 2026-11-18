#pragma once

#include <SDL3/SDL_scancode.h>

namespace Platform
{
	bool GetKeyboardDisplayName(SDL_Scancode Scancode, char* Destination, int Capacity);
}

#include "RHIWindow.h"
#include <SDL3/SDL.h>

void* RHIPlatform::GetWindowHandle(SDL_Window* Window)
{
#ifdef __APPLE__
	// DXMT's native view hooks receive the SDL window.
	return Window;
#else
	return SDL_GetPointerProperty(SDL_GetWindowProperties(Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
#endif
}


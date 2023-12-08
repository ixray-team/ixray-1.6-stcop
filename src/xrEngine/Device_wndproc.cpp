#include "stdafx.h"
#include "xr_input.h"
#include "imgui_impl_sdl3.h"

bool CRenderDevice::on_event	(SDL_Event& Event)
{
	ImGui_ImplSDL3_ProcessEvent(&Event);
	switch (Event.type) {
	case SDL_EVENT_WINDOW_MOUSE_ENTER:
		OnWM_Activate(true, false);
		break;
	case SDL_EVENT_WINDOW_MOUSE_LEAVE:
		OnWM_Activate(false, false);
		break;
	case SDL_EVENT_WINDOW_SHOWN:
		OnWM_Activate(true, false);
		break;
	case SDL_EVENT_WINDOW_HIDDEN:
		OnWM_Activate(false, true);
		break;
	case SDL_EVENT_QUIT:
		Profile::BeginFrame("Destroy");
		g_pEventManager->Event.Signal("KERNEL:disconnect");
		g_pEventManager->Event.Signal("KERNEL:quit");
		Profile::EndFrame();
		return false;
	case SDL_EVENT_KEY_DOWN:
		pInput->KeyPressed(Event.key.keysym.sym);
		break;
	case SDL_EVENT_KEY_UP:
		pInput->KeyReleased(Event.key.keysym.sym);
		break;
	case SDL_EVENT_MOUSE_MOTION:
		pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
		break;
	case SDL_EVENT_MOUSE_WHEEL:
		pInput->MouseScroll(Event.wheel.y);
		break;
	case SDL_EVENT_MOUSE_BUTTON_DOWN:	
	case SDL_EVENT_MOUSE_BUTTON_UP:
	{
		int mouse_button = 0;
		if (Event.button.button == SDL_BUTTON_LEFT) { mouse_button = 0; }
		if (Event.button.button == SDL_BUTTON_RIGHT) { mouse_button = 1; }
		if (Event.button.button == SDL_BUTTON_MIDDLE) { mouse_button = 2; }
		if (Event.type == SDL_EVENT_MOUSE_BUTTON_DOWN) {
			pInput->MousePressed(mouse_button);
		} else {
			pInput->MouseReleased(mouse_button);
		}
	}
		break;
	}

	return true;
}

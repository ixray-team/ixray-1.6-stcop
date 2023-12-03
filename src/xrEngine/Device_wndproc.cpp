#include "stdafx.h"
#include "xr_input.h"

bool CRenderDevice::on_event	(SDL_Event& Event)
{
	switch (Event.type) {
	case SDL_EVENT_WINDOW_SHOWN:
		break;
	case SDL_EVENT_QUIT:
		g_pEventManager->Event.Defer("KERNEL:disconnect");
		g_pEventManager->Event.Defer("KERNEL:quit");
		break;
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

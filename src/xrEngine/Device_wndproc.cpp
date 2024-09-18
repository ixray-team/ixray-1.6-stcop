#include "stdafx.h"
#include "xr_input.h"
#include "imgui_impl_sdl2.h"

bool CRenderDevice::on_event(SDL_Event& Event)
{
	ImGui_ImplSDL2_ProcessEvent(&Event);
	
	if (SDL_GetWindowID(g_AppInfo.Window) != Event.window.windowID)
		return true;

	switch (Event.type) {
	case SDL_WINDOWEVENT_ENTER:
		OnWM_Activate(true, false);
		break;
	case SDL_WINDOWEVENT_LEAVE:
		OnWM_Activate(false, false);
		break;
	case SDL_WINDOWEVENT_SHOWN:
		OnWM_Activate(true, false);
		break;
	case SDL_WINDOWEVENT_HIDDEN:
		OnWM_Activate(false, true);
		break;
	case SDL_QUIT:
	case SDL_WINDOWEVENT_CLOSE:
		g_pEventManager->Event.Signal("KERNEL:disconnect");
		g_pEventManager->Event.Signal("KERNEL:quit");
		return false;
	case SDL_JOYDEVICEREMOVED:
		SDL_GameControllerClose(pInput->pGamePad);
		pInput->pGamePad = nullptr;
		break;
	case SDL_JOYDEVICEADDED:
		if (SDL_IsGameController(Event.jdevice.which))
			pInput->pGamePad = SDL_GameControllerOpen(Event.jdevice.which);
		break;
	case SDL_CONTROLLERBUTTONDOWN:
		pInput->GamepadButtonUpdate(Event.cbutton.button, true);
		break;
	case SDL_CONTROLLERBUTTONUP:
	{
		pInput->GamepadButtonUpdate(Event.cbutton.button, false);
		break;
	}
	case SDL_CONTROLLERAXISMOTION:
	{
		float Value = std::clamp((float)Event.caxis.value / 32767.0f, -1.0f, 1.0f);

		if ((Value > 0 && Value < 0.1f) || (Value < 0 && Value > -0.1f))
			Value = 0;

		if (Event.caxis.axis < 2)
		{
			pInput->LeftAxisUpdate(Event.caxis.axis == SDL_CONTROLLER_AXIS_LEFTX, Value);
		}
		else if (Event.caxis.axis < 4)
		{
			pInput->RightAxisUpdate(Event.caxis.axis == SDL_CONTROLLER_AXIS_RIGHTX, Value);
		}
		else
		{
			// L2 & R2 Triggers
			pInput->AdaptiveTriggerUpdate(Event.caxis.axis == SDL_CONTROLLER_AXIS_TRIGGERLEFT, Value);
		}

		break;
	}
	/*
	case SDL_CONTROLLERAXIS_LEFTY:  // This isn't necessary if SDL_CONTROLLERAXISMOTION is handled.
		break;
		*/
	case SDL_KEYDOWN:
		pInput->KeyboardButtonUpdate(Event.key.keysym.scancode, true);
		break;
	case SDL_KEYUP:
		pInput->KeyboardButtonUpdate(Event.key.keysym.scancode, false);
		break;
	case SDL_MOUSEMOTION:
		pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
		break;
	case SDL_MOUSEWHEEL:
		pInput->MouseScroll(Event.wheel.y);
		break;
	case SDL_MOUSEBUTTONDOWN:
	case SDL_MOUSEBUTTONUP:
	{
		int mouse_button = 0;
		if (Event.button.button == SDL_BUTTON_LEFT) { mouse_button = 0; }
		if (Event.button.button == SDL_BUTTON_RIGHT) { mouse_button = 1; }
		if (Event.button.button == SDL_BUTTON_MIDDLE) { mouse_button = 2; }
		if (Event.button.button == SDL_BUTTON_X1) { mouse_button = 3; }
		if (Event.button.button == SDL_BUTTON_X2) { mouse_button = 4; }
		if (Event.type == SDL_MOUSEBUTTONDOWN) {
			pInput->MousePressed(mouse_button);
		}
		else {
			pInput->MouseReleased(mouse_button);
		}
	}
	break;
	}

	return true;
}
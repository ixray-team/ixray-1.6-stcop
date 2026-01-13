#include "stdafx.h"
#include "xr_input.h"
#include "imgui_impl_sdl3.h"

bool CRenderDevice::on_event	(SDL_Event& Event)
{
	PROF_EVENT("CRenderDevice::on_event");
	ImGui_ImplSDL3_ProcessEvent(&Event);
	
	// this is needed because default event handler that goes after if (SDL_GetWindowID(g_AppInfo.Window) != Event.window.windowID) is not reachable
	switch (Event.type)
	{
		case SDL_EVENT_GAMEPAD_REMOVED:
		{
			SDL_CloseGamepad(pInput->pGamePad);
			pInput->pGamePad = nullptr;
			
			break;
		}
		case SDL_EVENT_GAMEPAD_ADDED:
		{
			if (SDL_IsGamepad(Event.jdevice.which))
			{
				pInput->pGamePad = SDL_OpenGamepad(Event.jdevice.which);

				if (pInput->receive_gamepad_addedorremoved)
				{
					pInput->receive_gamepad_addedorremoved(reinterpret_cast<void*>(Event.jdevice.which), true);
				}
			}
			break;
		}
		case SDL_EVENT_GAMEPAD_BUTTON_DOWN:
		{
			pInput->GamepadButtonUpdate(Event.gbutton.button, true);
			pInput->SetControllerMode(true);
			break;
		}
		case SDL_EVENT_GAMEPAD_BUTTON_UP:
		{
			pInput->GamepadButtonUpdate(Event.gbutton.button, false);
			break;
		}
		case SDL_EVENT_GAMEPAD_AXIS_MOTION:
		{
			float Value = std::clamp((float)Event.gaxis.value / 32767.0f, -1.0f, 1.0f);

			if ((Value > 0 && Value < 0.1f) || (Value < 0 && Value > -0.1f))
				Value = 0;

			if (Event.gaxis.axis < 2)
			{
				pInput->LeftAxisUpdate(Event.gaxis.axis == 0, Value);
			}
			else if (Event.gaxis.axis < 4)
			{
				pInput->RightAxisUpdate(Event.gaxis.axis == 2, Value);
			}
			else
			{
				// L2 & R2 Triggers
				pInput->AdaptiveTriggerUpdate(Event.gaxis.axis == 4, Value);
			}
			pInput->SetControllerMode(true);

			break;
		}
		case SDL_GAMEPAD_AXIS_LEFTY:
		{
			break;
		}
		case SDL_EVENT_KEYBOARD_ADDED:
		{
			if (pInput->receive_keyboard_addedorremoved)
			{
				pInput->receive_keyboard_addedorremoved(reinterpret_cast<void*>(Event.kdevice.which), true);
			}

			break;
		}
		case SDL_EVENT_KEYBOARD_REMOVED:
		{
			if (pInput->receive_keyboard_addedorremoved)
			{
				pInput->receive_keyboard_addedorremoved(nullptr, false);
			}

			break;
		}
		case SDL_EVENT_MOUSE_ADDED:
		{
			if (pInput->receive_mouse_addedorremoved)
			{
				pInput->receive_mouse_addedorremoved(reinterpret_cast<void*>(Event.mdevice.which), true);
			}

			break;
		}
		case SDL_EVENT_MOUSE_REMOVED:
		{
			if (pInput->receive_mouse_addedorremoved)
			{
				pInput->receive_mouse_addedorremoved(nullptr, false);
			}

			break;
		}
	}

	if (SDL_GetWindowID(g_AppInfo.Window) != Event.window.windowID)
	{
		return true;
	}

	switch (Event.type)
	{
		case SDL_EVENT_WINDOW_MOUSE_ENTER:
		{	OnWM_Activate(true, false);
			break;
		}
		case SDL_EVENT_WINDOW_MOUSE_LEAVE:
		{
			OnWM_Activate(false, false);
			break;
		}
		case SDL_EVENT_WINDOW_SHOWN:
		{
			OnWM_Activate(true, false);
			break;
		}
		case SDL_EVENT_WINDOW_HIDDEN:
		{
			OnWM_Activate(false, true);
			break;
		}
		case SDL_EVENT_QUIT:
		case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
		{
			g_pEventManager->Event.Signal("KERNEL:disconnect");
			g_pEventManager->Event.Signal("KERNEL:quit");
			return false;
		}
		case SDL_EVENT_KEY_DOWN:
		{
			pInput->KeyboardButtonUpdate(Event.key.scancode, true);

#ifdef DEBUG_DRAW
			if (pInput)
			{
				if (pInput->xrgame_sdk_input_pressed)
				{
					pInput->xrgame_sdk_input_pressed((int)Event.key.scancode);
				}
			}
#endif

			pInput->SetControllerMode(false);
			break;
		}
		case SDL_EVENT_KEY_UP:
		{
			pInput->KeyboardButtonUpdate(Event.key.scancode, false);

#ifdef DEBUG_DRAW
			if (pInput)
			{
				if (pInput->xrgame_sdk_input_released)
				{
					pInput->xrgame_sdk_input_released((int)Event.key.scancode);
				}
			}
#endif

			break;
		}
		case SDL_EVENT_MOUSE_MOTION:
		{
			pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
			pInput->SetControllerMode(false);
			break;
		}
		case SDL_EVENT_MOUSE_WHEEL:
		{
			pInput->MouseScroll(Event.wheel.y);
			pInput->SetControllerMode(false);
			break;
		}
		case SDL_EVENT_MOUSE_BUTTON_DOWN:
		case SDL_EVENT_MOUSE_BUTTON_UP:
		{
			int mouse_button = 0;
			if (Event.button.button == SDL_BUTTON_LEFT) { mouse_button = 0; }
			if (Event.button.button == SDL_BUTTON_RIGHT) { mouse_button = 1; }
			if (Event.button.button == SDL_BUTTON_MIDDLE) { mouse_button = 2; }
			if (Event.button.button == SDL_BUTTON_X1) { mouse_button = 3; }
			if (Event.button.button == SDL_BUTTON_X2) { mouse_button = 4; }

			if (Event.type == SDL_EVENT_MOUSE_BUTTON_DOWN)
			{
				pInput->MousePressed(mouse_button);
				pInput->SetControllerMode(false);
			}
			else
			{
				pInput->MouseReleased(mouse_button);
			}
			break;
		}
	}

	return true;
}

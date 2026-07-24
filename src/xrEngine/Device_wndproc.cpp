#include "stdafx.h"
#include "xr_input.h"
#include "GamepadService.h"
#include "imgui_impl_sdl3.h"
#include "XR_IOConsole.h"
#include "IInputReceiver.h"

#define DEADZONE_SIZE 0.2f
bool CRenderDevice::on_event	(SDL_Event& Event)
{
	PROF_EVENT("CRenderDevice::on_event");

#ifdef IXR_WINDOWS
	ImGui_ImplSDL3_ProcessEvent(&Event);
#endif

	// this is needed because default event handler that goes after if (SDL_GetWindowID(g_AppInfo.Window) != Event.window.windowID) is not reachable
	switch (Event.type)
	{
		case SDL_EVENT_GAMEPAD_REMOVED:
		{
			SDL_CloseGamepad(GGamepadService->GamePadDevice);
			GGamepadService->GamePadDevice = nullptr;
			
			break;
		}
		case SDL_EVENT_GAMEPAD_ADDED:
		{
			if (SDL_IsGamepad(Event.jdevice.which))
			{
				GGamepadService->GamePadDevice = SDL_OpenGamepad(Event.jdevice.which);
				if (SDL_GamepadHasSensor(GGamepadService->GamePadDevice, SDL_SENSOR_GYRO))
				{
					SDL_SetGamepadSensorEnabled(GGamepadService->GamePadDevice, SDL_SENSOR_GYRO, true);
					Msg(make_string<const char*>("! Detected gyroscope for controller %s", SDL_GetGamepadName(GGamepadService->GamePadDevice)));
				}

				pInput->SelectGamepadPrefix();

				if (pInput->receive_gamepad_addedorremoved)
				{
					pInput->receive_gamepad_addedorremoved((Event.jdevice.which), true);
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

			if ((Value > 0 && Value < DEADZONE_SIZE) || (Value < 0 && Value > -DEADZONE_SIZE))
				Value = 0;

			float ValueReal = 0.0f;
			if (!fis_zero(Value))
			{
				ValueReal = (Value > 0.f ? Value - 0.2f : Value + 0.2f) / 0.8f;
			}
			bool zeroVal = Value == 0.0f;
			if (Event.gaxis.axis < 2)
			{
				bool isX = Event.gaxis.axis == 0;
				pInput->LeftAxisUpdate(isX, ValueReal);
			}
			else if (Event.gaxis.axis < 4)
			{
				bool isX = Event.gaxis.axis == 2;
				pInput->RightAxisUpdate(isX, ValueReal);
			}
			else
			{
				bool isX = Event.gaxis.axis == 4;
				int dik = DIK_RTRIGGER;
				if (isX)
				{
					dik = DIK_LTRIGGER;
				}
				pInput->GamepadButtonUpdate(dik, !zeroVal);
				// L2 & R2 Triggers
				pInput->AdaptiveTriggerUpdate(isX, ValueReal);
			}
			if (!zeroVal)
			{
				pInput->SetControllerMode(true);
			}
			break;
		}
		case SDL_EVENT_GAMEPAD_SENSOR_UPDATE:
		{
			if (!GGamepadService || !GGamepadService->GamePadDevice)
			{
				break;
			}

			if (psGyroscopeEnabled && SDL_GamepadHasSensor(GGamepadService->GamePadDevice, SDL_SENSOR_GYRO))
			{
				float gyroscopeVal[3];
				SDL_GetGamepadSensorData(GGamepadService->GamePadDevice, SDL_SENSOR_GYRO, gyroscopeVal, 3);
				pInput->GamepadGyroscopeUpdate({gyroscopeVal[0], gyroscopeVal[1], gyroscopeVal[2]});
			}
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
				pInput->receive_keyboard_addedorremoved((Event.kdevice.which), true);
			}

			break;
		}
		case SDL_EVENT_KEYBOARD_REMOVED:
		{
			if (pInput->receive_keyboard_addedorremoved)
			{
				pInput->receive_keyboard_addedorremoved(0, false);
			}

			break;
		}
		case SDL_EVENT_MOUSE_ADDED:
		{
			if (pInput->receive_mouse_addedorremoved)
			{
				pInput->receive_mouse_addedorremoved((Event.mdevice.which), true);
			}

			break;
		}
		case SDL_EVENT_MOUSE_REMOVED:
		{
			if (pInput->receive_mouse_addedorremoved)
			{
				pInput->receive_mouse_addedorremoved(0, false);
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
			if (!pInput->GetControllerMode())
			{
				pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
			}
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

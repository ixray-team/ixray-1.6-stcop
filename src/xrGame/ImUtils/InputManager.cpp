#include "stdafx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_input.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

#include "ImUtils.h"

#include <magic_enum/magic_enum.hpp>

struct InGameEditor_InputManager
{
	bool once_init = false;
	bool was_inited = false;
	unsigned char current_mouse_count = 0;
	unsigned char current_keyboard_count = 0;
	unsigned char current_gamepad_count = 0;
	const CInputDevice* pMouses = nullptr;
	const CInputDevice* pKeyboards = nullptr;
	const CInputDevice* pGamepads = nullptr;
	CInputDevice devices[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT];
	CInputDeviceVendorInfo infos[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT];
} ie_inputmanager;

void InitializeInGameEditor_InputManager(InGameEditor_InputManager*);
void UpdateInGameEditor_InputManager(InGameEditor_InputManager*);

void InGameEditor_InputManager_CallbackOnGamepadAddedOrRemoved(void* p_handle_device, bool added)
{
	if (ie_inputmanager.was_inited)
	{
		ie_inputmanager.was_inited = false;
	}

	InitializeInGameEditor_InputManager(&ie_inputmanager);
}

void InGameEditor_InputManager_CallbackOnKeyboardAddedOrRemoved(void* p_handle_device, bool added)
{
	if (ie_inputmanager.was_inited)
	{
		ie_inputmanager.was_inited = false;
	}

	InitializeInGameEditor_InputManager(&ie_inputmanager);
}

void InGameEditor_InputManager_CallbackOnMouseAddedOrRemoved(void* p_handle_device, bool added)
{
	if (ie_inputmanager.was_inited)
	{
		ie_inputmanager.was_inited = false;
	}

	InitializeInGameEditor_InputManager(&ie_inputmanager);
}

void InitializeInGameEditor_InputManager(InGameEditor_InputManager* p_editor)
{
	if (p_editor)
	{
		if (p_editor->was_inited == false)
		{
			p_editor->once_init = false;
			p_editor->was_inited = false;
			p_editor->current_mouse_count = 0;
			p_editor->current_keyboard_count = 0;
			p_editor->current_gamepad_count = 0;
			p_editor->pMouses = nullptr;
			p_editor->pKeyboards = nullptr;
			p_editor->pGamepads = nullptr;

			std::memset(p_editor->devices, 0, sizeof(p_editor->devices));
			std::memset(p_editor->infos, 0, sizeof(p_editor->infos));

			if (pInput)
			{
				pInput->GetConnectedInputDevices(p_editor->devices);

				for (unsigned char i = static_cast<unsigned char>(eInputDeviceType::keyboard); i < static_cast<unsigned char>(eInputDeviceType::keyboard) + DEF_XR_INPUT_MAX_INPUT_CONNECTED_KEYBOARD_COUNT; ++i)
				{
					R_ASSERT(p_editor->devices[i].type == eInputDeviceType::keyboard);

					if (p_editor && p_editor->devices[i].type == eInputDeviceType::keyboard)
						p_editor->current_keyboard_count++;
				}

				for (unsigned char i = static_cast<unsigned char>(eInputDeviceType::gamepad); i < static_cast<unsigned char>(eInputDeviceType::gamepad) + DEF_XR_INPUT_MAX_INPUT_CONNECTED_GAMEPAD_COUNT; ++i)
				{
			//		R_ASSERT(p_editor->devices[i].type == eInputDeviceType::gamepad);

					if (p_editor && p_editor->devices[i].type == eInputDeviceType::gamepad)
						p_editor->current_gamepad_count++;
				}

				for (unsigned char i = static_cast<unsigned char>(eInputDeviceType::mouse); i < static_cast<unsigned char>(eInputDeviceType::mouse) + DEF_XR_INPUT_MAX_INPUT_CONNECTED_MOUSE_COUNT; ++i)
				{
					R_ASSERT(p_editor->devices[i].type == eInputDeviceType::mouse);

					if (p_editor && p_editor->devices[i].type == eInputDeviceType::mouse)
						p_editor->current_mouse_count++;
				}

				p_editor->pMouses = &p_editor->devices[static_cast<unsigned char>(eInputDeviceType::mouse)];
				p_editor->pGamepads = &p_editor->devices[static_cast<unsigned char>(eInputDeviceType::gamepad)];
				p_editor->pKeyboards = &p_editor->devices[static_cast<unsigned char>(eInputDeviceType::keyboard)];

				pInput->GetInfoAboutConnectedInputDevices(p_editor->devices, p_editor->infos);


				pInput->receive_gamepad_addedorremoved = InGameEditor_InputManager_CallbackOnGamepadAddedOrRemoved;
				pInput->receive_keyboard_addedorremoved = InGameEditor_InputManager_CallbackOnKeyboardAddedOrRemoved;
				pInput->receive_mouse_addedorremoved = InGameEditor_InputManager_CallbackOnMouseAddedOrRemoved;
			}

			p_editor->was_inited = true;
		}
	}
}

void UpdateInGameEditor_InputManager(InGameEditor_InputManager* p_editor)
{
	if (p_editor)
	{
		if (pInput)
		{
		}
	}
}


void RenderKeyboard(const CInputDevice* pKeyboard, const CInputDeviceVendorInfo* pInfo)
{
	if (pKeyboard && pInfo)
	{
		char header_name[64];
		std::sprintf(header_name, "%s = %s", magic_enum::enum_name(eInputDeviceType::keyboard).data(), pInfo->name);

		if (ImGui::CollapsingHeader(header_name))
		{

		}
	}
}

void RenderMouse(const CInputDevice* pMouse, const CInputDeviceVendorInfo* pInfo)
{
	if (pMouse && pInfo)
	{
		char header_name[64];
		std::sprintf(header_name, "%s = %s", magic_enum::enum_name(eInputDeviceType::mouse).data(), pInfo->name);

		if (ImGui::CollapsingHeader(header_name))
		{

		}
	}
}

void RenderGamepad(const CInputDevice* pGamepad, const CInputDeviceVendorInfo* pInfo)
{
	if (pGamepad && pInfo)
	{
		char header_name[64];
		std::sprintf(header_name, "%s = %s", magic_enum::enum_name(eInputDeviceType::gamepad).data(), pInfo->name);

		if (ImGui::CollapsingHeader(header_name))
		{

		}
	}
}

void RenderToolsInputManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_InputManager)])
		return;

	if (ie_inputmanager.once_init == false)
	{
		InitializeInGameEditor_InputManager(&ie_inputmanager);
		ie_inputmanager.once_init = true;
	}

	UpdateInGameEditor_InputManager(&ie_inputmanager);

	if (ImGui::Begin("Editor - [Input]##ToolsInGameImGui", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_InputManager)]))
	{
		if (pInput)
		{
			if (ImGui::BeginTabBar("##TII"))
			{
				if (ImGui::BeginTabItem("Info"))
				{

					ImGui::Text("Connected devices: ");

					ImGui::Text("\t%ss: %d", magic_enum::enum_name(eInputDeviceType::keyboard).data(), ie_inputmanager.current_keyboard_count);
					ImGui::Text("\t%ss: %d", magic_enum::enum_name(eInputDeviceType::gamepad).data(), ie_inputmanager.current_gamepad_count);
					ImGui::Text("\t%ss: %d", magic_enum::enum_name(eInputDeviceType::mouse).data(), ie_inputmanager.current_mouse_count);

					ImGui::Separator();

					{
						char header_name[32];
						std::sprintf(header_name, "%ss = %d", magic_enum::enum_name(eInputDeviceType::keyboard).data(), ie_inputmanager.current_keyboard_count);

						const char* _kDeviceTypeSectionName = magic_enum::enum_name(eInputDeviceType::keyboard).data();

						if (ImGui::CollapsingHeader(header_name))
						{
							ImGui::Text("Max supported %ss: %d", _kDeviceTypeSectionName, DEF_XR_INPUT_MAX_INPUT_CONNECTED_KEYBOARD_COUNT);
							ImGui::Text("Current %ss count: %d", _kDeviceTypeSectionName, ie_inputmanager.current_keyboard_count);


							for (unsigned char i = 0; i < ie_inputmanager.current_keyboard_count; ++i)
							{
								const CInputDevice* pKeyboard = &ie_inputmanager.pKeyboards[i];

								RenderKeyboard(pKeyboard, &ie_inputmanager.infos[static_cast<unsigned char>(eInputDeviceType::keyboard) + i]);
							}
						}
					}

					{
						char header_name[32];
						std::sprintf(header_name, "%ss = %d", magic_enum::enum_name(eInputDeviceType::mouse).data(), ie_inputmanager.current_mouse_count);

						const char* _kDeviceTypeSectionName = magic_enum::enum_name(eInputDeviceType::mouse).data();

						if (ImGui::CollapsingHeader(header_name))
						{
							ImGui::Text("Max supported %ss: %d", _kDeviceTypeSectionName, DEF_XR_INPUT_MAX_INPUT_CONNECTED_MOUSE_COUNT);
							ImGui::Text("Current %ss count: %d", _kDeviceTypeSectionName, ie_inputmanager.current_mouse_count);

							for (unsigned char i = 0; i < ie_inputmanager.current_mouse_count; ++i)
							{
								const CInputDevice* pMouse = &ie_inputmanager.pMouses[i];

								RenderMouse(pMouse, &ie_inputmanager.infos[static_cast<unsigned char>(eInputDeviceType::mouse) + i]);
							}
						}
					}

					{
						char header_name[32];
						std::sprintf(header_name, "%ss = %d", magic_enum::enum_name(eInputDeviceType::gamepad).data(), ie_inputmanager.current_gamepad_count);

						const char* _kDeviceTypeSectionName = magic_enum::enum_name(eInputDeviceType::gamepad).data();

						if (ImGui::CollapsingHeader(header_name))
						{
							ImGui::Text("Max supported %ss: %d", _kDeviceTypeSectionName, DEF_XR_INPUT_MAX_INPUT_CONNECTED_GAMEPAD_COUNT);
							ImGui::Text("Current %ss count: %d", _kDeviceTypeSectionName, ie_inputmanager.current_gamepad_count);

							for (unsigned char i = 0; i < ie_inputmanager.current_gamepad_count; ++i)
							{
								const CInputDevice* pGamepad = &ie_inputmanager.pGamepads[i];

								RenderGamepad(pGamepad, &ie_inputmanager.infos[static_cast<unsigned char>(eInputDeviceType::gamepad) + i]);
							}
						}
					}

					ImGui::EndTabItem();
				}
			}
			ImGui::EndTabBar();

		}
		else
		{
			ImGui::Text("Initialize your CInput instance!");
		}
	}
	ImGui::End();
}


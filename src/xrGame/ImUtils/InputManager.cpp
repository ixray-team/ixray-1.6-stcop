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

struct InGameEditor_InputManager
{
	bool was_inited = false;
	unsigned char current_mouse_count = 0;
	unsigned char current_keyboard_count = 0;
	unsigned char current_gamepad_count = 0;
	const CInputDevice* pMouses = nullptr;
	const CInputDevice* pKeyboards = nullptr;
	const CInputDevice* pGamepads = nullptr;
	CInputDevice devices[DEF_XR_INPUT_MAX_INPUT_CONNECTED_DEVICES_COUNT];
} ie_inputmanager;


void RenderToolsInputManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_InputManager)])
		return;

	if (ie_inputmanager.was_inited == false)
	{
		// todo: continue;
		ie_inputmanager.was_inited = true;
	}

	if (ImGui::Begin("Editor - [Input]##ToolsInGameImGui", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_InputManager)]))
	{
		if (pInput)
		{
			if (ImGui::BeginTabBar("##TII"))
			{
				if (ImGui::BeginTabItem("Info"))
				{

					ImGui::Text("Connected devices: ");

					ImGui::Text("\t%ss:", translateInputDeviceTypeToString(eInputDeviceType::kKeyboard));
					ImGui::Text("\t%ss:", translateInputDeviceTypeToString(eInputDeviceType::kGamepad));
					ImGui::Text("\t%ss:", translateInputDeviceTypeToString(eInputDeviceType::kMouse));

					ImGui::Separator();

					{
						char header_name[32];
						std::sprintf(header_name, "%ss = %d", translateInputDeviceTypeToString(eInputDeviceType::kKeyboard), ie_inputmanager.current_keyboard_count);

						constexpr const char* _kDeviceTypeSectionName = translateInputDeviceTypeToString(eInputDeviceType::kKeyboard);

						if (ImGui::CollapsingHeader(header_name))
						{
							ImGui::Text("Max supported %ss: %d", _kDeviceTypeSectionName, DEF_XR_INPUT_MAX_INPUT_CONNECTED_KEYBOARD_COUNT);
							ImGui::Text("Current %ss count: %d", _kDeviceTypeSectionName, ie_inputmanager.current_keyboard_count);
						}
					}

					{
						char header_name[32];
						std::sprintf(header_name, "%ss = %d", translateInputDeviceTypeToString(eInputDeviceType::kMouse), ie_inputmanager.current_mouse_count);

						constexpr const char* _kDeviceTypeSectionName = translateInputDeviceTypeToString(eInputDeviceType::kMouse);

						if (ImGui::CollapsingHeader(header_name))
						{
							ImGui::Text("Max supported %ss: %d", _kDeviceTypeSectionName, DEF_XR_INPUT_MAX_INPUT_CONNECTED_MOUSE_COUNT);
							ImGui::Text("Current %ss count: %d", _kDeviceTypeSectionName, ie_inputmanager.current_mouse_count);
						}
					}

					{
						char header_name[32];
						std::sprintf(header_name, "%ss = %d", translateInputDeviceTypeToString(eInputDeviceType::kGamepad), ie_inputmanager.current_gamepad_count);
						
						constexpr const char* _kDeviceTypeSectionName = translateInputDeviceTypeToString(eInputDeviceType::kGamepad);

						if (ImGui::CollapsingHeader(header_name))
						{
							ImGui::Text("Max supported %ss: %d", _kDeviceTypeSectionName, DEF_XR_INPUT_MAX_INPUT_CONNECTED_GAMEPAD_COUNT);
							ImGui::Text("Current %ss count: %d", _kDeviceTypeSectionName, ie_inputmanager.current_gamepad_count);
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


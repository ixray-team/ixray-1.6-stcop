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
	bool once_init = false;
	bool was_inited = false;
	bool show_current_device_tab = false;
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

void InGameEditor_InputManager_CallbackOnGamepadAddedOrRemoved(u32 p_handle_device, bool added)
{
	if (ie_inputmanager.was_inited)
	{
		ie_inputmanager.was_inited = false;
	}

	InitializeInGameEditor_InputManager(&ie_inputmanager);
}

void InGameEditor_InputManager_CallbackOnKeyboardAddedOrRemoved(u32 p_handle_device, bool added)
{
	if (ie_inputmanager.was_inited)
	{
		ie_inputmanager.was_inited = false;
	}

	InitializeInGameEditor_InputManager(&ie_inputmanager);
}

void InGameEditor_InputManager_CallbackOnMouseAddedOrRemoved(u32 p_handle_device, bool added)
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

struct Key {
	bool is_modifier;
	bool is_visible;
	float width;  // in units of standard key width
	float height;
	char label[16];
	ImVec4 color;

	Key(
		const std::string_view& lbl = "",
		ImVec4 col = ImVec4(0.2f, 0.2f, 0.2f, 1.0f), 
		bool mod = false, 
		float w = 1.0f,
		bool visible = true,
		float h = 1.0f
	)
		: is_modifier(mod), is_visible(visible), width(w), height(h), label(), color(col)
	{
		std::strcpy(label, lbl.data());
		label[(sizeof(label) / sizeof(label[0])) - 1] = 0;
	}
};

class GeneralKeyboardLayout {
private:
	float base_key_width;
	float base_key_height;
	const ImVec4 default_key_color = ImVec4(0.2f, 0.2f, 0.2f, 1.0f);
	const ImVec4 modifier_key_color = ImVec4(0.4f, 0.4f, 0.4f, 1.0f);
	const ImVec4 color_function_key = ImVec4(0.1f, 0.4f, 0.8f, 1.0f);
	const ImVec4 color_function2_key = ImVec4(0.3f, 0.6f, 0.8f, 1.0f);
	const ImVec4 color_general_key = ImVec4(0.3f, 0.3f, 0.5f, 1.0f);
	const ImVec4 color_number_key = ImVec4(0.5f, 0.22f, 0.3f, 1.0f);
	const ImVec4 color_other_key = ImVec4(0.4f, 0.7f, 0.4f, 1.0f);
	const ImVec4 color_numpad_key = ImVec4(0.92f, 0.45f, 0.12f, 0.9f);
	const ImVec4 color_not_supported_by_input = ImVec4(0.3f, 0.3f, 0.3f, 1.0f);

	Key keys_function[21] = {
		Key("esc", color_function2_key, false, 1.5f),
		Key("F1", color_function_key),
		Key("F2", color_function_key),
		Key("F3", color_function_key),
		Key("F4", color_function_key),
		Key("F5", color_function_key),
		Key("F6", color_function_key),
		Key("F7", color_function_key),
		Key("F8", color_function_key),
		Key("F9", color_function_key),
		Key("F10", color_function_key),
		Key("F11", color_function_key),
		Key("F12", color_function_key),
		Key("CD/DVD", color_not_supported_by_input),
		Key("F13", color_function_key),
		Key("F14", color_function_key),
		Key("F15", color_function_key),
		Key("F16", color_function_key),
		Key("F17", color_function_key),
		Key("F18", color_function_key),
		Key("F19", color_function_key)
	};

	Key keys_numbers[21] = {
		Key("~", color_general_key),
		Key("1", color_number_key),
		Key("2", color_number_key),
		Key("3", color_number_key),
		Key("4", color_number_key),
		Key("5", color_number_key),
		Key("6", color_number_key),
		Key("7", color_number_key),
		Key("8", color_number_key),
		Key("9", color_number_key),
		Key("0", color_number_key),
		Key("-", color_general_key),
		Key("=", color_general_key),
		Key("backspace", color_general_key, false, 1.5f),
		Key("fn", color_other_key),
		Key("home", color_other_key),
		Key("pg up", color_other_key),
		Key("clear##NPAD", color_numpad_key),
		Key("=##NPAD", color_numpad_key),
		Key("/##NPAD", color_numpad_key),
		Key("*##NPAD", color_numpad_key)
	};

	Key keys_row3[21] = {
		Key("tab", color_function2_key, false, 1.5f),
		Key("Q", color_general_key),
		Key("W", color_general_key),
		Key("E", color_general_key),
		Key("R", color_general_key),
		Key("T", color_general_key),
		Key("Y", color_general_key),
		Key("U", color_general_key),
		Key("I", color_general_key),
		Key("O", color_general_key),
		Key("P", color_general_key),
		Key("[", color_general_key),
		Key("]", color_general_key),
		Key("\\", color_general_key),
		Key("delete", color_other_key),
		Key("end", color_other_key),
		Key("pg down", color_other_key),
		Key("7##NPAD", color_numpad_key),
		Key("8##NPAD", color_numpad_key),
		Key("9##NPAD", color_numpad_key),
		Key("-##NPAD", color_numpad_key)
	};

	Key keys_row4[20] = {
		Key("caps lock", color_function2_key, false, 1.75f),
		Key("A", color_general_key),
		Key("S", color_general_key),
		Key("D", color_general_key),
		Key("F", color_general_key),
		Key("G", color_general_key),
		Key("H", color_general_key),
		Key("J", color_general_key),
		Key("K", color_general_key),
		Key("L", color_general_key),
		Key(";", color_general_key),
		Key("'", color_general_key),
		Key("return", color_function2_key, false, 1.75f),
		Key("##INVIS1", color_general_key, false, 1.1f, false),
		Key("##INVIS2", color_general_key, false, 1.0f, false),
		Key("##INVIS3", color_general_key, false, 1.0f, false),
		Key("4##NPAD", color_numpad_key),
		Key("5##NPAD", color_numpad_key),
		Key("6##NPAD", color_numpad_key),
		Key("+##NPAD", color_numpad_key),
	};

	Key keys_row5[18] = {
		Key("l. shift", color_function2_key, false, 2.25f),
		Key("Z", color_general_key),
		Key("X", color_general_key),
		Key("C", color_general_key),
		Key("V", color_general_key),
		Key("B", color_general_key),
		Key("N", color_general_key),
		Key("M", color_general_key),
		Key(",", color_general_key),
		Key(".", color_general_key),
		Key("/", color_general_key),
		Key("r. shift", color_function2_key, false, 2.25f),
		Key("##INVIS4", color_general_key, false, 1.2f, false),
		Key("up", color_other_key),
		Key("##INVIS5", color_general_key, false, 1.0f, false),
		Key("1##NPAD", color_numpad_key),
		Key("2##NPAD", color_numpad_key),
		Key("3##NPAD", color_numpad_key),
	};

	Key keys_row6[14] = {
		Key("l. ctrl", color_function2_key, false, 1.5f),
		Key("l. alt", color_function2_key),
		Key("l. win.", color_function2_key,false, 1.5f),
		Key("space", color_function2_key, false, 7.0f),
		Key("r. win", color_function2_key, false, 1.5f),
		Key("r. alt", color_function2_key, false),
		Key("r. ctrl", color_function2_key, false, 1.5f),
		Key("##INVIS6", color_general_key, false, 0.1f, false),
		Key("left", color_other_key),
		Key("down", color_other_key),
		Key("right", color_other_key),
		Key("0##NPAD", color_numpad_key, false, 2.1f),
		Key(".##NPAD", color_numpad_key),
		Key("enter##NPAD", color_numpad_key, false, 1.0f, true, 2.3f),
	};

	const unsigned char rowCount[6] = {
		sizeof(keys_function)/sizeof(keys_function[0]),
		sizeof(keys_numbers)/sizeof(keys_numbers[0]),
		sizeof(keys_row3)/sizeof(keys_row3[0]),
		sizeof(keys_row4)/sizeof(keys_row4[0]),
		sizeof(keys_row5)/sizeof(keys_row5[0]),
		sizeof(keys_row6)/sizeof(keys_row6[0])
	};

	const Key* p_keys[6] = 
	{
		&keys_function[0],
		&keys_numbers[0],
		&keys_row3[0],
		&keys_row4[0],
		&keys_row5[0],
		&keys_row6[0]
	};

public:


	unsigned char GetFunctionKeyIndex(SDL_Scancode code) noexcept
	{
		switch (code)
		{
		case SDL_Scancode::SDL_SCANCODE_ESCAPE:
			return 0;
		case SDL_Scancode::SDL_SCANCODE_F1:
			return 1;
		case SDL_Scancode::SDL_SCANCODE_F2:
			return 2;
		case SDL_Scancode::SDL_SCANCODE_F3:
			return 3;
		case SDL_Scancode::SDL_SCANCODE_F4:
			return 4;
		case SDL_Scancode::SDL_SCANCODE_F5:
			return 5;
		case SDL_Scancode::SDL_SCANCODE_F6:
			return 6;
		case SDL_Scancode::SDL_SCANCODE_F7:
			return 7;
		case SDL_Scancode::SDL_SCANCODE_F8:
			return 8;
		case SDL_Scancode::SDL_SCANCODE_F9:
			return 9;
		case SDL_Scancode::SDL_SCANCODE_F10:
			return 10;
		case SDL_Scancode::SDL_SCANCODE_F11:
			return 11;
		case SDL_Scancode::SDL_SCANCODE_F12:
			return 12;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	unsigned char GetNumberKeyIndex(SDL_Scancode code) noexcept
	{
		switch (code)
		{
		case SDL_Scancode::SDL_SCANCODE_APOSTROPHE:
			return 0;
		case SDL_Scancode::SDL_SCANCODE_1:
			return 1;
		case SDL_Scancode::SDL_SCANCODE_2:
			return 2;
		case SDL_Scancode::SDL_SCANCODE_3:
			return 3;
		case SDL_Scancode::SDL_SCANCODE_4:
			return 4;
		case SDL_Scancode::SDL_SCANCODE_5:
			return 5;
		case SDL_Scancode::SDL_SCANCODE_6:
			return 6;
		case SDL_Scancode::SDL_SCANCODE_7:
			return 7;
		case SDL_Scancode::SDL_SCANCODE_8:
			return 8;
		case SDL_Scancode::SDL_SCANCODE_9:
			return 9;
		case SDL_Scancode::SDL_SCANCODE_0:
			return 10;
		case SDL_Scancode::SDL_SCANCODE_MINUS:
			return 11;
		case SDL_Scancode::SDL_SCANCODE_EQUALS:
			return 12;
		case SDL_Scancode::SDL_SCANCODE_BACKSPACE:
			return 13;
		case SDL_Scancode::SDL_SCANCODE_INSERT:
			return 14;
		case SDL_Scancode::SDL_SCANCODE_HOME:
			return 15;
		case SDL_Scancode::SDL_SCANCODE_PAGEUP:
			return 16;
		case SDL_Scancode::SDL_SCANCODE_CLEAR:
			return 17;
		case SDL_Scancode::SDL_SCANCODE_KP_EQUALS:
			return 18;
		case SDL_Scancode::SDL_SCANCODE_KP_DIVIDE:
			return 19;
		case SDL_Scancode::SDL_SCANCODE_KP_MULTIPLY:
			return 20;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	unsigned char GetRow3KeyIndex(SDL_Scancode code) noexcept
	{
		switch (code)
		{
		case SDL_Scancode::SDL_SCANCODE_TAB:
			return 0;
		case SDL_Scancode::SDL_SCANCODE_Q:
			return 1;
		case SDL_Scancode::SDL_SCANCODE_W:
			return 2;
		case SDL_Scancode::SDL_SCANCODE_E:
			return 3;
		case SDL_Scancode::SDL_SCANCODE_R:
			return 4;
		case SDL_Scancode::SDL_SCANCODE_T:
			return 5;
		case SDL_Scancode::SDL_SCANCODE_Y:
			return 6;
		case SDL_Scancode::SDL_SCANCODE_U:
			return 7;
		case SDL_Scancode::SDL_SCANCODE_I:
			return 8;
		case SDL_Scancode::SDL_SCANCODE_O:
			return 9;
		case SDL_Scancode::SDL_SCANCODE_P:
			return 10;
		case SDL_Scancode::SDL_SCANCODE_LEFTBRACKET:
			return 11;
		case SDL_Scancode::SDL_SCANCODE_RIGHTBRACKET:
			return 12;
		case SDL_Scancode::SDL_SCANCODE_BACKSLASH:
			return 13;
		case SDL_Scancode::SDL_SCANCODE_DELETE:
			return 14;
		case SDL_Scancode::SDL_SCANCODE_END:
			return 15;
		case SDL_Scancode::SDL_SCANCODE_PAGEDOWN:
			return 16;
		case SDL_Scancode::SDL_SCANCODE_KP_7:
			return 17;
		case SDL_Scancode::SDL_SCANCODE_KP_8:
			return 18;
		case SDL_Scancode::SDL_SCANCODE_KP_9:
			return 19;
		case SDL_Scancode::SDL_SCANCODE_KP_MINUS:
			return 20;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	unsigned char GetRow4KeyIndex(SDL_Scancode code) noexcept
	{
		switch (code)
		{
		case SDL_Scancode::SDL_SCANCODE_CAPSLOCK:
			return 0;
		case SDL_Scancode::SDL_SCANCODE_A:
			return 1;
		case SDL_Scancode::SDL_SCANCODE_S:
			return 2;
		case SDL_Scancode::SDL_SCANCODE_D:
			return 3;
		case SDL_Scancode::SDL_SCANCODE_F:
			return 4;
		case SDL_Scancode::SDL_SCANCODE_G:
			return 5;
		case SDL_Scancode::SDL_SCANCODE_H:
			return 6;
		case SDL_Scancode::SDL_SCANCODE_J:
			return 7;
		case SDL_Scancode::SDL_SCANCODE_K:
			return 8;
		case SDL_Scancode::SDL_SCANCODE_L:
			return 9;
		case SDL_Scancode::SDL_SCANCODE_SEMICOLON:
			return 10;
		case SDL_Scancode::SDL_SCANCODE_APOSTROPHE:
			return 11;
		case SDL_Scancode::SDL_SCANCODE_RETURN:
			return 12;
		case SDL_Scancode::SDL_SCANCODE_KP_4:
			return 16;
		case SDL_Scancode::SDL_SCANCODE_KP_5:
			return 17;
		case SDL_Scancode::SDL_SCANCODE_KP_6:
			return 18;
		case SDL_Scancode::SDL_SCANCODE_KP_PLUS:
			return 19;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	unsigned char GetRow5KeyIndex(SDL_Scancode code) noexcept
	{
		switch (code)
		{
		case SDL_Scancode::SDL_SCANCODE_LSHIFT:
			return 0;
		case SDL_Scancode::SDL_SCANCODE_Z:
			return 1;
		case SDL_Scancode::SDL_SCANCODE_X:
			return 2;
		case SDL_Scancode::SDL_SCANCODE_C:
			return 3;
		case SDL_Scancode::SDL_SCANCODE_V:
			return 4;
		case SDL_Scancode::SDL_SCANCODE_B:
			return 5;
		case SDL_Scancode::SDL_SCANCODE_N:
			return 6;
		case SDL_Scancode::SDL_SCANCODE_M:
			return 7;
		case SDL_Scancode::SDL_SCANCODE_COMMA:
			return 8;
		case SDL_Scancode::SDL_SCANCODE_PERIOD:
			return 9;
		case SDL_Scancode::SDL_SCANCODE_SLASH:
			return 10;
		case SDL_Scancode::SDL_SCANCODE_RSHIFT:
			return 11;
		case SDL_Scancode::SDL_SCANCODE_UP:
			return 13;
		case SDL_Scancode::SDL_SCANCODE_KP_1:
			return 15;
		case SDL_Scancode::SDL_SCANCODE_KP_2:
			return 16;
		case SDL_Scancode::SDL_SCANCODE_3:
			return 17;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	unsigned char GetRow6KeyIndex(SDL_Scancode code) noexcept
	{
		switch (code)
		{
		case SDL_Scancode::SDL_SCANCODE_LCTRL:
			return 0;
		case SDL_Scancode::SDL_SCANCODE_LALT:
			return 1;
		case SDL_Scancode::SDL_SCANCODE_LGUI:
			return 2;
		case SDL_Scancode::SDL_SCANCODE_SPACE:
			return 3;
		case SDL_Scancode::SDL_SCANCODE_RGUI:
			return 4;
		case SDL_Scancode::SDL_SCANCODE_RALT:
			return 5;
		case SDL_Scancode::SDL_SCANCODE_RCTRL:
			return 6;
		case SDL_Scancode::SDL_SCANCODE_LEFT:
			return 8;
		case SDL_Scancode::SDL_SCANCODE_DOWN:
			return 9;
		case SDL_Scancode::SDL_SCANCODE_RIGHT:
			return 10;
		case SDL_Scancode::SDL_SCANCODE_KP_0:
			return 11;
		case SDL_Scancode::SDL_SCANCODE_KP_PERIOD:
			return 12;
		case SDL_Scancode::SDL_SCANCODE_KP_ENTER:
			return 13;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}


	GeneralKeyboardLayout()
		: base_key_width(40.0f),
		base_key_height(40.0f)
	{
	}

	void draw() {

		float total_base_width = 0.0f;
		constexpr int _kRowCount = sizeof(rowCount) / sizeof(rowCount[0]);
		for (unsigned char i = 0; i < _kRowCount; ++i)
		{
			float row_width = 0.0f;
			
			for (unsigned char j = 0; j < rowCount[i]; ++j)
			{
				const Key* p_buffer = p_keys[i];
				const Key& key = p_buffer[j];
				row_width += key.width;
			}
			

			// Add spacing between keys (n-1 spacings per row)
			row_width += (rowCount[i] - 1) * 0.1f; // 0.1 units for spacing
			total_base_width = std::max(total_base_width, row_width);
		}

		// Get available width and calculate scale
		float available_width = ImGui::GetContentRegionAvail().x;
		float scale = available_width / (total_base_width * base_key_width);

		// Apply scaling with constraints
		scale = std::min(scale, 2.0f); // Max 2x scale
		scale = std::max(scale, 0.3f); // Min 0.3x scale

		float key_width = base_key_width * scale;
		float key_height = base_key_height * scale;
		float spacing = 4.0f * scale;

		for (unsigned char  i = 0; i < _kRowCount; ++i)
		{
			ImGui::BeginGroup();

			for (unsigned char j = 0; j < rowCount[i]; ++j)
			{
				const Key* p_buffer = p_keys[i];
				const Key& key = p_buffer[j];
				float actual_width = key_width * key.width;

				// Set button color
				ImGui::PushStyleColor(ImGuiCol_Button, key.color);
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered,
					ImVec4(key.color.x * 1.2f, key.color.y * 1.2f, key.color.z * 1.2f, key.color.w));
				ImGui::PushStyleColor(ImGuiCol_ButtonActive,
					ImVec4(key.color.x * 0.8f, key.color.y * 0.8f, key.color.z * 0.8f, key.color.w));

				// Draw the key

				if (key.is_visible)
				{

					if (i == 5 && j == rowCount[i] - 1)
					{
						ImGui::SetCursorPosY(ImGui::GetCursorPosY() - (key_height*1.3f));
					}

					if (ImGui::Button(key.label, ImVec2(actual_width, key_height * key.height)))
					{

					}
				}
				else
				{
					ImGui::BeginDisabled();
					ImGui::InvisibleButton(key.label, ImVec2(actual_width, key_height));
					ImGui::EndDisabled();
				}

				ImGui::PopStyleColor(3);

				// Add spacing between keys
				ImGui::SameLine(0.0f, spacing);
			}
			ImGui::EndGroup();

			// Add spacing between rows
			ImGui::Dummy(ImVec2(0.0f, spacing));
		}
	}
};

class GeneralMouseLayout {
private:
	float base_key_width;
	float base_key_height;
	const ImVec4 color_general_key = ImVec4(0.3f, 0.3f, 0.5f, 1.0f);
	const ImVec4 color_side_key = ImVec4(0.5f, 0.22f, 0.3f, 1.0f);
	const ImVec4 color_middle_key = ImVec4(0.4f, 0.7f, 0.4f, 1.0f);

	Key keys[3] = {
		Key("left", color_general_key, false, 1.0f, true, 2.0f),
		Key("middle", color_middle_key, false, 1.0f, true, 1.0f),
		Key("right", color_general_key, false, 1.0f, true, 2.0f)
	};

	Key keys_side[2] = {
		Key("mouse4", color_side_key),
		Key("mouse5", color_side_key)
	};

	const unsigned char rowCount[2] =
	{
		sizeof(keys) / sizeof(keys[0]),
		sizeof(keys_side) / sizeof(keys_side[0])
	};

	const Key* p_keys[2] = {
		&keys[0],
		&keys_side[0]
	};

public:


	unsigned char GetMouseKeyIndex(Uint8 code) noexcept
	{
		switch (code)
		{
		case SDL_BUTTON_LEFT:
			return 0;
		case SDL_BUTTON_MIDDLE:
			return 1;
		case SDL_BUTTON_RIGHT:
			return 2;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	unsigned char GetMouseSideKeyIndex(Uint8 code) noexcept
	{
		switch (code)
		{
		case SDL_BUTTON_X1:
			return 0;
		case SDL_BUTTON_X2:
			return 1;
		default:
		{
			assert(false && "don't pass this please");
			return 0;
		}
		}
	}

	GeneralMouseLayout()
		: base_key_width(40.0f),
		base_key_height(40.0f)
	{
	}

	void draw() {

		float total_base_width = 0.0f;
		constexpr int _kRowCount = sizeof(rowCount) / sizeof(rowCount[0]);
		for (unsigned char i = 0; i < _kRowCount; ++i)
		{
			float row_width = 0.0f;

			for (unsigned char j = 0; j < rowCount[i]; ++j)
			{
				const Key* p_buffer = p_keys[i];
				const Key& key = p_buffer[j];
				row_width += key.width;
			}


			// Add spacing between keys (n-1 spacings per row)
			row_width += (rowCount[i] - 1) * 0.1f; // 0.1 units for spacing
			total_base_width = std::max(total_base_width, row_width);
		}

		// Get available width and calculate scale
		float available_width = ImGui::GetContentRegionAvail().x;
		float scale = available_width / (total_base_width * base_key_width);

		// Apply scaling with constraints
		scale = std::min(scale, 2.0f); // Max 2x scale
		scale = std::max(scale, 0.3f); // Min 0.3x scale

		float key_width = base_key_width * scale;
		float key_height = base_key_height * scale;
		float spacing = 4.0f * scale;

		for (unsigned char i = 0; i < _kRowCount; ++i)
		{
			ImGui::BeginGroup();

			for (unsigned char j = 0; j < rowCount[i]; ++j)
			{
				const Key* p_buffer = p_keys[i];
				const Key& key = p_buffer[j];
				float actual_width = key_width * key.width;

				// Set button color
				ImGui::PushStyleColor(ImGuiCol_Button, key.color);
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered,
					ImVec4(key.color.x * 1.2f, key.color.y * 1.2f, key.color.z * 1.2f, key.color.w));
				ImGui::PushStyleColor(ImGuiCol_ButtonActive,
					ImVec4(key.color.x * 0.8f, key.color.y * 0.8f, key.color.z * 0.8f, key.color.w));

				// Draw the key

				if (key.is_visible)
				{

					if (i == 5 && j == rowCount[i] - 1)
					{
						ImGui::SetCursorPosY(ImGui::GetCursorPosY() - (key_height * 1.3f));
					}

					if (ImGui::Button(key.label, ImVec2(actual_width, key_height * key.height)))
					{

					}
				}
				else
				{
					ImGui::BeginDisabled();
					ImGui::InvisibleButton(key.label, ImVec2(actual_width, key_height));
					ImGui::EndDisabled();
				}

				ImGui::PopStyleColor(3);

				// Add spacing between keys
				ImGui::SameLine(0.0f, spacing);
			}
			ImGui::EndGroup();

			// Add spacing between rows
			ImGui::Dummy(ImVec2(0.0f, spacing));
		}
	}
};

// Usage in your ImGui application:
GeneralKeyboardLayout keyboard_layout;
GeneralMouseLayout mouse_layout;


void RenderKeyboard(InGameEditor_InputManager* p_editor, const CInputDevice* pKeyboard, const CInputDeviceVendorInfo* pInfo, unsigned char id)
{
	if (pKeyboard && pInfo && p_editor)
	{
		char tab_name[32];

		std::sprintf(tab_name, "%s#%d", magic_enum::enum_name(eInputDeviceType::keyboard).data(), id);

		if (ImGui::BeginTabItem(tab_name))
		{
			ImGui::Text("Name: %s", pInfo->name);


			ImGui::SeparatorText("Layout");

			keyboard_layout.draw();

			ImGui::SeparatorText("Bindings");


			ImGui::EndTabItem();
		}
	}
}

void RenderMouse(InGameEditor_InputManager* p_editor, const CInputDevice* pMouse, const CInputDeviceVendorInfo* pInfo, unsigned char id)
{
	if (pMouse && pInfo && p_editor)
	{
		char tab_name[32];

		std::sprintf(tab_name, "%s#%d", magic_enum::enum_name(eInputDeviceType::mouse).data(), id);

		if (ImGui::BeginTabItem(tab_name))
		{
			ImGui::Text("Name: %s", pInfo->name);


			ImGui::SeparatorText("Layout");

			mouse_layout.draw();

			ImGui::SeparatorText("Bindings");


			ImGui::EndTabItem();
		}
	}
}

void RenderGamepad(InGameEditor_InputManager* p_editor, const CInputDevice* pGamepad, const CInputDeviceVendorInfo* pInfo, unsigned char id)
{
	if (pGamepad && pInfo && p_editor)
	{
		char tab_name[32];

		std::sprintf(tab_name, "%s#%d", magic_enum::enum_name(eInputDeviceType::gamepad).data(), id);

		if (ImGui::BeginTabItem(tab_name))
		{
			ImGui::Text("Name: %s", pInfo->name);

			ImGui::Text("Serial: %s", (char*)pInfo->data2);

			u16 vendor = *((u16*)(&pInfo->data[0]));
			u16 product = *((u16*)(&pInfo->data[2]));
			u16 version = *((u16*)(&pInfo->data[4]));
			ImGui::Text("Vendor: %d Product: %d Version: %d", vendor, product, version);

			ImGui::SeparatorText("Layout");

			

			ImGui::SeparatorText("Bindings");


			ImGui::EndTabItem();
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
						}
					}

					ImGui::EndTabItem();
				}

				{
					for (unsigned char i = 0; i < ie_inputmanager.current_keyboard_count; ++i)
					{
						const CInputDevice* pKeyboard = &ie_inputmanager.pKeyboards[i];

						RenderKeyboard(&ie_inputmanager, pKeyboard, &ie_inputmanager.infos[static_cast<unsigned char>(eInputDeviceType::keyboard) + i], i);
					}

					for (unsigned char i = 0; i < ie_inputmanager.current_mouse_count; ++i)
					{
						const CInputDevice* pMouse = &ie_inputmanager.pMouses[i];

						RenderMouse(&ie_inputmanager, pMouse, &ie_inputmanager.infos[static_cast<unsigned char>(eInputDeviceType::mouse) + i], i);
					}

					for (unsigned char i = 0; i < ie_inputmanager.current_gamepad_count; ++i)
					{
						const CInputDevice* pGamepad = &ie_inputmanager.pGamepads[i];

						RenderGamepad(&ie_inputmanager, pGamepad, &ie_inputmanager.infos[static_cast<unsigned char>(eInputDeviceType::gamepad) + i], i);
					}
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


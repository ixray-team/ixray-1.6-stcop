#pragma once
namespace ImGui
{
	IMGUI_API float GetWindowBarHeight(); 
	IMGUI_API ImVec2        CalcItemSize(ImVec2 size, float default_w, float default_h);
	IMGUI_API bool OpenPopupOnItemClick2(const char* str_id, ImGuiPopupFlags popup_flags);

    IMGUI_API bool InputFloat(const char* label, float* v, float step, float step_fast, int dec, ImGuiInputTextFlags flags = 0);
    IMGUI_API bool InputFloat2(const char* label, float v[2], int dec, ImGuiInputTextFlags flags = 0);
    IMGUI_API bool InputFloat3(const char* label, float v[3], int dec, ImGuiInputTextFlags flags = 0);
    IMGUI_API bool InputFloat4(const char* label, float v[4], int dec, ImGuiInputTextFlags flags = 0);
    IMGUI_API bool BeginPopupModal(const char* name, bool* p_open, ImGuiWindowFlags flags, bool open_always);
    IMGUI_API bool ArrowButton(const char* str_id, ImGuiDir dir, ImVec2 size, ImGuiButtonFlags flags);
	IMGUI_API void PushItemFlag(int option, bool enabled);
	IMGUI_API void PopItemFlag();
}

enum ImGuiItemFlags_
{
    // Controlled by user
    ImGuiItemFlags_None = 0,
    ImGuiItemFlags_NoTabStop = 1 << 0,  // false     // Disable keyboard tabbing. This is a "lighter" version of ImGuiItemFlags_NoNav.
    ImGuiItemFlags_ButtonRepeat = 1 << 1,  // false     // Button() will return true multiple times based on io.KeyRepeatDelay and io.KeyRepeatRate settings.
    ImGuiItemFlags_Disabled = 1 << 2,  // false     // Disable interactions but doesn't affect visuals. See BeginDisabled()/EndDisabled(). See github.com/ocornut/imgui/issues/211
    ImGuiItemFlags_NoNav = 1 << 3,  // false     // Disable any form of focusing (keyboard/gamepad directional navigation and SetKeyboardFocusHere() calls)
    ImGuiItemFlags_NoNavDefaultFocus = 1 << 4,  // false     // Disable item being a candidate for default focus (e.g. used by title bar items)
    ImGuiItemFlags_SelectableDontClosePopup = 1 << 5,  // false     // Disable MenuItem/Selectable() automatically closing their popup window
    ImGuiItemFlags_MixedValue = 1 << 6,  // false     // [BETA] Represent a mixed/indeterminate value, generally multi-selection where values differ. Currently only supported by Checkbox() (later should support all sorts of widgets)
    ImGuiItemFlags_ReadOnly = 1 << 7,  // false     // [ALPHA] Allow hovering interactions but underlying value is not changed.
    ImGuiItemFlags_NoWindowHoverableCheck = 1 << 8,  // false     // Disable hoverable check in ItemHoverable()
    ImGuiItemFlags_AllowOverlap = 1 << 9,  // false     // Allow being overlapped by another widget. Not-hovered to Hovered transition deferred by a frame.

    // Controlled by widget code
    ImGuiItemFlags_Inputable = 1 << 10, // false     // [WIP] Auto-activate input mode when tab focused. Currently only used and supported by a few items before it becomes a generic feature.
    ImGuiItemFlags_HasSelectionUserData = 1 << 11, // false     // Set by SetNextItemSelectionUserData()
};
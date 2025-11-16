#ifndef IMGUI_DEFINE_MATH_OPERATORS
#define IMGUI_DEFINE_MATH_OPERATORS
#endif

#include "imgui_internal.h"
#include "imgui_user.h"

float ImGui::GetWindowBarHeight()
{
    ImGuiWindow* window = GImGui->CurrentWindow;
    return window->MenuBarHeight;
}
bool ImGui::OpenPopupOnItemClick2(const char* str_id, ImGuiPopupFlags popup_flags)
{
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;
    int mouse_button = (popup_flags & ImGuiPopupFlags_MouseButtonMask_);
    if (IsMouseReleased(mouse_button) && IsItemHovered(ImGuiHoveredFlags_AllowWhenBlockedByPopup))
    {
        ImGuiID id = str_id ? window->GetID(str_id) : g.LastItemData.ID;    // If user hasn't passed an ID, we can use the LastItemID. Using LastItemID as a Popup ID won't conflict!
        IM_ASSERT(id != 0);                                             // You cannot pass a NULL str_id if the last item has no identifier (e.g. a Text() item)
        OpenPopupEx(id, popup_flags);
        return true;
    }
    return false;
}

bool ImGui::InputFloat(const char* label, float* v, float step, float step_fast, int dec, ImGuiInputTextFlags flags)
{
    char Format[256];
    sprintf(Format, "%%.%df", dec);
    return   InputFloat(label, v, step, step_fast, Format, flags);
}

bool ImGui::InputFloat2(const char* label, float v[2], int dec, ImGuiInputTextFlags flags)
{
    char Format[256];
    sprintf(Format, "%%.%df", dec);
    return InputFloat2(label, v, Format, flags);
}

bool ImGui::InputFloat3(const char* label, float v[3], int dec, ImGuiInputTextFlags flags)
{
    char Format[256];
    sprintf(Format, "%%.%df", dec);
    return  InputFloat3(label, v, Format, flags);
}

bool ImGui::InputFloat4(const char* label, float v[4], int dec, ImGuiInputTextFlags flags)
{
    char Format[256];
    sprintf(Format, "%%.%df", dec);
    return  InputFloat4(label, v, Format, flags);
}
bool ImGui::BeginPopupModal(const char* name, bool* p_open, ImGuiWindowFlags flags, bool open_always)
{
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;

    const ImGuiID id = window->GetID(name);
    if (!IsPopupOpen(id, ImGuiPopupFlags_None))
    {
        if (open_always)
        {
            OpenPopupEx(id);
        }
    }
    return  ImGui::BeginPopupModal(name, p_open, flags);
}

bool ImGui::BeginMenuI(const char* label, const char* icon, bool enabled)
{
    return BeginMenuEx(label, icon, enabled);
}

bool ImGui::MenuItemI(const char* label, const char* icon, const char* shortcut, bool selected, bool enabled)
{
    return MenuItemEx(label, icon, shortcut, selected, enabled);
}

bool ImGui::MenuItemI(const char* label, const char* icon, const char* shortcut, bool* p_selected, bool enabled)
{
    if (MenuItemEx(label, icon, shortcut, p_selected ? *p_selected : false, enabled))
    {
        if (p_selected)
            *p_selected = !*p_selected;
        return true;
    }
    return false;
}

void ImGui::RenderTextI(ImVec2 pos, const char* text, const float font_size, const char* text_end, bool hide_text_after_hash)
{
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;

    // Hide anything after a '##' string
    const char* text_display_end;
    if (hide_text_after_hash)
    {
        text_display_end = FindRenderedTextEnd(text, text_end);
    }
    else
    {
        if (!text_end)
            text_end = text + ImStrlen(text); // FIXME-OPT
        text_display_end = text_end;
    }

    if (text != text_display_end)
    {
        window->DrawList->AddText(g.Font, font_size, pos, GetColorU32(ImGuiCol_Text), text, text_display_end);
        if (g.LogEnabled)
            LogRenderedText(&pos, text, text_display_end);
    }
}

IMGUI_API bool ImGui::ArrowButton(const char* str_id, ImGuiDir dir, ImVec2 size, ImGuiButtonFlags flags)
{
    return ArrowButtonEx(str_id, dir, size, flags);
}
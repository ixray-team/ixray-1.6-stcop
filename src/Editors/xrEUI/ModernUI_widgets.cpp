#include "stdafx.h"
#include "ModernUI.h"
#include <imgui_internal.h>

XREUI_API bool XRay::ImGui::ToggleButton(const char* Label, bool* Flags, const ImVec2& Size)
{
	if(!Flags) return false;
	bool Enabled = *Flags;
	bool Changed = false;

	if (XRay::ImGui::Button(Label, Size, Flags))
	{
		*Flags = !*Flags;
		Changed = true;
		Enabled = !Enabled;
	}

	const ImVec4 FlagColor		= GetEditorColor(EEditorColors::Accent);
	const float StripeWidth		= GetEditorSize(EEditorSizes::IndicatorWidth);
	const float Rounding		= ::ImGui::GetStyle().FrameRounding;
	
	ImDrawList* DrawList		= ::ImGui::GetWindowDrawList();
	ImVec2		Min				= ::ImGui::GetItemRectMin();
	ImVec2		Max				= ::ImGui::GetItemRectMax();

	if (!Enabled)
	{
		DrawList->AddRectFilled(
			Min,
			ImVec2(Min.x + StripeWidth, Max.y),
			::ImGui::ColorConvertFloat4ToU32(FlagColor),
			Rounding,
			ImDrawFlags_RoundCornersLeft
		);
	}
	return Changed;
}

XREUI_API bool XRay::ImGui::ToggleFlagButton(const char* Label, uint32_t* Flags, uint32_t Mask, const ImVec2& Size)
{
	bool Enabled = (*Flags & Mask) != 0;
	bool Changed = false;

	const char* Text = Enabled ? "Enable" : "Disable";

	::ImGui::PushID(Label);
	if (XRay::ImGui::ToggleButton(Text, &Enabled, Size))
	{
		*Flags ^= Mask;
		Changed = true;
		Enabled = !Enabled;
	}
	::ImGui::PopID();
	return Changed;
}

// This button is made to be reused in toggles, so it has an optional bool* Toggle
// Needs to reduce the code copypaste 
XREUI_API bool XRay::ImGui::Button(const char* Label, const ImVec2& Size, bool* Toggle)
{
	// --- Styling ---
	const ImVec4 EnabledColor		= GetEditorColor(EEditorColors::Accent);
	const ImVec4 EnabledHover		= GetEditorColor(EEditorColors::ToggleHover);
	const ImVec4 EnabledActive		= GetEditorColor(EEditorColors::ToggleActive);
	const ImVec4 DisabledColor		= GetEditorColor(EEditorColors::ButtonTint);
	const ImVec4 DisabledHover		= GetEditorColor(EEditorColors::ButtonHover);
	const ImVec4 DisabledActive		= GetEditorColor(EEditorColors::ButtonActive);
	const ImVec4 BorderColor		= GetEditorColor(EEditorColors::ButtonBorderTint);
	const float  BorderSize			= GetEditorSize(EEditorSizes::ButtonBorderSize);

	if (Toggle) {
		::ImGui::PushStyleColor(ImGuiCol_Button,		*Toggle ? EnabledColor	: DisabledColor);
		::ImGui::PushStyleColor(ImGuiCol_ButtonHovered,	*Toggle ? EnabledHover	: DisabledHover);
		::ImGui::PushStyleColor(ImGuiCol_ButtonActive,	*Toggle ? EnabledActive	: DisabledActive);
	}
	else {
		::ImGui::PushStyleColor(ImGuiCol_Button,		DisabledColor);
		::ImGui::PushStyleColor(ImGuiCol_ButtonHovered,	DisabledHover);
		::ImGui::PushStyleColor(ImGuiCol_ButtonActive,	DisabledActive);
	}
	::ImGui::PushStyleColor(ImGuiCol_Border,		BorderColor);
	::ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, BorderSize);
	::ImGui::PushStyleVar(ImGuiStyleVar_ButtonTextAlign, { 0, 0.5 });

	bool button = ::ImGui::Button(Label, Size);

	::ImGui::PopStyleVar(2);
	::ImGui::PopStyleColor(4);

	return button;
}

XREUI_API bool XRay::ImGui::ToolbarIconButton(
	const	char*			id,
			ImTextureRef	texture,
			bool*			toggle,
			ImDrawFlags		rounding_flags,
			float	rounding,
			ImVec2	button_size,
			ImVec2	image_size)
{
	ImGuiWindow* window = ::ImGui::GetCurrentWindow();
	if (window->SkipItems)
		return false;

	::ImGui::PushID(id);

	// --- draw background ---
	bool		clicked		= ToolbarButtonBackground("##icon_btn", toggle, button_size, rounding_flags, rounding);
	ImDrawList*	dl			= ::ImGui::GetWindowDrawList();
	ImVec2		p_min		= ::ImGui::GetItemRectMin();
	ImVec2		p_max		= ::ImGui::GetItemRectMax();

	// --- center image ---
	ImVec2		center		= (p_min + p_max) * 0.5f;
	ImVec2		img_min		= center - image_size * 0.5f;
	ImVec2		img_max		= center + image_size * 0.5f;

	dl->AddImage(texture, img_min, img_max);

	::ImGui::PopID();
	return clicked;
}

XREUI_API bool XRay::ImGui::ToolbarButton(
	const	char*		id,
	const	char*		label,
			bool*		toggle,
			ImVec2		size,
			ImDrawFlags	rounding_flags,
			float		rounding)
{
	ImGuiWindow* window = ::ImGui::GetCurrentWindow();
	if (window->SkipItems)
		return false;

	::ImGui::PushID(id);

	// --- size calc stolen from imgui_widgets.cpp ---
	const	ImGuiStyle& style = ::ImGui::GetStyle();
	const	ImVec2 label_size = ::ImGui::CalcTextSize(label, NULL, true);
			ImVec2 sizeCalced = ::ImGui::CalcItemSize(size, label_size.x + style.FramePadding.x * 2.0f, label_size.y + style.FramePadding.y * 2.0f);

	// --- draw background ---
	bool clicked = ToolbarButtonBackground("##toggle_btn", toggle, sizeCalced, rounding_flags, rounding);

	ImDrawList* dl			= ::ImGui::GetWindowDrawList();
	ImVec2		p_min		= ::ImGui::GetItemRectMin();
	ImVec2		p_max		= ::ImGui::GetItemRectMax();

	// --- draw text centered ---
	ImVec2		text_size	= ::ImGui::CalcTextSize(label);
	ImVec2		center		= (p_min + p_max) * 0.5f;
	ImVec2		text_pos	= center - text_size * 0.5f;

	ImU32 text_col = ::ImGui::GetColorU32(ImGuiCol_Text);
	dl->AddText(text_pos, text_col, label);

	::ImGui::PopID();
	return clicked;
}

XREUI_API bool XRay::ImGui::ToolbarButtonBackground(
	const	char*		id,
			bool*		toggle,
			ImVec2		size,
			ImDrawFlags	rounding_flags,
			float		rounding)
{
	ImGuiWindow* window = ::ImGui::GetCurrentWindow();
	if (window->SkipItems)
		return false;

			bool clicked	= ::ImGui::InvisibleButton(id, size);
	const	bool hovered	= ::ImGui::IsItemHovered();
	const	bool active		= ::ImGui::IsItemActive();

			ImVec4 Color	= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ToolbarButtonTint);
			ImVec4 Hover	= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ButtonHover);
			ImVec4 Active	= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ButtonActive);

	// --- toggle logic ---
	if (toggle) {
		if (clicked)
			*toggle = !*toggle;
		if (*toggle == true) {
			Color	= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::Accent);
			Hover	= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ToggleHover);
			Active	= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ToggleActive);
		}
	}

	// --- colors ---
	ImU32 col;
	if (active)
		col = ::ImGui::GetColorU32(Active);
	else if (hovered)
		col = ::ImGui::GetColorU32(Hover);
	else
		col = ::ImGui::GetColorU32(Color);

	// --- draw background ---
	ImDrawList*	dl		= ::ImGui::GetWindowDrawList();
	ImVec2		p_min	= ::ImGui::GetItemRectMin();
	ImVec2		p_max	= ::ImGui::GetItemRectMax();

	dl->AddRectFilled(
		p_min,
		p_max,
		col,
		rounding,
		rounding_flags);

	return clicked;
}
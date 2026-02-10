#include "stdafx.h"
#include "ModernUI.h"

static xr_hash_map<XRay::ImGui::EEditorColors, ImColor> EditorColors;

XREUI_API bool XRay::ImGui::ToggleFlagButton(const char* Label, uint32_t* Flags, uint32_t Mask, const ImVec2& Size)
{
	bool Enabled = (*Flags & Mask) != 0;
	bool Changed = false;

	::ImGui::PushID(Label);

	const ImVec4 EnabledColor = GetEditorColor(EEditorColors::ToggleColorActive);
	const ImVec4 DisabledColor = GetEditorColor(EEditorColors::ButtonTint);
	const ImVec4 BorderColor = EnabledColor;

	constexpr float StripeWidth = 3.0f;
	constexpr float Rounding = 5.0f;

	::ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, Rounding);
	::ImGui::PushStyleColor(ImGuiCol_Button, Enabled ? EnabledColor : DisabledColor);
	::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, Enabled ? EnabledColor : DisabledColor);
	::ImGui::PushStyleColor(ImGuiCol_ButtonActive, Enabled ? EnabledColor : DisabledColor);

	const char* Text = Enabled ? "Enable" : "Disable";

	if (::ImGui::Button(Text, Size))
	{
		*Flags ^= Mask;
		Changed = true;
		Enabled = !Enabled;
	}

	::ImGui::PopStyleColor(3);
	::ImGui::PopStyleVar();

	ImDrawList* DrawList = ::ImGui::GetWindowDrawList();
	ImVec2 Min = ::ImGui::GetItemRectMin();
	ImVec2 Max = ::ImGui::GetItemRectMax();

	Min.x += 1;
	DrawList->AddRectFilled(
		Min,
		ImVec2(Min.x + StripeWidth, Max.y),
		::ImGui::ColorConvertFloat4ToU32(BorderColor),
		Rounding,
		ImDrawFlags_RoundCornersLeft
	);

	::ImGui::PopID();
	return Changed;
}

XREUI_API bool XRay::ImGui::ToggleButton(const char* Label, bool* Flags, const ImVec2& Size)
{
	bool Enabled = *Flags;
	bool Changed = false;

	const ImVec4 EnabledColor = GetEditorColor(EEditorColors::ToggleColorActive);
	const ImVec4 DisabledColor = GetEditorColor(EEditorColors::ButtonTint);
	const ImVec4 BorderColor = EnabledColor;

	constexpr float StripeWidth = 3.0f;
	constexpr float Rounding = 5.0f;

	::ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, Rounding);
	::ImGui::PushStyleColor(ImGuiCol_Button, Enabled ? EnabledColor : DisabledColor);
	::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, Enabled ? EnabledColor : DisabledColor);
	::ImGui::PushStyleColor(ImGuiCol_ButtonActive, Enabled ? EnabledColor : DisabledColor);

	if (::ImGui::Button(Label, Size))
	{
		*Flags = !*Flags;
		Changed = true;
		Enabled = !Enabled;
	}

	::ImGui::PopStyleColor(3);
	::ImGui::PopStyleVar();

	ImDrawList* DrawList = ::ImGui::GetWindowDrawList();
	ImVec2 Min = ::ImGui::GetItemRectMin();
	ImVec2 Max = ::ImGui::GetItemRectMax();

	Min.x += 1;
	DrawList->AddRectFilled(
		Min,
		ImVec2(Min.x + StripeWidth, Max.y),
		::ImGui::ColorConvertFloat4ToU32(BorderColor),
		Rounding,
		ImDrawFlags_RoundCornersLeft
	);

	return Changed;
}

std::array<ImColor, 11>* CurrentTheme = nullptr;

std::array<ImColor, 11> PurpleTheme =
{
	ImColor(63, 71, 101, 255),
	ImColor(29, 129, 136, 255),
	ImColor(0.f, 0.f, 0.f, 0.3f), // ToolbarButtonTint
	ImColor(0.f, 0.f, 0.f, 0.1f), // ToolbarTint
	ImColor(0.f, 0.f, 0.f, 0.8f), // BackgroundTint
	ImColor(0.f, 0.f, 0.f, 0.5f), // TableTint
	ImColor(0.f, 0.f, 0.f, 0.5f), // TabBarTint
	ImColor(0.f, 0.f, 0.f, 0.3f), // PanelTint
	ImColor(0.f, 0.f, 0.f, 0.f),  // ButtonTint
	ImColor(1.f, 1.f, 1.f, 0.6f), // ContentIconTint
	ImColor(0.f, 0.f, 0.f, 0.8f)  // PanelBackgroundTint
};

std::array<ImColor, 11> DarkTheme =
{
	ImColor(51, 51, 51, 255),
	ImColor(121, 113, 189, 255),
	ImColor(0.f, 0.f, 0.f, 0.0f), //  ToolbarButtonTint
	ImColor(0.f, 0.f, 0.f, 0.1f), //  ToolbarTint
	ImColor(0.f, 0.f, 0.f, 0.8f), //  BackgroundTint
	ImColor(0.f, 0.f, 0.f, 0.6f), //  TableTint
	ImColor(0.f, 0.f, 0.f, 0.4f), //  TabBarTint
	ImColor(0.f, 0.f, 0.f, 0.3f), //  PanelTint
	ImColor(0.f, 0.f, 0.f, 0.f),  //  ButtonTint
	ImColor(1.f, 1.f, 1.f, 0.6f), //  ContentIconTint
	ImColor(0.f, 0.f, 0.f, 0.8f)  // PanelBackgroundTint
};

ImColor AlphaBlend(const ImColor& Base, const ImColor& Tint)
{
	const float Alpha = Tint.Value.w;

	ImVec4 Out;
	Out.x = Base.Value.x * (1.0f - Alpha) + Tint.Value.x * Alpha;
	Out.y = Base.Value.y * (1.0f - Alpha) + Tint.Value.y * Alpha;
	Out.z = Base.Value.z * (1.0f - Alpha) + Tint.Value.z * Alpha;
	Out.w = 1.0f;

	return ImColor(Out);
}

XREUI_API void XRay::ImGui::SetupColorsList(int ID)
{
	switch (ID)
	{
		case 0: CurrentTheme = &PurpleTheme; break;
		case 1: CurrentTheme = &DarkTheme; break;
	}

	EditorColors.clear();
}

XREUI_API ImColor XRay::ImGui::GetEditorColor(EEditorColors Color)
{
	if (EditorColors.contains(Color))
	{
		return EditorColors[Color];
	}

	if (CurrentTheme == nullptr)
	{
		CurrentTheme = &PurpleTheme;
	}
	std::array<ImColor, 11>& DefaultTheme = *CurrentTheme;

	ImColor Clr = DefaultTheme[(size_t)EEditorColors::Main];

	switch (Color)
	{
		case EEditorColors::Main:
		{
			EditorColors[Color] = Clr;
			return EditorColors[Color];
		}
		case EEditorColors::ToggleColorActive:
		{
			EditorColors[Color] = DefaultTheme[(size_t)EEditorColors::ToggleColorActive];
			return EditorColors[Color];
		}
		case EEditorColors::ToolbarButtonTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::ToolbarButtonTint]);
			return EditorColors[Color];
		}
		case EEditorColors::BackgroundTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::BackgroundTint]);
			return EditorColors[Color];
		}
		case EEditorColors::TableTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::TableTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ToolbarTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::ToolbarTint]);
			return EditorColors[Color];
		}
		case EEditorColors::TabBarTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::TabBarTint]);
			return EditorColors[Color];
		}
		case EEditorColors::PanelTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::PanelTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ButtonTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::ButtonTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ContentIconTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::ContentIconTint]);
			return EditorColors[Color];
		}
		case EEditorColors::PanelBackgroundTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::PanelBackgroundTint]);
			return EditorColors[Color];
		}
	}
}

XREUI_API bool XRay::ImGui::InputVector3(const char* Label, float V[3], float Step)
{
	bool Changed = false;

	::ImGui::PushID(Label);
	::ImGui::BeginGroup();

	static const ImVec4 AxisColors[3] =
	{
		{0.30f, 0.50f, 0.90f, 1.0f},
		{0.30f, 0.90f, 0.30f, 1.0f},
		{0.90f, 0.30f, 0.30f, 1.0f}
	};

	static const char* Ids[3] = { "##x", "##y", "##z" };
	constexpr float StripeWidth = 2.0f;
	constexpr float Rounding = 4.0f;

	float TotalWidth = ::ImGui::GetContentRegionAvail().x;
	float InputWidth = (TotalWidth) / 3.0f;

	for (int i = 0; i < 3; ++i)
	{
		if (i > 0)
		{
			::ImGui::SameLine(0.0f, 0.0f);
		}

		::ImGui::SetNextItemWidth(InputWidth);
		::ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(4, 4));

		Changed |= ::ImGui::DragFloat(Ids[i], &V[i], Step, 0, 0, "%.3f");

		::ImGui::PopStyleVar();

		ImDrawList* DrawList = ::ImGui::GetWindowDrawList();
		ImVec2 Min = ::ImGui::GetItemRectMin();
		ImVec2 Max = ::ImGui::GetItemRectMax();

		DrawList->AddRectFilled
		(
			Min,
			ImVec2(Min.x + StripeWidth, Max.y),
			::ImGui::ColorConvertFloat4ToU32(AxisColors[i]),
			Rounding,
			ImDrawFlags_RoundCornersLeft
		);
	}

	::ImGui::EndGroup();
	::ImGui::PopID();

	return Changed;
}

XREUI_API bool XRay::ImGui::TumblerButton(const char* Label, bool& State, ImVec2 Size)
{
	bool OldState = State;

	const ImVec4 EnabledColor = GetEditorColor(EEditorColors::ToggleColorActive);
	const ImVec4 DisabledColor = GetEditorColor(EEditorColors::ButtonTint);

	constexpr float StripeWidth = 3.0f;
	constexpr float Rounding = 5.0f;

	::ImGui::PushID(Label);

	::ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, Rounding);

	if (OldState)
	{
		::ImGui::PushStyleColor(ImGuiCol_Button, EnabledColor);
		::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, EnabledColor);
		::ImGui::PushStyleColor(ImGuiCol_ButtonActive, EnabledColor);
	}

	bool Pressed = ::ImGui::Button(Label, Size);
	if (Pressed)
	{
		State = !State;
	}

	if (OldState)
	{
		::ImGui::PopStyleColor(3);
	}
	
	::ImGui::PopStyleVar();

	if (!OldState)
	{
		ImDrawList* DrawList = ::ImGui::GetWindowDrawList();
		ImVec2 Min = ::ImGui::GetItemRectMin();
		ImVec2 Max = ::ImGui::GetItemRectMax();

		Min.x += 1.0f;

		DrawList->AddRectFilled
		(
			Min,
			ImVec2(Min.x + StripeWidth, Max.y),
			::ImGui::ColorConvertFloat4ToU32(EnabledColor),
			Rounding,
			ImDrawFlags_RoundCornersLeft
		);
	}

	::ImGui::PopID();
	return Pressed;
}

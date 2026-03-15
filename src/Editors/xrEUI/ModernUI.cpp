#include "stdafx.h"
#include "ModernUI.h"

static xr_hash_map<XRay::ImGui::EEditorColors, ImColor> EditorColors;
static xr_hash_map<XRay::ImGui::EEditorSizes, float> EditorSizes;

std::array<ImColor, 22>* CurrentTheme = nullptr;

std::array<ImColor, 22> PurpleTheme =
{
	ImColor(63, 71, 101, 255),	  // Base Color
	ImColor(29, 129, 136, 255),	  // Accent Color
	ImColor(0.f, 0.f, 0.f, 0.3f), // ToolbarButtonTint
	ImColor(0.f, 0.f, 0.f, 0.8f), // BackgroundTint
	ImColor(0.f, 0.f, 0.f, 0.5f), // TableTint
	ImColor(0.f, 0.f, 0.f, 0.5f), // TabBarTint
	ImColor(0.f, 0.f, 0.f, 0.3f), // PanelTint
	ImColor(0.f, 0.f, 0.f, 0.1f), // PanelBorderTint
	ImColor(0.f, 0.f, 0.f, 0.f),  // ButtonTint
	ImColor(1.f, 1.f, 1.f, 0.1f), // ButtonBorderTint
	ImColor(1.f, 1.f, 1.f, 0.6f), // ContentIconTint
	ImColor(0.f, 0.f, 0.f, 0.8f), // PanelBackgroundTint
	ImColor(1.f, 1.f, 1.f, 0.1f), // HoverTint
	ImColor(1.f, 1.f, 1.f, 0.05f) // ActiveTint
};

std::array<ImColor, 22> DarkTheme =
{
	ImColor(51, 51, 51, 255),	  // Base Color
	ImColor(121, 113, 189, 255),  // Accent Color
	ImColor(0.f, 0.f, 0.f, 0.0f), //  ToolbarButtonTint
	ImColor(0.f, 0.f, 0.f, 0.8f), //  BackgroundTint
	ImColor(0.f, 0.f, 0.f, 0.6f), //  TableTint
	ImColor(0.f, 0.f, 0.f, 0.4f), //  TabBarTint
	ImColor(0.f, 0.f, 0.f, 0.3f), //  PanelTint
	ImColor(1.f, 1.f, 1.f, 0.1f), // PanelBorderTint
	ImColor(0.f, 0.f, 0.f, 0.f),  //  ButtonTint
	ImColor(1.f, 1.f, 1.f, 0.1f), // ButtonBorderTint
	ImColor(1.f, 1.f, 1.f, 0.6f), //  ContentIconTint
	ImColor(0.f, 0.f, 0.f, 0.8f), // PanelBackgroundTint
	ImColor(1.f, 1.f, 1.f, 0.1f), // HoverTint
	ImColor(1.f, 1.f, 1.f, 0.05f) // ActiveTint
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
	std::array<ImColor, 22>& DefaultTheme = *CurrentTheme;

	ImColor Clr = DefaultTheme[(size_t)EEditorColors::Main];

	switch (Color)
	{
		case EEditorColors::Main:
		{
			EditorColors[Color] = Clr;
			return EditorColors[Color];
		}
		case EEditorColors::Accent:
		{
			EditorColors[Color] = DefaultTheme[(size_t)EEditorColors::Accent];
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
		case EEditorColors::PanelBorderTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::PanelBorderTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ButtonTint:
		{
			EditorColors[Color] = AlphaBlend(Clr, DefaultTheme[(size_t)EEditorColors::ButtonTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ButtonBorderTint:
		{
			EditorColors[Color] = DefaultTheme[(size_t)EEditorColors::ButtonBorderTint];
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
		case EEditorColors::HoverTint:
		{
			EditorColors[Color] = DefaultTheme[(size_t)EEditorColors::HoverTint];
			return EditorColors[Color];
		}
		case EEditorColors::ActiveTint:
		{
			EditorColors[Color] = DefaultTheme[(size_t)EEditorColors::ActiveTint];
			return EditorColors[Color];
		}
		case EEditorColors::ButtonHover:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::ButtonTint], DefaultTheme[(size_t)EEditorColors::HoverTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ButtonActive:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::ButtonTint], DefaultTheme[(size_t)EEditorColors::ActiveTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ToggleHover:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::Accent], DefaultTheme[(size_t)EEditorColors::HoverTint]);
			return EditorColors[Color];
		}
		case EEditorColors::ToggleActive:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::Accent], DefaultTheme[(size_t)EEditorColors::ActiveTint]);
			return EditorColors[Color];
		}
		case EEditorColors::TabHover:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::PanelBorderTint], DefaultTheme[(size_t)EEditorColors::HoverTint]);
			return EditorColors[Color];
		}
		case EEditorColors::TabActive:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::PanelBorderTint], DefaultTheme[(size_t)EEditorColors::ActiveTint]);
			return EditorColors[Color];
		}
		case EEditorColors::TableHover:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::TableTint], DefaultTheme[(size_t)EEditorColors::HoverTint]);
			return EditorColors[Color];
		}
		case EEditorColors::TableActive:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::TableTint], DefaultTheme[(size_t)EEditorColors::ActiveTint]);
			return EditorColors[Color];
		}
	}
}

std::array<float, 13> Sizes =
{
    16.f,								// FontSize
    4.0f,								// DockingGap
    2.0f,								// WindowPadding
    4.0f,								// PanelPadding
	26.0f, 								// ButtonSize
	1.0f, 								// ButtonBorderSize
	4.0f, 								// ButtonRadius
	8.0f, 								// ButtonPaddingW
	4.0f, 								// ButtonPaddingH
	4.0f,								// IndicatorWidth
	22.0f,								// TableRowHeight
	2.0f,								// TableBorder
	4.0f								// ToolbarPadding
};
//ImGui::GetFontSize();
XREUI_API float XRay::ImGui::GetEditorSize(EEditorSizes Size)
{
    if (EditorSizes.contains(Size))
	{
		return EditorSizes[Size];
	}

	switch(Size)
	{
		case EEditorSizes::ButtonTextPaddingY:
		{
			return EditorSizes[Size] = (Sizes[static_cast<std::size_t>(ButtonSize)] - Sizes[static_cast<std::size_t>(FontSize)]) / 2;
		}
		case EEditorSizes::TableTextPaddingY:
		{
			return EditorSizes[Size] = (Sizes[static_cast<std::size_t>(TableRowHeight)] - Sizes[static_cast<std::size_t>(FontSize)]) / 2;
		}
		default:
		{
			return EditorSizes[Size] = Sizes[static_cast<std::size_t>(Size)];
		}
    }
}

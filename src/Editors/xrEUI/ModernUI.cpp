#include "stdafx.h"
#include "ModernUI.h"

static xr_hash_map<XRay::ImGui::EEditorColors, ImColor> EditorColors;
static xr_hash_map<XRay::ImGui::EEditorSizes, float> EditorSizes;

std::array<ImColor, 26>* CurrentTheme = nullptr;

std::array<ImColor, 26> DarkBlueTheme =
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

std::array<ImColor, DarkBlueTheme.size()> VSTheme =
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

std::array<ImColor, DarkBlueTheme.size()> DarkOrangeTheme =
{
	ImColor(77, 77, 77, 255),	   // Base Color
	ImColor(199, 112, 26, 255),    // Accent Color
	ImColor(0.f, 0.f, 0.f, 0.0f),  // ToolbarButtonTint
	ImColor(0.f, 0.f, 0.f, 0.6f),  // BackgroundTint
	ImColor(0.f, 0.f, 0.f, 0.5f),  // TableTint
	ImColor(0.f, 0.f, 0.f, 0.45f), // TabBarTint
	ImColor(0.f, 0.f, 0.f, 0.2f),  // PanelTint
	ImColor(0.f, 0.f, 0.f, 0.1f),  // PanelBorderTint
	ImColor(1.f, 1.f, 1.f, 0.f),   // ButtonTint
	ImColor(1.f, 1.f, 1.f, 0.1f),  // ButtonBorderTint
	ImColor(1.f, 1.f, 1.f, 0.6f),  // ContentIconTint
	ImColor(0.f, 0.f, 0.f, 0.6f),  // PanelBackgroundTint
	ImColor(1.f, 1.f, 1.f, 0.16f), // HoverTint
	ImColor(1.f, 1.f, 1.f, 0.08f)  // ActiveTint
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
		case 0: CurrentTheme = &DarkBlueTheme; break;
		case 1: CurrentTheme = &VSTheme; break;
		case 2: CurrentTheme = &DarkOrangeTheme; break;
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
		CurrentTheme = &DarkBlueTheme;
	}
	std::array<ImColor, DarkBlueTheme.size()>& DefaultTheme = *CurrentTheme;

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
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::ButtonTint], ImVec4(ImVec4(EditorColors[EEditorColors::Accent]).x,
																							 ImVec4(EditorColors[EEditorColors::Accent]).y,
																							 ImVec4(EditorColors[EEditorColors::Accent]).z,
																							 ImVec4(DefaultTheme[(size_t)EEditorColors::HoverTint]).w * 2.f
																							 ));
			return EditorColors[Color];
		}
		case EEditorColors::ToggleActive:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::ButtonTint], ImVec4(ImVec4(EditorColors[EEditorColors::Accent]).x,
																							 ImVec4(EditorColors[EEditorColors::Accent]).y,
																							 ImVec4(EditorColors[EEditorColors::Accent]).z,
																							 ImVec4(DefaultTheme[(size_t)EEditorColors::ActiveTint]).w * 2.f
																							 ));
			return EditorColors[Color];
		}
		case EEditorColors::ToolbarToggleHover:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::ToolbarButtonTint], ImVec4(ImVec4(EditorColors[EEditorColors::Accent]).x,
																							 ImVec4(EditorColors[EEditorColors::Accent]).y,
																							 ImVec4(EditorColors[EEditorColors::Accent]).z,
																							 ImVec4(DefaultTheme[(size_t)EEditorColors::HoverTint]).w * 4.f
																							 ));
			return EditorColors[Color];
		}
		case EEditorColors::ToolbarToggleActive:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::ToolbarButtonTint], ImVec4(ImVec4(EditorColors[EEditorColors::Accent]).x,
																							 ImVec4(EditorColors[EEditorColors::Accent]).y,
																							 ImVec4(EditorColors[EEditorColors::Accent]).z,
																							 ImVec4(DefaultTheme[(size_t)EEditorColors::ActiveTint]).w * 4.f
																							 ));
			return EditorColors[Color];
		}
		case EEditorColors::AccentHover:
		{
			EditorColors[Color] = AlphaBlend(EditorColors[EEditorColors::Accent], DefaultTheme[(size_t)EEditorColors::HoverTint]);
			return EditorColors[Color];
		}
		case EEditorColors::AccentActive:
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

	return Clr;
}

const size_t SizeCount = 17;
std::array<float, SizeCount> DefaultSizes =
{
    16.f,								// FontSize
	20.f,								// IconSize
    4.0f,								// DockingGap
    2.0f,								// WindowPadding
    4.0f,								// PanelPadding
	26.0f, 								// ButtonSize
	1.0f, 								// ButtonBorderSize
	4.0f, 								// ButtonRadius
	8.0f, 								// ButtonPaddingW
	4.0f, 								// ButtonPaddingH
	14.0f,								// CheckboxSize
	4.0f,								// IndicatorWidth
	22.0f,								// TableRowHeight
	2.0f,								// TableBorder
	2.0f								// ToolbarPadding
};
std::array<float, SizeCount> TitySizes =
{
    16.f,								// FontSize
	20.f,								// IconSize
	4.0f,								// DockingGap
	2.0f,								// WindowPadding
    4.0f,								// PanelPadding
	26.0f, 								// ButtonSize
	1.0f, 								// ButtonBorderSize
	4.0f, 								// ButtonRadius
	8.0f, 								// ButtonPaddingW
	4.0f, 								// ButtonPaddingH
	14.0f,								// CheckboxSize
	4.0f,								// IndicatorWidth
	22.0f,								// TableRowHeight
	2.0f,								// TableBorder
	4.0f								// ToolbarPadding
};

XREUI_API void XRay::ImGui::InitSizes()
{
	for(size_t i = 0; i < SizeCount; ++i)
	{
		GetEditorSize((EEditorSizes)i);
	}
}

std::array<float, DefaultSizes.size()> *CurrentSizes = nullptr;
XREUI_API void XRay::ImGui::SetupSizesList(int ID)
{
	switch (ID)
	{
		case 0: CurrentSizes = &DefaultSizes; break;
		case 1: CurrentSizes = &TitySizes; break;
	}

}

XREUI_API float XRay::ImGui::GetEditorSize(EEditorSizes Size)
{
	std::array<float, SizeCount>& Sizes = *CurrentSizes;
	float dpiScale = 1.0f;
	if (GUIManager)
		dpiScale = GUIManager->GetScaleDpi();
	
	if (EditorSizes.contains(Size))
	{
		return EditorSizes[Size];
	}

	switch(Size)
	{
		case EEditorSizes::ButtonTextPaddingY:
		{
			return EditorSizes[Size] = (Sizes[static_cast<std::size_t>(ButtonSize)] - Sizes[static_cast<std::size_t>(FontSize)]) / 2 * dpiScale;
		}
		case EEditorSizes::TableTextPaddingY:
		{
			return EditorSizes[Size] = (Sizes[static_cast<std::size_t>(TableRowHeight)] - Sizes[static_cast<std::size_t>(FontSize)]) / 2 * dpiScale;
		}
		default:
		{
			return EditorSizes[Size] = Sizes[static_cast<std::size_t>(Size)] * dpiScale;
		}
    }
}

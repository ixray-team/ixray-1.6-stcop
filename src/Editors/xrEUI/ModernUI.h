#pragma once

namespace XRay::ImGui
{
	enum class EEditorColors
	{
        // Base Colors
		Main,
		Accent,
		// Tints
		ToolbarButtonTint,
		BackgroundTint,
		TableTint,
		TabBarTint,
		PanelTint,
		PanelBorderTint,
		ButtonTint,
		ButtonBorderTint,
		ContentIconTint,
		PanelBackgroundTint,
		HoverTint,
		ActiveTint,
		// Derived Colors
		ButtonHover,
        ButtonActive,
        ToggleHover,
        ToggleActive,
        TabHover,
        TabActive,
	};
	enum EEditorSizes
	{
        FontSize,
		DockingGap,
		WindowPadding,
		PanelPadding,
		ButtonSize,
		ButtonBorderSize,
		ButtonRadius,
		ButtonPaddingW,
		ButtonPaddingH,
		IndicatorWidth,
		TableRowHeight,
		TableBorder,
		ToolbarPadding,
        // Derived Sizes
		ButtonTextPaddingY,
		TableTextPaddingY,
	};

	XREUI_API void SetupColorsList(int ID);
	XREUI_API ImColor GetEditorColor(EEditorColors Color);
	XREUI_API float GetEditorSize(EEditorSizes Size);
	XREUI_API bool InputVector3(const char* Label, float V[3], float Step);
	XREUI_API bool TumblerButton(const char* Label, bool& State, ImVec2 Size = {0, 0});
	XREUI_API bool ToggleFlagButton(const char* Label, uint32_t* Flags, uint32_t Mask, const ImVec2& Size);
	XREUI_API bool ToggleButton(const char* Label, bool* Flags, const ImVec2& Size);
	XREUI_API bool Button(const char* Label, const ImVec2& Size = ImVec2(0, 0));
}
ImColor AlphaBlend(const ImColor& Base, const ImColor& Tint);
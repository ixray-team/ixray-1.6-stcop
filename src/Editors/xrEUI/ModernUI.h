#pragma once

namespace XRay::ImGui
{
	enum class EEditorColors
	{
		Main,
		ToggleColorActive,

		ToolbarButtonTint,
		ToolbarTint,
		BackgroundTint,
		TableTint,
		TabBarTint,
		PanelTint,
		ButtonTint,
		ButtonBorderTint,
		ContentIconTint,
		PanelBackgroundTint,
		HoverTint,
		ActiveTint,
	};
	enum EEditorSizes
	{
		ButtonSize,
		ButtonBorderSize,
		ButtonRadius,
		ButtonPaddingW,
		ButtonPaddingH,
		TextFieldPadding,
		IndicatorWidth,
	};

	XREUI_API void SetupColorsList(int ID);
	XREUI_API ImColor GetEditorColor(EEditorColors Color);
	XREUI_API float GetEditorSize(EEditorSizes Size);
	XREUI_API bool InputVector3(const char* Label, float V[3], float Step);
	XREUI_API bool TumblerButton(const char* Label, bool& State, ImVec2 Size = {0, 0});
	XREUI_API bool ToggleFlagButton(const char* Label, uint32_t* Flags, uint32_t Mask, const ImVec2& Size);
	XREUI_API bool ToggleButton(const char* Label, bool* Flags, const ImVec2& Size);
}
ImColor AlphaBlend(const ImColor& Base, const ImColor& Tint);
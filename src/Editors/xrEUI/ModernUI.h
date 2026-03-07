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

	XREUI_API	void			SetupColorsList(int ID);
	XREUI_API	ImColor			GetEditorColor(EEditorColors Color);
	XREUI_API	float			GetEditorSize(EEditorSizes Size);

	XREUI_API	bool			InputVector3(const char* Label, float V[3], float Step);

	// This button is made to be reused in toggles, so it has an optional bool* Toggle
	// Needs to reduce the code copypaste 
    XREUI_API	bool			BeginDarkChild(const char* str_id, const ImVec2& size = ImVec2(0, 0), ImGuiChildFlags child_flags = 0, ImGuiWindowFlags window_flags = 0);
	XREUI_API	void            EndDarkChild();

	XREUI_API	bool			Button(
			const	char*			label,
			const	ImVec2&			size			= ImVec2(0, 0),
					bool*			toggle			= nullptr,
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll
	);
	XREUI_API	bool			IconButton(
			const	char*			id,
					ImTextureRef	texture,
			const	ImVec2&			button_size		= ImVec2(GetEditorSize(EEditorSizes::ButtonBorderSize), GetEditorSize(EEditorSizes::ButtonBorderSize)),
			const	ImVec2&			image_size		= ImVec2(20, 20),
					bool*			toggle			= nullptr,
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll
	);
	XREUI_API	bool			ButtonBackground(
			const   char*			id,
					bool*			toggle			= nullptr,
			const   ImVec2&			size			= { -1, GetEditorSize(EEditorSizes::ButtonSize) },
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll
	);

	XREUI_API	bool			ToggleButton(const char* label, bool* flags, const ImVec2& size);
	   inline	bool			ToggleButton(const char* label, bool& state, const ImVec2& size) { return ToggleButton(label, &state, size); }
	XREUI_API	bool			ToggleFlagButton(const char* label, uint32_t* flags, uint32_t mask, const ImVec2& size);
	XREUI_API	void			Separator(float thickness = 2.0f);

	XREUI_API	bool			ToolbarIconButton(
			const	char*			id,
					ImTextureRef	texture,
					bool*			toggle			= nullptr,
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll,
					float			rounding		= GetEditorSize(EEditorSizes::ButtonRadius),
					ImVec2			button_size		= { GetEditorSize(EEditorSizes::ButtonSize), GetEditorSize(EEditorSizes::ButtonSize) },
					ImVec2			image_size		= { 20.0f, 20.0f });

	XREUI_API	bool			ToolbarButton(
			const	char*			id,
			const	char*			label,
					bool*			toggle			= nullptr,
					ImVec2			size			= ImVec2(-1, GetEditorSize(EEditorSizes::ButtonSize)),
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll,
					float			rounding		= GetEditorSize(EEditorSizes::ButtonRadius));

	XREUI_API	bool			ToolbarButtonBackground(
			const	char*			id,
					bool*			toggle			= nullptr,
					ImVec2			size			= ImVec2(-1, GetEditorSize(EEditorSizes::ButtonSize)),
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll,
					float			rounding		= GetEditorSize(EEditorSizes::ButtonRadius));

	XREUI_API	bool			TreeNode(const char* label);
	XREUI_API	bool			TreeNode(const char* str_id, const char* fmt, ...) IM_FMTARGS(2);	// helper variation to easily decorelate the id from the displayed string. Read the FAQ about why and how to use ID. to align arbitrary text at the same level as a TreeNode() you can use Bullet().
	XREUI_API	bool			TreeNodeEx(const char* label, ImGuiTreeNodeFlags flags);
	XREUI_API	bool			TreeNodeEx(const char* str_id, ImGuiTreeNodeFlags flags, const char* fmt, ...) IM_FMTARGS(3);
	XREUI_API	bool			TreeNodeEx(const void* ptr_id, ImGuiTreeNodeFlags flags, const char* fmt, ...) IM_FMTARGS(3);
	XREUI_API	bool			TreeNodeExV(const char* str_id, ImGuiTreeNodeFlags flags, const char* fmt, va_list args);
	XREUI_API	bool			TreeNodeExV(const void* ptr_id, ImGuiTreeNodeFlags flags, const char* fmt, va_list args) IM_FMTLIST(3);
	XREUI_API	bool			CollapsingHeader(const char* label, ImGuiTreeNodeFlags flags = 0);


}
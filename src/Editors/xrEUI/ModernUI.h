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
		ToolbarToggleHover,
		ToolbarToggleActive,
		AccentHover,
		AccentActive,
        TabHover,
        TabActive,
		TableHover,
		TableActive
	};
	enum EEditorSizes
	{
        FontSize,
		IconSize,
		DockingGap,
		WindowPadding,
		PanelPadding,
		ButtonSize,
		ButtonBorderSize,
		ButtonRadius,
		ButtonPaddingW,
		ButtonPaddingH,
		CheckboxSize,
		IndicatorWidth,
		TableRowHeight,
		TableBorder,
		ToolbarPadding,
        // Derived Sizes
		ButtonTextPaddingY,
		TableTextPaddingY,
	};

	XREUI_API	void			SetupColorsList(int ID);
	XREUI_API	void			SetupSizesList(int ID);
	XREUI_API	void			InitSizes();

	XREUI_API	ImColor			GetEditorColor(EEditorColors Color);
	XREUI_API	float			GetEditorSize(EEditorSizes Size);

	XREUI_API	void			SameLine(float offset_from_start_x = 0.0f, float spacing = -1.0f);  // call between widgets or groups to layout them horizontally. X position given in window coordinates.

	XREUI_API	bool			InputVector3(const char* Label, float V[3], float Step);

	// This button is made to be reused in toggles, so it has an optional bool* Toggle
	// Needs to reduce the code copypaste 
    XREUI_API	bool			BeginDarkChild(const char* str_id, const ImVec2& size = ImVec2(0, 0), ImGuiChildFlags child_flags = 0, ImGuiWindowFlags window_flags = 0);
	XREUI_API	void            EndDarkChild();

	XREUI_API	bool			BeginTable(const char* str_id, int columns, ImGuiTableFlags flags = 0, const ImVec2& outer_size = ImVec2(0.0f, 0.0f), float inner_width = 0.0f);
	XREUI_API	void			EndTable();
	XREUI_API	void			TableNextColumn();
	XREUI_API	void			TableNextRow(ImGuiTableRowFlags row_flags = 0, float min_row_height = GetEditorSize(EEditorSizes::TableRowHeight));
	XREUI_API	void			TextFramed(const char* label, const ImVec2 size = { 0, 0 }, const ImVec2 text_align = { 0.f, 0.5f }, const bool draw_background = true, ...);
	XREUI_API	void			TextFramedV(const char* fmt, const ImVec2 size, const bool draw_background, const ImVec2 text_align, va_list args);
	XREUI_API	void			TextFramedEx(const char* label, const char* text_end = NULL, const ImVec2 size = { 0, 0 }, const bool draw_background = false, const ImVec2 text_align = { 0.f, 0.5f });

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
			const	ImVec2&			image_size		= ImVec2(GetEditorSize(EEditorSizes::IconSize), GetEditorSize(EEditorSizes::IconSize)),
					bool*			toggle			= nullptr,
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll
	);
	XREUI_API	bool			ButtonBackground(
			const   char*			id,
					bool*			toggle			= nullptr,
			const   ImVec2&			size			= { -1, GetEditorSize(EEditorSizes::ButtonSize) },
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll
	);

	XREUI_API	bool			ToggleButton(const char* label, bool* flags, const ImVec2& size = { 0, 0 });
	   inline	bool			ToggleButton(const char* label, bool& state, const ImVec2& size = { 0, 0 }) { return ToggleButton(label, &state, size); }
	XREUI_API	bool			ToggleFlagButton(const char* label, uint32_t* flags, uint32_t mask, const ImVec2& size);
	XREUI_API	void			Separator(float thickness = 2.0f);

	XREUI_API	bool			ToolbarIconButton(
			const	char*			id,
					ImTextureRef	texture,
					bool*			toggle			= nullptr,
					ImDrawFlags		rounding_flags	= ImDrawFlags_RoundCornersAll,
					float			rounding		= GetEditorSize(EEditorSizes::ButtonRadius),
					ImVec2			button_size		= { GetEditorSize(EEditorSizes::ButtonSize), GetEditorSize(EEditorSizes::ButtonSize) },
					ImVec2			image_size		= ImVec2(GetEditorSize(EEditorSizes::IconSize), GetEditorSize(EEditorSizes::IconSize)));

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
	XREUI_API	bool			BeginExpand(const char* label, ImGuiTreeNodeFlags flags = 0);
	XREUI_API	void			EndExpand();


}
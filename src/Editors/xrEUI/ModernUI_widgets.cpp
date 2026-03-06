#include "stdafx.h"
#include "ModernUI.h"
#include <imgui_internal.h>
#include "IconsFontAwesome7.h"


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

	static  const   char* Ids[3]    = { "##x", "##y", "##z" };
	        float   StripeWidth     = GetEditorSize(EEditorSizes::IndicatorWidth);
	        float   Rounding        = GetEditorSize(EEditorSizes::ButtonRadius);

	        float   TotalWidth      = ::ImGui::GetContentRegionAvail().x;
	        float   InputWidth      = (TotalWidth) / 3.0f;

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

		ImDrawList*     DrawList    = ::ImGui::GetWindowDrawList();
		ImVec2          Min         = ::ImGui::GetItemRectMin();
		ImVec2          Max         = ::ImGui::GetItemRectMax();

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

XREUI_API bool XRay::ImGui::Button(const char* Label, const ImVec2& Size, bool* Toggle)
{
    // --- Styling ---
    const ImVec4 EnabledColor = GetEditorColor(EEditorColors::Accent);
    const ImVec4 EnabledHover = GetEditorColor(EEditorColors::ToggleHover);
    const ImVec4 EnabledActive = GetEditorColor(EEditorColors::ToggleActive);
    const ImVec4 DisabledColor = GetEditorColor(EEditorColors::ButtonTint);
    const ImVec4 DisabledHover = GetEditorColor(EEditorColors::ButtonHover);
    const ImVec4 DisabledActive = GetEditorColor(EEditorColors::ButtonActive);
    const ImVec4 BorderColor = GetEditorColor(EEditorColors::ButtonBorderTint);
    const float  BorderSize = GetEditorSize(EEditorSizes::ButtonBorderSize);

    if (Toggle) {
        ::ImGui::PushStyleColor(ImGuiCol_Button, *Toggle ? EnabledColor : DisabledColor);
        ::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, *Toggle ? EnabledHover : DisabledHover);
        ::ImGui::PushStyleColor(ImGuiCol_ButtonActive, *Toggle ? EnabledActive : DisabledActive);
    }
    else {
        ::ImGui::PushStyleColor(ImGuiCol_Button, DisabledColor);
        ::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, DisabledHover);
        ::ImGui::PushStyleColor(ImGuiCol_ButtonActive, DisabledActive);
    }
    ::ImGui::PushStyleColor(ImGuiCol_Border, BorderColor);
    ::ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, BorderSize);

    bool button = ::ImGui::Button(Label, Size);

    ::ImGui::PopStyleVar();
    ::ImGui::PopStyleColor(4);

    return button;
}

XREUI_API void XRay::ImGui::Separator(float thickness)
{
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;
    if (window->SkipItems)
        return;

    ImGuiSeparatorFlags flags = (window->DC.LayoutType == ImGuiLayoutType_Horizontal) ? ImGuiSeparatorFlags_Vertical : ImGuiSeparatorFlags_Horizontal;

    if (window->DC.CurrentColumns)
        flags |= ImGuiSeparatorFlags_SpanAllColumns;

    ::ImGui::SeparatorEx(flags, thickness);
}

XREUI_API bool XRay::ImGui::ToggleButton(const char* Label, bool* Flags, const ImVec2& Size)
{
	if(!Flags) return false;
	bool Enabled = *Flags;
	bool Changed = false;

	::ImGui::PushStyleVar(ImGuiStyleVar_ButtonTextAlign, { 0, 0.5 });
	if (XRay::ImGui::Button(Label, Size, Flags))
	{
		*Flags = !*Flags;
		Changed = true;
		Enabled = !Enabled;
	}
	::ImGui::PopStyleVar();

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

static void RenderArrow(ImDrawList* draw_list, ImVec2 pos, ImU32 col, ImGuiDir dir, float scale)
{
    ImGuiContext& g = *GImGui;
    const   float        fontSize   = g.FontSize;
    const   char*        arrow      = nullptr;
                         pos.x     += fontSize * (1.45 - scale) * 0.5f;
                         pos.y     += fontSize * (1.25 - scale) * 0.5f;
    switch (dir)
    {
    case ImGuiDir_Up:    arrow      = ICON_FA_CARET_UP; break;
    case ImGuiDir_Down:  arrow      = ICON_FA_CARET_DOWN; break;
    case ImGuiDir_Left:  arrow      = ICON_FA_CARET_LEFT; break;
    case ImGuiDir_Right: arrow      = ICON_FA_CARET_RIGHT; break;
    default:             arrow      = ICON_FA_CIRCLE; break;
    }
    ImGui::SetWindowFontScale(scale);
    ::ImGui::RenderText(pos, arrow, NULL);
    ImGui::SetWindowFontScale(1.f);
}

// COPYPASTA FROM imgui_widgets.cpp
static void TreeNodeStoreStackData(ImGuiTreeNodeFlags flags, float x1)
{
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;

    g.TreeNodeStack.resize(g.TreeNodeStack.Size + 1);
    ImGuiTreeNodeStackData* tree_node_data = &g.TreeNodeStack.Data[g.TreeNodeStack.Size - 1];
    tree_node_data->ID = g.LastItemData.ID;
    tree_node_data->TreeFlags = flags;
    tree_node_data->ItemFlags = g.LastItemData.ItemFlags;
    tree_node_data->NavRect = g.LastItemData.NavRect;

    // Initially I tried to latch value for GetColorU32(ImGuiCol_TreeLines) but it's not a good trade-off for very large trees.
    const bool draw_lines = (flags & (ImGuiTreeNodeFlags_DrawLinesFull | ImGuiTreeNodeFlags_DrawLinesToNodes)) != 0;
    tree_node_data->DrawLinesX1 = draw_lines ? (x1 + g.FontSize * 0.5f + g.Style.FramePadding.x) : +FLT_MAX;
    tree_node_data->DrawLinesTableColumn = (draw_lines && g.CurrentTable) ? (ImGuiTableColumnIdx)g.CurrentTable->CurrentColumn : -1;
    tree_node_data->DrawLinesToNodesY2 = -FLT_MAX;
    window->DC.TreeHasStackDataDepthMask |= (1 << window->DC.TreeDepth);
    if (flags & ImGuiTreeNodeFlags_DrawLinesToNodes)
        window->DC.TreeRecordsClippedNodesY2Mask |= (1 << window->DC.TreeDepth);
}

static bool TreeNodeBehavior(ImGuiID id, ImGuiTreeNodeFlags flags, const char* label, const char* label_end)
{
    ImGuiWindow* window = ::ImGui::GetCurrentWindow();
    if (window->SkipItems)
        return false;

    ImGuiContext& g = *GImGui;
    const ImGuiStyle& style = g.Style;
    const bool display_frame = (flags & ImGuiTreeNodeFlags_Framed) != 0;
    const ImVec2 padding = (display_frame || (flags & ImGuiTreeNodeFlags_FramePadding)) ? style.FramePadding : ImVec2(style.FramePadding.x, ImMin(window->DC.CurrLineTextBaseOffset, style.FramePadding.y));
    // <IX-Ray Modified>
    const float spacing = style.ItemSpacing.x; 

    if (!label_end)
        label_end = ::ImGui::FindRenderedTextEnd(label);
    const   ImVec2  label_size      = ::ImGui::CalcTextSize(label, label_end, false);

    const   float   text_offset_x   = padding.x + g.FontSize + spacing;   // Collapsing arrow width + Spacing
    const   float   text_offset_y   = ImMax(padding.y, window->DC.CurrLineTextBaseOffset);            // Latch before ItemSize changes it
    const   float   text_width      = padding.x + g.FontSize + spacing + label_size.x;                         // Include collapsing arrow
    // </IX-Ray Modified>

    // We vertically grow up to current line height up the typical widget height.
    const float frame_height = ImMax(ImMin(window->DC.CurrLineSize.y, g.FontSize + style.FramePadding.y * 2), label_size.y + padding.y * 2);
    const bool span_all_columns = (flags & ImGuiTreeNodeFlags_SpanAllColumns) != 0 && (g.CurrentTable != NULL);
    const bool span_all_columns_label = (flags & ImGuiTreeNodeFlags_LabelSpanAllColumns) != 0 && (g.CurrentTable != NULL);
    ImRect frame_bb;
    frame_bb.Min.x = span_all_columns ? window->ParentWorkRect.Min.x : (flags & ImGuiTreeNodeFlags_SpanFullWidth) ? window->WorkRect.Min.x : window->DC.CursorPos.x;
    frame_bb.Min.y = window->DC.CursorPos.y;
    frame_bb.Max.x = span_all_columns ? window->ParentWorkRect.Max.x : (flags & ImGuiTreeNodeFlags_SpanLabelWidth) ? window->DC.CursorPos.x + text_width + padding.x : window->WorkRect.Max.x;
    frame_bb.Max.y = window->DC.CursorPos.y + frame_height;
    // <IX-Ray Modified>
    // Don't need this outer extends
    /*
    if (display_frame)
    {
        const float outer_extend = IM_TRUNC(window->WindowPadding.x * 0.5f); // Framed header expand a little outside of current limits
        frame_bb.Min.x -= outer_extend;
        frame_bb.Max.x += outer_extend;
    }
    */
    // </IX-Ray Modified>

    ImVec2 text_pos(window->DC.CursorPos.x + text_offset_x, window->DC.CursorPos.y + text_offset_y);
    ::ImGui::ItemSize(ImVec2(text_width, frame_height), padding.y);

    // For regular tree nodes, we arbitrary allow to click past 2 worth of ItemSpacing
    ImRect interact_bb = frame_bb;
    if ((flags & (ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_SpanFullWidth | ImGuiTreeNodeFlags_SpanLabelWidth | ImGuiTreeNodeFlags_SpanAllColumns)) == 0)
        interact_bb.Max.x = frame_bb.Min.x + text_width + (label_size.x > 0.0f ? style.ItemSpacing.x * 2.0f : 0.0f);

    // Compute open and multi-select states before ItemAdd() as it clear NextItem data.
    ImGuiID storage_id = (g.NextItemData.HasFlags & ImGuiNextItemDataFlags_HasStorageID) ? g.NextItemData.StorageId : id;
    bool is_open = ::ImGui::TreeNodeUpdateNextOpen(storage_id, flags);

    bool is_visible;
    if (span_all_columns || span_all_columns_label)
    {
        // Modify ClipRect for the ItemAdd(), faster than doing a PushColumnsBackground/PushTableBackgroundChannel for every Selectable..
        const float backup_clip_rect_min_x = window->ClipRect.Min.x;
        const float backup_clip_rect_max_x = window->ClipRect.Max.x;
        window->ClipRect.Min.x = window->ParentWorkRect.Min.x;
        window->ClipRect.Max.x = window->ParentWorkRect.Max.x;
        is_visible = ::ImGui::ItemAdd(interact_bb, id);
        window->ClipRect.Min.x = backup_clip_rect_min_x;
        window->ClipRect.Max.x = backup_clip_rect_max_x;
    }
    else
    {
        is_visible = ::ImGui::ItemAdd(interact_bb, id);
    }
    g.LastItemData.StatusFlags |= ImGuiItemStatusFlags_HasDisplayRect;
    g.LastItemData.DisplayRect = frame_bb;

    // If a NavLeft request is happening and ImGuiTreeNodeFlags_NavLeftJumpsToParent enabled:
    // Store data for the current depth to allow returning to this node from any child item.
    // For this purpose we essentially compare if g.NavIdIsAlive went from 0 to 1 between TreeNode() and TreePop().
    // It will become tempting to enable ImGuiTreeNodeFlags_NavLeftJumpsToParent by default or move it to ImGuiStyle.
    bool store_tree_node_stack_data = false;
    if ((flags & ImGuiTreeNodeFlags_DrawLinesMask_) == 0)
        flags |= g.Style.TreeLinesFlags;
    const bool draw_tree_lines = (flags & (ImGuiTreeNodeFlags_DrawLinesFull | ImGuiTreeNodeFlags_DrawLinesToNodes)) && (frame_bb.Min.y < window->ClipRect.Max.y) && (g.Style.TreeLinesSize > 0.0f);
    if (!(flags & ImGuiTreeNodeFlags_NoTreePushOnOpen))
    {
        store_tree_node_stack_data = draw_tree_lines;
        if ((flags & ImGuiTreeNodeFlags_NavLeftJumpsToParent) && !g.NavIdIsAlive)
            if (g.NavMoveDir == ImGuiDir_Left && g.NavWindow == window && ::ImGui::NavMoveRequestButNoResultYet())
                store_tree_node_stack_data = true;
    }

    const bool is_leaf = (flags & ImGuiTreeNodeFlags_Leaf) != 0;
    if (!is_visible)
    {
        if ((flags & ImGuiTreeNodeFlags_DrawLinesToNodes) && (window->DC.TreeRecordsClippedNodesY2Mask & (1 << (window->DC.TreeDepth - 1))))
        {
            ImGuiTreeNodeStackData* parent_data = &g.TreeNodeStack.Data[g.TreeNodeStack.Size - 1];
            parent_data->DrawLinesToNodesY2 = ImMax(parent_data->DrawLinesToNodesY2, window->DC.CursorPos.y); // Don't need to aim to mid Y position as we are clipped anyway.
            if (frame_bb.Min.y >= window->ClipRect.Max.y)
                window->DC.TreeRecordsClippedNodesY2Mask &= ~(1 << (window->DC.TreeDepth - 1)); // Done
        }
        if (is_open && store_tree_node_stack_data)
            TreeNodeStoreStackData(flags, text_pos.x - text_offset_x); // Call before TreePushOverrideID()
        if (is_open && !(flags & ImGuiTreeNodeFlags_NoTreePushOnOpen))
            ::ImGui::TreePushOverrideID(id);
        IMGUI_TEST_ENGINE_ITEM_INFO(g.LastItemData.ID, label, g.LastItemData.StatusFlags | (is_leaf ? 0 : ImGuiItemStatusFlags_Openable) | (is_open ? ImGuiItemStatusFlags_Opened : 0));
        return is_open;
    }

    if (span_all_columns || span_all_columns_label)
    {
        ::ImGui::TablePushBackgroundChannel();
        g.LastItemData.StatusFlags |= ImGuiItemStatusFlags_HasClipRect;
        g.LastItemData.ClipRect = window->ClipRect;
    }

    ImGuiButtonFlags button_flags = ImGuiTreeNodeFlags_None;
    if ((flags & ImGuiTreeNodeFlags_AllowOverlap) || (g.LastItemData.ItemFlags & ImGuiItemFlags_AllowOverlap))
        button_flags |= ImGuiButtonFlags_AllowOverlap;
    if (!is_leaf)
        button_flags |= ImGuiButtonFlags_PressedOnDragDropHold;

    // We allow clicking on the arrow section with keyboard modifiers held, in order to easily
    // allow browsing a tree while preserving selection with code implementing multi-selection patterns.
    // When clicking on the rest of the tree node we always disallow keyboard modifiers.
    const float arrow_hit_x1 = (text_pos.x - text_offset_x) - style.TouchExtraPadding.x;
    const float arrow_hit_x2 = (text_pos.x - text_offset_x) + (g.FontSize + padding.x * 2.0f) + style.TouchExtraPadding.x;
    const bool is_mouse_x_over_arrow = (g.IO.MousePos.x >= arrow_hit_x1 && g.IO.MousePos.x < arrow_hit_x2);

    const bool is_multi_select = (g.LastItemData.ItemFlags & ImGuiItemFlags_IsMultiSelect) != 0;
    if (is_multi_select) // We absolutely need to distinguish open vs select so _OpenOnArrow comes by default
        flags |= (flags & ImGuiTreeNodeFlags_OpenOnMask_) == 0 ? ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_OpenOnDoubleClick : ImGuiTreeNodeFlags_OpenOnArrow;

    // Open behaviors can be altered with the _OpenOnArrow and _OnOnDoubleClick flags.
    // Some alteration have subtle effects (e.g. toggle on MouseUp vs MouseDown events) due to requirements for multi-selection and drag and drop support.
    // - Single-click on label = Toggle on MouseUp (default, when _OpenOnArrow=0)
    // - Single-click on arrow = Toggle on MouseDown (when _OpenOnArrow=0)
    // - Single-click on arrow = Toggle on MouseDown (when _OpenOnArrow=1)
    // - Double-click on label = Toggle on MouseDoubleClick (when _OpenOnDoubleClick=1)
    // - Double-click on arrow = Toggle on MouseDoubleClick (when _OpenOnDoubleClick=1 and _OpenOnArrow=0)
    // It is rather standard that arrow click react on Down rather than Up.
    // We set ImGuiButtonFlags_PressedOnClickRelease on OpenOnDoubleClick because we want the item to be active on the initial MouseDown in order for drag and drop to work.
    if (is_mouse_x_over_arrow)
        button_flags |= ImGuiButtonFlags_PressedOnClick;
    else if (flags & ImGuiTreeNodeFlags_OpenOnDoubleClick)
        button_flags |= ImGuiButtonFlags_PressedOnClickRelease | ImGuiButtonFlags_PressedOnDoubleClick;
    else
        button_flags |= ImGuiButtonFlags_PressedOnClickRelease;
    if (flags & ImGuiTreeNodeFlags_NoNavFocus)
        button_flags |= ImGuiButtonFlags_NoNavFocus;

    bool selected = (flags & ImGuiTreeNodeFlags_Selected) != 0;
    const bool was_selected = selected;

    // Multi-selection support (header)
    if (is_multi_select)
    {
        // Handle multi-select + alter button flags for it
        ::ImGui::MultiSelectItemHeader(id, &selected, &button_flags);
        if (is_mouse_x_over_arrow)
            button_flags = (button_flags | ImGuiButtonFlags_PressedOnClick) & ~ImGuiButtonFlags_PressedOnClickRelease;
    }
    else
    {
        if (window != g.HoveredWindow || !is_mouse_x_over_arrow)
            button_flags |= ImGuiButtonFlags_NoKeyModsAllowed;
    }

    bool hovered, held;
    bool pressed = ::ImGui::ButtonBehavior(interact_bb, id, &hovered, &held, button_flags);
    bool toggled = false;
    if (!is_leaf)
    {
        if (pressed && g.DragDropHoldJustPressedId != id)
        {
            if ((flags & ImGuiTreeNodeFlags_OpenOnMask_) == 0 || (g.NavActivateId == id && !is_multi_select))
                toggled = true; // Single click
            if (flags & ImGuiTreeNodeFlags_OpenOnArrow)
                toggled |= is_mouse_x_over_arrow && !g.NavHighlightItemUnderNav; // Lightweight equivalent of IsMouseHoveringRect() since ButtonBehavior() already did the job
            if ((flags & ImGuiTreeNodeFlags_OpenOnDoubleClick) && g.IO.MouseClickedCount[0] == 2)
                toggled = true; // Double click
        }
        else if (pressed && g.DragDropHoldJustPressedId == id)
        {
            IM_ASSERT(button_flags & ImGuiButtonFlags_PressedOnDragDropHold);
            if (!is_open) // When using Drag and Drop "hold to open" we keep the node highlighted after opening, but never close it again.
                toggled = true;
            else
                pressed = false; // Cancel press so it doesn't trigger selection.
        }

        if (g.NavId == id && g.NavMoveDir == ImGuiDir_Left && is_open)
        {
            toggled = true;
            ::ImGui::NavClearPreferredPosForAxis(ImGuiAxis_X);
            ::ImGui::NavMoveRequestCancel();
        }
        if (g.NavId == id && g.NavMoveDir == ImGuiDir_Right && !is_open) // If there's something upcoming on the line we may want to give it the priority?
        {
            toggled = true;
            ::ImGui::NavClearPreferredPosForAxis(ImGuiAxis_X);
            ::ImGui::NavMoveRequestCancel();
        }

        if (toggled)
        {
            is_open = !is_open;
            window->DC.StateStorage->SetInt(storage_id, is_open);
            g.LastItemData.StatusFlags |= ImGuiItemStatusFlags_ToggledOpen;
        }
    }

    // Multi-selection support (footer)
    if (is_multi_select)
    {
        bool pressed_copy = pressed && !toggled;
        ::ImGui::MultiSelectItemFooter(id, &selected, &pressed_copy);
        if (pressed)
            ::ImGui::SetNavID(id, window->DC.NavLayerCurrent, g.CurrentFocusScopeId, interact_bb);
    }

    if (selected != was_selected)
        g.LastItemData.StatusFlags |= ImGuiItemStatusFlags_ToggledSelection;

    // Render
    {
        const ImU32 text_col = ::ImGui::GetColorU32(ImGuiCol_Text);
        ImGuiNavRenderCursorFlags nav_render_cursor_flags = ImGuiNavRenderCursorFlags_Compact;
        if (is_multi_select)
            nav_render_cursor_flags |= ImGuiNavRenderCursorFlags_AlwaysDraw; // Always show the nav rectangle
        if (display_frame)
        {
            // Framed type
            const ImU32 bg_col = ::ImGui::GetColorU32((held && hovered) ? ImGuiCol_HeaderActive : hovered ? ImGuiCol_HeaderHovered : ImGuiCol_Header);
            ::ImGui::RenderFrame(frame_bb.Min, frame_bb.Max, bg_col, true, style.FrameRounding);
            ::ImGui::RenderNavCursor(frame_bb, id, nav_render_cursor_flags);
            if (span_all_columns && !span_all_columns_label)
                ::ImGui::TablePopBackgroundChannel();
            if (flags & ImGuiTreeNodeFlags_Bullet)
                ::ImGui::RenderBullet(window->DrawList, ImVec2(text_pos.x - text_offset_x * 0.60f, text_pos.y + g.FontSize * 0.5f), text_col);
            else if (!is_leaf)
// <IX-Ray Modified>
                //::ImGui::RenderArrow(window->DrawList, ImVec2(text_pos.x - text_offset_x + padding.x, text_pos.y), text_col, is_open ? ((flags & ImGuiTreeNodeFlags_UpsideDownArrow) ? ImGuiDir_Up : ImGuiDir_Down) : ImGuiDir_Right, 1.0f);
                RenderArrow(window->DrawList, ImVec2(text_pos.x - text_offset_x + padding.x, text_pos.y), text_col, is_open ? ((flags & ImGuiTreeNodeFlags_UpsideDownArrow) ? ImGuiDir_Up : ImGuiDir_Down) : ImGuiDir_Right, 0.75f);
// </IX-Ray Modified>
            else // Leaf without bullet, left-adjusted text
                text_pos.x -= text_offset_x - padding.x;
            if (flags & ImGuiTreeNodeFlags_ClipLabelForTrailingButton)
                frame_bb.Max.x -= g.FontSize + style.FramePadding.x;
            if (g.LogEnabled)
                ::ImGui::LogSetNextTextDecoration("###", "###");
        }
        else
        {
            // Unframed typed for tree nodes
            if (hovered || selected)
            {
                const ImU32 bg_col = ::ImGui::GetColorU32((held && hovered) ? ImGuiCol_HeaderActive : hovered ? ImGuiCol_HeaderHovered : ImGuiCol_Header);
                ::ImGui::RenderFrame(frame_bb.Min, frame_bb.Max, bg_col, false);
            }
            ::ImGui::RenderNavCursor(frame_bb, id, nav_render_cursor_flags);
            if (span_all_columns && !span_all_columns_label)
                ::ImGui::TablePopBackgroundChannel();
            if (flags & ImGuiTreeNodeFlags_Bullet)
                ::ImGui::RenderBullet(window->DrawList, ImVec2(text_pos.x - text_offset_x * 0.5f, text_pos.y + g.FontSize * 0.5f), text_col);
            else if (!is_leaf)
// <IX-Ray Modified>
                //::ImGui::RenderArrow(window->DrawList, ImVec2(text_pos.x - text_offset_x + padding.x, text_pos.y + g.FontSize * 0.15f), text_col, is_open ? ((flags & ImGuiTreeNodeFlags_UpsideDownArrow) ? ImGuiDir_Up : ImGuiDir_Down) : ImGuiDir_Right, 0.70f);
                RenderArrow(window->DrawList, ImVec2(text_pos.x - text_offset_x + padding.x, text_pos.y), text_col, is_open ? ((flags & ImGuiTreeNodeFlags_UpsideDownArrow) ? ImGuiDir_Up : ImGuiDir_Down) : ImGuiDir_Right, 0.75f);
// </IX-Ray Modified>
            if (g.LogEnabled)
                ::ImGui::LogSetNextTextDecoration(">", NULL);
        }

        if (draw_tree_lines)
            ::ImGui::TreeNodeDrawLineToChildNode(ImVec2(text_pos.x - text_offset_x + padding.x, text_pos.y + g.FontSize * 0.5f));

        // Label
        if (display_frame)
            ::ImGui::RenderTextClipped(text_pos, frame_bb.Max, label, label_end, &label_size);
        else
            ::ImGui::RenderText(text_pos, label, label_end, false);

        if (span_all_columns_label)
            ::ImGui::TablePopBackgroundChannel();
    }

    if (is_open && store_tree_node_stack_data)
        TreeNodeStoreStackData(flags, text_pos.x - text_offset_x); // Call before TreePushOverrideID()
    if (is_open && !(flags & ImGuiTreeNodeFlags_NoTreePushOnOpen))
        ::ImGui::TreePushOverrideID(id); // Could use TreePush(label) but this avoid computing twice

    IMGUI_TEST_ENGINE_ITEM_INFO(id, label, g.LastItemData.StatusFlags | (is_leaf ? 0 : ImGuiItemStatusFlags_Openable) | (is_open ? ImGuiItemStatusFlags_Opened : 0));
    return is_open;
}

XREUI_API bool XRay::ImGui::TreeNode(const char* str_id, const char* fmt, ...)
{
    va_list args;
            va_start(args, fmt);
    bool    is_open     = ::ImGui::TreeNodeExV(str_id, 0, fmt, args);
            va_end(args);
    return  is_open;
}

XREUI_API bool XRay::ImGui::TreeNode(const char* label)
{
    ImGuiWindow*    window  = ::ImGui::GetCurrentWindow();
    if (window->SkipItems)
        return      false;
    ImGuiID         id      = window->GetID(label);
    return          TreeNodeBehavior(id, ImGuiTreeNodeFlags_None, label, NULL);
}

XREUI_API bool XRay::ImGui::TreeNodeEx(const char* label, ImGuiTreeNodeFlags flags)
{
    ImGuiWindow* window = ::ImGui::GetCurrentWindow();
    if (window->SkipItems)
        return false;
    ImGuiID id = window->GetID(label);
    return TreeNodeBehavior(id, flags, label, NULL);
}
XREUI_API bool XRay::ImGui::TreeNodeEx(const char* str_id, ImGuiTreeNodeFlags flags, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    bool is_open = XRay::ImGui::TreeNodeExV(str_id, flags, fmt, args);
    va_end(args);
    return is_open;
}
XREUI_API bool XRay::ImGui::TreeNodeEx(const void* ptr_id, ImGuiTreeNodeFlags flags, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    bool is_open = XRay::ImGui::TreeNodeExV(ptr_id, flags, fmt, args);
    va_end(args);
    return is_open;
}
XREUI_API bool XRay::ImGui::TreeNodeExV(const char* str_id, ImGuiTreeNodeFlags flags, const char* fmt, va_list args)
{
    ImGuiWindow* window = ::ImGui::GetCurrentWindow();
    if (window->SkipItems)
        return false;

    ImGuiID id = window->GetID(str_id);
    const char* label, * label_end;
    ImFormatStringToTempBufferV(&label, &label_end, fmt, args);
    return TreeNodeBehavior(id, flags, label, label_end);
}
XREUI_API bool XRay::ImGui::TreeNodeExV(const void* ptr_id, ImGuiTreeNodeFlags flags, const char* fmt, va_list args)
{
    ImGuiWindow* window = ::ImGui::GetCurrentWindow();
    if (window->SkipItems)
        return false;

    ImGuiID id = window->GetID(ptr_id);
    const char* label, * label_end;
    ImFormatStringToTempBufferV(&label, &label_end, fmt, args);
    return TreeNodeBehavior(id, flags, label, label_end);
}

// Made from scratch, not based on CollapsingHeader(). Needs for Properties Tables, good for other Tree Nodes, Framed by default.
XREUI_API bool XRay::ImGui::CollapsingHeader(const char* label, ImGuiTreeNodeFlags flags)
{
    ImGuiWindow*    window  = ::ImGui::GetCurrentWindow();
    ImGuiID		    id      = window->GetID(label);
                    flags   |= 0
                        | ImGuiTreeNodeFlags_Framed
                        | ImGuiTreeNodeFlags_FramePadding
                        | ImGuiTreeNodeFlags_SpanAllColumns
                        | ImGuiTreeNodeFlags_LabelSpanAllColumns
                        ;

    return TreeNodeBehavior(id, flags, label, NULL);
}
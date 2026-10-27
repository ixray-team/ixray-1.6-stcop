#pragma once
using TOnRenderContextMenu = xr_delegate<void()>;
using TOnRenderToolBar = xr_delegate<void(ImVec2, ImVec2)>;

class ECORE_API                                     UIRenderForm: public IEditorWnd
{
public:
	using DragCallback = void(const xr_string&, int);
	DragCallback* DragFunctor = nullptr;

	// Called every frame a payload hovers the viewport; nullptr path ends the preview
	using DragPreviewCallback = void(const xr_string*, int);
	DragPreviewCallback* DragPreviewFunctor = nullptr;

public:
	UIRenderForm();
	virtual ~UIRenderForm();
	virtual void Draw();

	void DrawVP();

	IC Ivector2 GetMousePos()const { return m_mouse_position; }
	IC const Frect&	GetRect() const { return m_render_pos; }

	IC void		SetContextMenuEvent(TOnRenderContextMenu e) { m_OnContextMenu = e; }
	IC void		SetToolBarEvent(TOnRenderToolBar e) { m_OnToolBar = e; }

	xr_delegate<void()> OnFocusCallback;
	xr_delegate<void()> OnClickCallback;
	int ViewportID = 0;
	string32 ViewportName;

	bool UseHint = false;

private:
	void DrawStatistics();
	void HandleDragDrop(const ImVec2&);
	static int GetDropType(const xr_string& FileName);

	Ivector2	m_mouse_position;
	Frect		m_render_pos;

	TOnRenderToolBar m_OnToolBar;
	TOnRenderContextMenu m_OnContextMenu;
	bool m_mouse_down;
	bool m_mouse_move;
	bool m_shiftstate_down;
	bool DragPreviewActive = false;
};
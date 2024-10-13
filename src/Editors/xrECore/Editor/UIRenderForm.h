#pragma once
using TOnRenderContextMenu = xr_delegate<void()>;
using TOnRenderToolBar = xr_delegate<void(ImVec2, ImVec2)>;

class ECORE_API                                     UIRenderForm: public IEditorWnd
{
public:
	using DragCallback = void(const xr_string&, int);
	DragCallback* DragFunctor;

public:
	UIRenderForm();
	virtual ~UIRenderForm();
	virtual void Draw();

	IC Ivector2 GetMousePos()const { return m_mouse_position; }
	IC const Frect&	GetRect() const { return m_render_pos; }

	IC void		SetContextMenuEvent(TOnRenderContextMenu e) { m_OnContextMenu = e; }
	IC void		SetToolBarEvent(TOnRenderToolBar e) { m_OnToolBar = e; }

	xr_delegate<void()> OnFocusCallback;
	int ViewportID = 0;
	string32 ViewportName;
	int DockId = -1;
private:
	void DrawStatistics();
	Ivector2	m_mouse_position;
	Frect		m_render_pos;

	TOnRenderToolBar m_OnToolBar;
	TOnRenderContextMenu m_OnContextMenu;
	bool m_mouse_down;
	bool m_mouse_move;
	bool m_shiftstate_down;
};
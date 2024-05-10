#pragma once
typedef fastdelegate::FastDelegate0<>		  					TOnRenderContextMenu;
typedef fastdelegate::FastDelegate1<ImVec2>		  				TOnRenderToolBar;
class ECORE_API UIRenderForm :public XrUI
{
public:
	UIRenderForm();
	virtual ~UIRenderForm();
	virtual void Draw();
	IC Ivector2 GetMousePos()const { return m_mouse_position; }
	IC void		SetContextMenuEvent(TOnRenderContextMenu e) { m_OnContextMenu = e; }
	IC void		SetToolBarEvent(TOnRenderToolBar e) { m_OnToolBar = e; }
private:
	Ivector2	m_mouse_position;
	ref_texture m_rt;
	TOnRenderToolBar m_OnToolBar;
	TOnRenderContextMenu m_OnContextMenu;
	bool m_mouse_down;
	bool m_mouse_move;
	bool m_shiftstate_down;
};
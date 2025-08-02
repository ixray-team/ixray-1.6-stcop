#pragma once
#include "UIFrameWindow.h"

class CUIStatic;
class CUIFrameLineWnd;

class UI_API CUIButtonHint :
	public CUIFrameWindow
{
	CUIWindow*			m_ownerWnd;

	CUIStatic*			m_text;
	CUIFrameLineWnd*	m_border;

	bool				m_enabledOnFrame;
public:
					CUIButtonHint	();
	virtual			~CUIButtonHint	();
	CUIWindow*		Owner			()	{return m_ownerWnd;}
	void			Discard			()	{m_ownerWnd=NULL;};
	void			OnRender		();
	void			Draw_			()	{m_enabledOnFrame = true;};
	void			SetHintText		(CUIWindow* w, LPCSTR text);

	virtual CUIWindow* ui_cast_window() { return this; }

};

extern UI_API CUIButtonHint* g_btnHint; 
extern UI_API CUIButtonHint* g_statHint;

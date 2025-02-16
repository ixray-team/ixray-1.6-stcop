#pragma once
#include "UIWindow.h"
#include "UIFrameLineWnd.h"

class CUIStatic;

class CUIButtonHint :public CUIWindow, public pureRender
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
};

extern CUIButtonHint* g_btnHint; 
extern CUIButtonHint* g_statHint;
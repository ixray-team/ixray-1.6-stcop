#pragma once
#include "UIFrameLineWnd.h"

class UI_API CUIScrollBox :
	public CUIFrameLineWnd
{
	typedef	CUIFrameLineWnd		inherited;
public:
								CUIScrollBox			();

	virtual bool				OnMouseAction					(float x, float y, EUIMessages mouse_action);

	virtual CUIWindow* ui_cast_window() { return this; }
};

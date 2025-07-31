#pragma once
#include "../../xrUI/Widgets/UIStatic.h"

class CUIPdaMsgListItem : 
	public CUIColorAnimConrollerContainer
{
	typedef	CUIColorAnimConrollerContainer	inherited;
public:
			void		InitPdaMsgListItem				(const Fvector2& size);
	virtual void		SetFont							(CGameFont* pFont);
	
	virtual CUIWindow* ui_cast_window() { return this; }

	CUIStatic			UIIcon;
	CUITextWnd			UITimeText;
	CUITextWnd			UICaptionText;
	CUITextWnd			UIMsgText;
};
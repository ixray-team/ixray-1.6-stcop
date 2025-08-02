#pragma once
#include "../../xrUI/Widgets/UIStatic.h"

class CUIPdaMsgListItem final :
	public CUIColorAnimConrollerContainer
{
	typedef	CUIColorAnimConrollerContainer	inherited;
public:
			void		InitPdaMsgListItem				(const Fvector2& size);
	virtual void		SetFont							(CGameFont* pFont);
	
	virtual CUIWindow* ui_cast_window() { return this; }

	CUIStatic			UIIcon;
	CUIStatic			UITimeText;
	CUIStatic*			UICaptionText;
	CUIStatic			UIMsgText;
};
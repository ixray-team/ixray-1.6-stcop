#pragma once
#include "../../xrUI/Widgets/UIStatic.h"

class CUIPdaMsgListItem final :
	public CUIColorAnimConrollerContainer
{
	typedef	CUIColorAnimConrollerContainer	inherited;
public:
			void		InitPdaMsgListItem				(const Fvector2& size);
	virtual void		SetFont							(CGameFont* pFont) override;

	virtual CUIWindow* ui_cast_window() override { return this; }

	CUIStatic			UIIcon;
	CUIStatic			UITimeText;
	CUIStatic*			UICaptionText;
	CUIStatic			UIMsgText;
};
#pragma once
#include "../../xrUI/Widgets/UIDialogWnd.h"

class CUIScrollView;


class CUISpeechMenu final : public CUIDialogWnd
{
public:
					CUISpeechMenu	(const char* section_name);
	virtual ~CUISpeechMenu	() override;
			void	InitList		(const char* section_name);
	virtual bool	NeedCursor		()const override {return  false;}
	virtual bool	OnKeyboardAction		(int dik, EUIMessages keyboard_action) override;
	virtual bool	StopAnyMove		() override {return false;}
	virtual CUIWindow* ui_cast_window() override { return this; }
private:
	CUIScrollView*	m_pList;
	u32				m_text_color;
	CGameFont*		m_pFont;
};
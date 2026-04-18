#pragma once
#include "../../xrUI/Widgets/UIDialogWnd.h"

class CUIScrollView;


class CUISpeechMenu final : public CUIDialogWnd
{
public:
					CUISpeechMenu	(const char* section_name);
	virtual			~CUISpeechMenu	();
			void	InitList		(const char* section_name);
	virtual bool	NeedCursor		()const {return  false;}
	virtual bool	OnKeyboardAction		(int dik, EUIMessages keyboard_action);
	virtual bool	StopAnyMove		() {return false;}
	virtual CUIWindow* ui_cast_window() { return this; }
private:
	CUIScrollView*	m_pList;
	u32				m_text_color;
	CGameFont*		m_pFont;
};
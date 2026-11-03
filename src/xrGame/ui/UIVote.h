#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"

class CUIStatic;
class CUI3tButton;
class CUIListBox;
class CUIFrameWindow;



class CUIVote final : public CUIDialogWnd
{
public:

					CUIVote		();
			void 	Init		();
	virtual void 	Update		() override;
	virtual void 	SendMessage	(CUIWindow* pWnd, s16 msg, void* pData = 0) override;
			void 	OnBtnYes	();
			void 	OnBtnNo		();
			void 	OnBtnCancel	();
			void 	SetVoting	(const char* txt);

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	CUIStatic*		msg;
	CUIStatic*		cap[3];
	CUIFrameWindow* frame[3];
	CUIListBox*		list[3];

	CUI3tButton*	btn_yes;
	CUI3tButton*	btn_no;
	CUI3tButton*	btn_cancel;
	CUIStatic*		bkgrnd;
	u32				m_prev_upd_time;
};
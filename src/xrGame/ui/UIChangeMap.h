#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"

class CUIStatic;
class CUI3tButton;
class CUIFrameWindow;
class CUIListBox;
class CUIXml;

class CUIChangeMap final : public CUIDialogWnd 
{
public:
					CUIChangeMap			();
	virtual ~CUIChangeMap			() override;
			void	InitChangeMap			(CUIXml& xml_doc);

	virtual bool	OnKeyboardAction				(int dik, EUIMessages keyboard_action) override;
	virtual void	SendMessage				(CUIWindow* pWnd, s16 msg, void* pData = 0) override;

	void 			OnBtnOk					();
	void 			OnBtnCancel				();
	void 			OnItemSelect			();

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
			void	FillUpList				();

	CUIStatic*		bkgrnd;
	CUIStatic*		header;
	CUIStatic*		map_pic;
	CUIStatic*		map_frame;
	CUIStatic*		map_version;
	CUIFrameWindow* frame;
	CUIFrameWindow* lst_back;
	CUIListBox*		lst;

	CUI3tButton*	btn_ok;
	CUI3tButton*	btn_cancel;

	u32						m_prev_upd_time;
};
#pragma once
#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

class CUIMessageBox;

class CUIMessageBoxEx final : public CUIDialogWnd, public CUIWndCallback
{
public:
					CUIMessageBoxEx		();
	virtual ~CUIMessageBoxEx	() override;
			void	 SetText			(const char* text);
			const char*	GetText				();
	virtual void	InitMessageBox		(const char* xml_template);
	virtual void	SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = NULL) override;

	const char*			GetHost				();
	const char*			GetPassword			();

	void			SetTextEditURL		(const char* text);
	const char*			GetTextEditURL		();

	CUIWndCallback::void_function		func_on_ok;
	CUIWndCallback::void_function		func_on_no;
	void 	OnOKClicked			(CUIWindow*, void*);
	void 	OnNOClicked			(CUIWindow*, void*);

	virtual bool	OnKeyboardAction			(int dik, EUIMessages keyboard_action) override;
	virtual bool	OnGamepadKeyAction			(int id, EUIMessages gamepad_action) override;
	virtual bool	NeedCenterCursor	()const override {return false;}

	virtual CUIWindow* ui_cast_window() override { return this; }

    CUIMessageBox*						m_pMessageBox;
};
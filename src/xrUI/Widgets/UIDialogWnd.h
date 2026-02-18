#pragma once

#include "UIWindow.h"
#include "UIDialogHolder.h"

class UI_API CUIDialogWnd : public CUIWindow  
{
private:
	typedef CUIWindow inherited;
	CDialogHolder*					m_pParentHolder;
protected:
public:
	bool										m_bWorkInPause;
				CUIDialogWnd					();
	virtual		~CUIDialogWnd					();

	virtual void Show							(bool status);

	virtual bool OnKeyboardAction				(int dik, EUIMessages keyboard_action);
	virtual bool OnKeyboardHold					(int dik);

	virtual bool OnGamepadKeyAction				(int key, EUIMessages gamepad_action);
	virtual bool OnGamepadStickAction			(int key, Fvector2 value, EUIMessages gamepad_action);

	CDialogHolder* GetHolder					()								{return m_pParentHolder;};
			void SetHolder						(CDialogHolder* h)				{m_pParentHolder = h;};
	virtual bool StopAnyMove					()								{return true;}
	virtual bool NeedCursor						()const							{return true;}
	virtual bool ForceCursorInput				()								{return false;}
	virtual bool NeedCenterCursor				()const							{return true;}
	virtual bool WorkInPause					()const							{return m_bWorkInPause;}
	virtual bool Dispatch						(int cmd, int param)			{return true;}
    virtual void ShowOrHideDialog				(bool bDoHideIndicators);
			void ShowDialog						(bool bDoHideIndicators);
	virtual void HideDialog						();

	virtual bool IR_process						();

	virtual CUIWindow* ui_cast_window() { return this; }
};

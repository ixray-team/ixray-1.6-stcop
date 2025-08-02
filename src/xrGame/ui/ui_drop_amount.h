////////////////////////////////////////////////////////////////////////////
//	Module 		: ui_drop_amount.h
//	Created 	: 25.07.2025
//	Author		: St4lker0k765
//	Description : Implementation for custom amount of items for drop
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

class CUIStatic;
class CUIWindow;
class CUITrackBar;
class CUI3tButton;

class CUIItemDropAmountWnd final : 
	public CUIDialogWnd,
	public CUIWndCallback
{

public:
	enum EDropMode
	{
		eModeDrop,
		eModeMove,
		eModeTake,
	};
								CUIItemDropAmountWnd				();
			void				InitDropAmount						();
			void				ShowDropAmount						(u32 max, EDropMode mode);
	virtual void				SendMessage							(CUIWindow* pWnd, s16 msg, void* pData = NULL);
	virtual bool				OnKeyboardAction					(int dik, EUIMessages keyboard_action);
	virtual CUIWindow* ui_cast_window() { return this; }
private:
	CUIStatic*					m_UIBackground;
	CUIStatic*					m_UIStaticPicture;
	CUIStatic*					m_UIStaticText;
	CUI3tButton*				m_UIButtonYes;
	CUI3tButton*				m_UIButtonNo;
	CUITrackBar*				m_UITrackBar;
	CUIStatic*					m_UIStaticValueMin;
	CUIStatic*					m_UIStaticValueMax;

	EDropMode					m_dropMode;

	void						PerformDrop							();
	void						OnBtnYesClicked						(CUIWindow* w, void* d);
	void						OnBtnNoClicked						(CUIWindow* w, void* d);
};
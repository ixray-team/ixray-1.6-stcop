////////////////////////////////////////////////////////////////////////////
//	Module 		: ui_drop_amount.h
//	Created 	: 25.07.2025
//	Author		: St4lker0k765
//	Description : Implementation for custom amount of items for drop
////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "ui_drop_amount.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UITrackBar.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UIGameCustom.h"
#include "UIActorMenu.h"

CUIItemDropAmountWnd::CUIItemDropAmountWnd()
{
	m_UIStaticPicture	= nullptr;
	m_UIStaticText		= nullptr;
	m_UIButtonYes		= nullptr;
	m_UIButtonNo		= nullptr;
	m_UITrackBar		= nullptr;
	m_UIStaticValueMin	= nullptr;
	m_UIStaticValueMax	= nullptr;
	m_dropMode			= eModeDrop;
}

void CUIItemDropAmountWnd::InitDropAmount()
{
	CUIXml							uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "custom_drop_amount.xml");

	SetWndPos(Fvector2().set(0, 0));
	SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));

	LPCSTR base = "custom_drop";
	m_UIBackground = UIHelper::CreateStatic(uiXml, base, this);

	string512 str;

	xr_strconcat(str, base, ":picture");
	if (uiXml.NavigateToNode(str, 0)) 
	{
		m_UIStaticPicture = UIHelper::CreateStatic(uiXml, str, m_UIBackground);
	}

	xr_strconcat(str, base, ":text_hint");
	m_UIStaticText = UIHelper::CreateStatic(uiXml, str, m_UIBackground);

	xr_strconcat(str, base, ":button_yes");
	m_UIButtonYes = UIHelper::Create3tButton(uiXml, str, m_UIBackground);
	Register(m_UIButtonYes);
	AddCallback(m_UIButtonYes, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnYesClicked));

	xr_strconcat(str, base, ":button_no");
	m_UIButtonNo = UIHelper::Create3tButton(uiXml, str, m_UIBackground);
	Register(m_UIButtonNo);
	AddCallback(m_UIButtonNo, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnNoClicked));

	xr_strconcat(str, base, ":trackbar");
	m_UITrackBar = UIHelper::CreateTrackBar(uiXml, str, m_UIBackground);
	m_UITrackBar->SetCurrentID(0);
	m_UITrackBar->SaveBackUpOptValue();

	xr_strconcat(str, base, ":value_min");
	m_UIStaticValueMin = UIHelper::CreateStatic(uiXml, str, m_UIBackground);

	xr_strconcat(str, base, ":value_max");
	m_UIStaticValueMax = UIHelper::CreateStatic(uiXml, str, m_UIBackground);
}

void CUIItemDropAmountWnd::ShowDropAmount(u32 max, EDropMode mode)
{
	ShowDialog(false);

	m_UITrackBar->SetOptIBounds(1, max + 1);
	m_UITrackBar->UndoOptValue();

	m_dropMode = mode;

	string256 hint_str;
	xr_sprintf(hint_str, "st_custom_drop_hint_%d", m_dropMode);
	m_UIStaticText->SetTextST(hint_str);

	string32 cnt;
	xr_sprintf(cnt, "%d", max + 1);
	m_UIStaticValueMax->SetText(cnt);
}

void CUIItemDropAmountWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIItemDropAmountWnd::PerformDrop()
{
	switch (m_dropMode)
	{
	case eModeDrop:
	{
		CurrentGameUI()->ActorMenu().DropAllCurrentItem(m_UITrackBar->GetIValue() - 1);
		break;
	}
	case eModeMove:
	{
		CurrentGameUI()->ActorMenu().MoveAllCurrentItem(m_UITrackBar->GetIValue() - 1);
		break;
	}
	case eModeTake:
	{
		CurrentGameUI()->ActorMenu().TakeAllCurrentItem(m_UITrackBar->GetIValue() - 1);
		break;
	}
	}
}

void CUIItemDropAmountWnd::OnBtnYesClicked(CUIWindow* w, void* d)
{
	PerformDrop();
	HideDialog();
}

void CUIItemDropAmountWnd::OnBtnNoClicked(CUIWindow* w, void* d)
{
	HideDialog();
}

bool CUIItemDropAmountWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if ( is_binded(kUSE, dik) || is_binded(kINVENTORY, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			HideDialog();
		}
		return true;
	}	

	if ( is_binded(kQUIT, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			HideDialog();
		}
		return true;
	}

	if( CUIDialogWnd::OnKeyboardAction(dik,keyboard_action) )
		return true;

	return false;
}

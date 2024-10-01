#include "stdafx.h"
#include "uidialogwnd.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_UICustom.h"

CUIDialogWnd:: CUIDialogWnd()
{
	m_pParentHolder		= nullptr;
	m_bWorkInPause		= false;
	m_bShowMe			= false;
}

CUIDialogWnd::~CUIDialogWnd()
{}

void CUIDialogWnd::Show(bool status)
{
	inherited::Show		(status);

	if(status)
		ResetAll();
}

bool CUIDialogWnd::OnKeyboardHold(int dik)
{
	if(!IR_process()) return false;
	return inherited::OnKeyboardHold(dik);
}

bool CUIDialogWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if(!IR_process()) return false;
	if (inherited::OnKeyboardAction(dik, keyboard_action) )
		return true;
	return false;
}

bool CUIDialogWnd::IR_process()
{
	if(!IsEnabled())					return false;

	if(GetHolder() && GetHolder()->IgnorePause())		
		return true;

	if(Device.Paused()&&!WorkInPause())	return false;
	
	return true;
}


UI_API CDialogHolder* CurrentDialogHolder()
{
	if (g_pGamePersistent->m_pMainMenu->IsActive())
		return g_pGamePersistent->m_pMainMenu->GetDialogHolder();

	return g_pGameCustom->GetDialogHolder();
}


void CUIDialogWnd::ShowDialog(bool bDoHideIndicators)
{
	if(!IsShown())
		CurrentDialogHolder()->StartDialog(this,bDoHideIndicators);
}

void CUIDialogWnd::HideDialog()
{
	if (!IsShown())
		return;

	GetHolder()->StopDialog	(this);
}
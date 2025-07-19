#include "StdAfx.h"
#include "UIGameMP.h"
#include "ui/UIDemoPlayControl.h"
#include "ui/UIServerInfo.h"
#include "../../xrUI/UICursor.h"
#include "Level.h"
#include "game_cl_mp.h"

UIGameMP::UIGameMP() :
	m_pDemoPlayControl(nullptr),
	m_pServerInfo(nullptr),
	m_game(nullptr)
{
}

UIGameMP::~UIGameMP()
{
	xr_delete	(m_pDemoPlayControl);
	xr_delete	(m_pServerInfo);
}

void UIGameMP::ShowDemoPlayControl()
{
	if (!m_pDemoPlayControl)
	{
		m_pDemoPlayControl = new CUIDemoPlayControl();
		m_pDemoPlayControl->Init();
	}
	m_pDemoPlayControl->ShowDialog(false);
	GetUICursor().SetUICursorPosition(m_pDemoPlayControl->GetLastCursorPos());
}

bool UIGameMP::IR_UIOnKeyboardPress(int dik)
{
	if ( is_binded(kCROUCH, dik) && Level().IsDemoPlay())
	{
		ShowDemoPlayControl();
		return true;
	}
	return inherited::IR_UIOnKeyboardPress(dik);
}

bool UIGameMP::IR_UIOnKeyboardRelease(int dik)
{
	return inherited::IR_UIOnKeyboardRelease(dik);
}
/*
bool UIGameMP::IsMapDescShown()
{
	VERIFY(m_pMapDesc);
	return m_pMapDesc->IsShown();
}
void UIGameMP::ShowMapDesc()
{
	if (Level().IsDemoPlay())
		return;
	
	VERIFY(m_pMapDesc);
	if (!m_pMapDesc->IsShown())
	{
		m_pMapDesc->ShowDialog(true);
	}
}*/

bool UIGameMP::IsServerInfoShown	()
{
	if (m_pServerInfo)
		return m_pServerInfo->IsShown();
}

//shows only if it has some info ...
bool UIGameMP::ShowServerInfo()
{
	if (Level().IsDemoPlay())
		return true;

	if (!m_pServerInfo)
	{
		return false;
	}

	/*if (m_pServerInfo->InfoAboted())
	{
		m_game->OnMapInfoAccept();
		return true;
	}*/
	
	if (!m_pServerInfo->HasInfo())
	{
		m_game->OnMapInfoAccept();
		return true;
	}

	if (!m_pServerInfo->IsShown())
	{
		m_pServerInfo->ShowDialog(true);
	}
	return true;
}

void UIGameMP::SetClGame(game_cl_GameState* g)
{
	inherited::SetClGame(g);
	m_game = g->cast_game_cl_mp();
	VERIFY(m_game);

	if (m_pServerInfo)
	{
		if (m_pServerInfo->IsShown())
			m_pServerInfo->HideDialog();

		xr_delete(m_pServerInfo);
	}
	CUIXml xml_doc;
	if (xml_doc.Load(CONFIG_PATH, UI_PATH, "server_info.xml"))
		m_pServerInfo			= new CUIServerInfo();
}

void UIGameMP::SetServerLogo(u8 const * data_ptr, u32 data_size)
{
	if (m_pServerInfo)
		m_pServerInfo->SetServerLogo(data_ptr, data_size);
}
void UIGameMP::SetServerRules(u8 const * data_ptr, u32 data_size)
{
	if (m_pServerInfo)
		m_pServerInfo->SetServerRules(data_ptr, data_size);
}
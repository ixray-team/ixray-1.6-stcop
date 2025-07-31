//=============================================================================
//  Filename:   UIGameLog.h
//	Created by Vitaly 'Mad Max' Maximov, mad-max@gsc-game.kiev.ua
//	Copyright 2005. GSC Game World
//	---------------------------------------------------------------------------
//  Multiplayer game log window
//=============================================================================
#include "stdafx.h"
#include "UIGameLog.h"
#include "../../xrUI/UIXmlInit.h"
#include "UIPdaMsgListItem.h"
#include "UIPdaKillMessage.h"
#include "../../xrUI/Widgets/UILines.h"

CUIGameLog::CUIGameLog()
{
	kill_msg_height = 20;
	txt_color = 0xff000000;
	m_pFont = nullptr;
}

CUITextWnd* CUIGameLog::AddLogMessage(LPCSTR msg)
{
	CUITextWnd* pItem = nullptr;
	ADD_TEXT_TO_VIEW3(msg, pItem, this);
	pItem->SetFont(m_pFont);
	pItem->SetTextColor(txt_color);
	pItem->SetColorAnimation("ui_main_msgs_short", LA_ONLYALPHA | LA_TEXTCOLOR, 5000.0f);
	ForceUpdate();

	return pItem;
}

CUIPdaMsgListItem* CUIGameLog::AddPdaMessage()
{
	CUIPdaMsgListItem* pItem = new CUIPdaMsgListItem();
	pItem->InitPdaMsgListItem(Fvector2().set(GetDesiredChildWidth(), 10.0f));
	pItem->SetColorAnimation("ui_main_msgs_short", LA_ONLYALPHA | LA_TEXTCOLOR | LA_TEXTURECOLOR);
	AddWindow(pItem, true);

	return pItem;
}

CUIPdaKillMessage* CUIGameLog::AddLogMessage(KillMessageStruct& msg)
{
	CUIPdaKillMessage* pItem = new CUIPdaKillMessage();
	pItem->SetWidth(GetDesiredChildWidth());
	pItem->SetHeight(kill_msg_height);
	pItem->Init(msg, m_pFont);
	AddWindow(pItem, true);

	return pItem;
}

void CUIGameLog::AddChatMessage(LPCSTR msg, LPCSTR author)
{
	string256 fullLine;
	xr_strconcat(fullLine, author, " ", msg);

	_TrimRight(fullLine);

	CUITextWnd* pItem = new CUITextWnd();
	pItem->SetTextComplexMode(true);
	pItem->SetText(fullLine);
	pItem->SetCutWordsMode(true);
	pItem->SetFont(m_pFont);
	pItem->SetTextColor(txt_color);
	pItem->SetColorAnimation("ui_main_msgs_short", LA_ONLYALPHA | LA_TEXTCOLOR, 5000.0f);
	pItem->SetWidth(this->GetDesiredChildWidth());
	pItem->AdjustHeightToText();
	AddWindow(pItem, true);
}

void CUIGameLog::SetTextAtrib(CGameFont* pFont, u32 color)
{
	m_pFont = pFont;
	txt_color = color;
}

void CUIGameLog::Update()
{
	CUIScrollView::Update();
	toDelList.clear();

	xrCriticalSectionGuard guard(m_pad->csUi);

	for (CUIWindow* child : m_pad->GetChildWndList())
	{
		CUILightAnimColorConroller* pItem = child->ui_cast_light_anim_color_controller();

		if (!pItem->IsColorAnimationPresent())
		{
			toDelList.push_back(child);
		}
	}

	// Delete elements
	for (CUIWindow* del_item : toDelList)
	{
		RemoveWindow(del_item);
	}

	// REMOVE INVISIBLE AND PART VISIBLE ITEMS
	if (m_flags.test(eNeedRecalc))
	{
		RecalcSize();
	}

	toDelList.clear();
	Frect visible_rect;
	GetAbsoluteRect(visible_rect);

	for (CUIWindow* child : m_pad->GetChildWndList())
	{
		Frect r;
		child->GetAbsoluteRect(r);
		r.shrink(3.0f, 3.0f);

		if (!(visible_rect.in(r.x1, r.y1) && visible_rect.in(r.x2, r.y1) && visible_rect.in(r.x1, r.y2) && visible_rect.in(r.x2, r.y2)))
		{
			toDelList.push_back(child);
		}
	}

	// Delete elements
	for (CUIWindow* del_item : toDelList)
	{
		RemoveWindow(del_item);
	}

	if (m_flags.test(eNeedRecalc))
	{
		RecalcSize();
	}
}
//////////////////////////////////////////////////////////////////////
// UIListItem.cpp: элемент окна списка CListWnd
//////////////////////////////////////////////////////////////////////

#include"stdafx.h"

#include "UIlistitem.h"

CUIListItem::CUIListItem(void)
{
	m_eButtonState = BUTTON_NORMAL;

	m_pData = NULL;

	m_iIndex = -1;
	m_iValue = 0;
	m_bHighlightText = false;
	m_iGroupID = -1;
	SetAutoDelete(true);
	SetTextAlignment(CGameFont::alLeft);
}

CUIListItem::~CUIListItem(void)
{
}

void CUIListItem::Init(float x, float y, float width, float height)
{
	inherited::SetWndPos(Fvector2().set(x, y));
	inherited::SetWndSize(Fvector2().set(width, height));
	SetButtonState(BUTTON_PUSHED);
	SetPushOffset(Fvector2().set(0.0f,0.0f));
}

void CUIListItem::InitTexture(LPCSTR tex_name)
{
	CUIButton::InitTexture(tex_name);
	TextItemControl()->m_TextOffset.x = (m_UIStaticItem.GetSize().x);
}

bool CUIListItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
{

	if( CUIStatic::OnMouseAction(x, y, mouse_action) ) return true;

	if ( (	WINDOW_LBUTTON_DOWN==mouse_action	||
			WINDOW_LBUTTON_UP==mouse_action		||
			WINDOW_RBUTTON_DOWN==mouse_action	||
			WINDOW_RBUTTON_UP==mouse_action)	&& 
			HasChildMouseHandler())
		return false;

	if(mouse_action == WINDOW_MOUSE_MOVE)
	{
		if(m_bCursorOverWindow)
		{
			SetButtonState(BUTTON_PUSHED);
		} else {
			SetButtonState(BUTTON_NORMAL);
		}
	}
	else if(mouse_action == WINDOW_LBUTTON_DOWN || mouse_action == WINDOW_LBUTTON_DB_CLICK)
	{
		if(m_bCursorOverWindow)
		{
			OnClick();
			return true;
		}
	}
	return false;
}


void CUIListItem::Init(const char* str, float x, float y, float width, float height)
{
	Init(x,y,width, height);
	SetTextST(str);	
}

bool CUIListItem::IsHighlightText()
{
	return CUIButton::IsHighlightText();
}

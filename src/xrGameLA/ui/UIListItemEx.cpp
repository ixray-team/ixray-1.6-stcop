// File:        UIListWndEx.cpp
// Description: Extended ListItem
//              Requiered to use feature "Selected Item"
// Created:     
// Author:      Serhiy O. Vynnychenko
// Mail:        narrator@gsc-game.kiev.ua

// Copyright:   2004 GSC Game World

#include "stdafx.h"
#include ".\uilistitemex.h"

CUIListItemEx::CUIListItemEx(void)
{
	this->m_dwSelectionColor = color_argb(200, 95, 82, 74);
	this->SetColor(color_argb(0, 0, 0, 0));
}

CUIListItemEx::~CUIListItemEx(void)
{
}

void CUIListItemEx::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	switch (msg)
	{
	case LIST_ITEM_SELECT:
		this->SetColor(m_dwSelectionColor);	
		break;
	case LIST_ITEM_UNSELECT:
		this->SetColor(color_argb(0, 0, 0, 0));
		break;
	default:
		break;
	}
}

void CUIListItemEx::SetSelectionColor(u32 dwColor)
{
	m_dwSelectionColor = dwColor;
}

void CUIListItemEx::Draw()
{
	inherited::Draw();
}

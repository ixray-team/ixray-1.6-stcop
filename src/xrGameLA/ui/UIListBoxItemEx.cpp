// File:        UIListBoxItemEx.cpp
// Description: Extended ListItem
//              Requiered to use feature "Selected Item"
// Created:     
// Author:      Serhiy O. Vynnychenko
// Mail:        narrator@gsc-game.kiev.ua
// Adapted by SkyLoader

// Copyright:   2004 GSC Game World

#include "stdafx.h"
#include ".\uilistboxitemex.h"

CUIListBoxItemEx::CUIListBoxItemEx(void)
{
	this->m_dwSelectionColor = color_argb(200, 95, 82, 74);
	this->SetTextColor(color_argb(0, 0, 0, 0));
}

CUIListBoxItemEx::~CUIListBoxItemEx(void) {}

void CUIListBoxItemEx::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	switch (msg)
	{
	case LIST_ITEM_SELECT:
		this->SetTextColor(m_dwSelectionColor);	
		break;
	case LIST_ITEM_UNSELECT:
		this->SetTextColor(color_argb(0, 0, 0, 0));
		break;
	}
}

void CUIListBoxItemEx::SetSelectionColor(u32 dwColor)
{
	m_dwSelectionColor = dwColor;
}

void CUIListBoxItemEx::Draw()
{
	inherited::Draw();	
}
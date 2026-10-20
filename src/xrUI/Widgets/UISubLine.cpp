// File:		UISubLine.cpp
// Description:	Text line. Owns color attribute
// Created:		04.04.2005
// Author:		Serge Vynnycheko
// Mail:		narrator@gsc-game.kiev.ua
//
// Copyright 2005 GSC Game World

#include "stdafx.h"
#include "UISubLine.h"
#include "uilinestd.h"
#include "../ui_base.h"

CUISubLine::CUISubLine(const CUISubLine& other)
{
	m_color = other.m_color;
	m_last_in_line = other.m_last_in_line;
	m_text = other.m_text;
	m_pTempLine = nullptr;
}

CUISubLine& CUISubLine::operator=(const CUISubLine& other)
{
	m_color = other.m_color;
	m_text = other.m_text;
	m_last_in_line = other.m_last_in_line;
	xr_delete(m_pTempLine);
	return (*this);
}

CUISubLine::CUISubLine()
	: m_color(0), m_pTempLine(nullptr), m_last_in_line(false)
{
}

CUISubLine::~CUISubLine()
{
	xr_delete(m_pTempLine);
}

const CUISubLine* CUISubLine::Cut2Pos(int i)
{
	R_ASSERT2(i < (int)m_text.size(), make_string<const char*>("CUISubLine::Cut2Pos - invalid parameter [%d][%d]", i, m_text.size()));

	if (!m_pTempLine)
	{
		m_pTempLine = new CUISubLine();
	}
	m_pTempLine->m_color = m_color;
	m_pTempLine->m_text.assign(m_text, 0, i + 1);
	m_text.replace(0, i + 1, "");

	return m_pTempLine;
}

void CUISubLine::Draw(CGameFont* pFont, float x, float y, u32 colorOverride) const
{
	u32 drawColor = m_color;
	if (colorOverride != 0)
	{
		const u32 alpha = (color_get_A(colorOverride) * color_get_A(m_color)) / 255;
		drawColor = subst_alpha(colorOverride, alpha);
	}
	pFont->SetColor(drawColor);
	pFont->Out(UI().ClientToScreenScaledX(x), UI().ClientToScreenScaledY(y), "%s", m_text.c_str());
}

void CUISubLine::DrawWS(CGameFont* pFont, float x, float y, u32 colorOverride) const
{
	u32 DrawColor = m_color;
	if (colorOverride != 0)
	{
		const u32 Alpha = (color_get_A(colorOverride) * color_get_A(m_color)) / 255;
		DrawColor = subst_alpha(colorOverride, Alpha);
	}
	pFont->SetColor(DrawColor);
	pFont->Out(x, y, "%s", m_text.c_str());
}

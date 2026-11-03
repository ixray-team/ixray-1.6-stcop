// File:		UILine.cpp
// Description:	Single text line
// Created:		05.04.2005
// Author:		Serge Vynnycheko
// Mail:		narrator@gsc-game.kiev.ua
//
// Copyright 2005 GSC Game World


#include "stdafx.h"
#include "UILine.h"
#include "uilinestd.h"
#include "ui_base.h"

CUILine::CUILine()
{
	m_tmpLine = nullptr;
}

CUILine::~CUILine()
{
	xr_delete(m_tmpLine);
}

CUILine::CUILine(const CUILine& other)
{
	m_subLines = other.m_subLines;
	m_tmpLine = nullptr;
}

CUILine& CUILine::operator=(const CUILine& other)
{
	m_subLines = other.m_subLines;
	xr_delete(m_tmpLine);
	return (*this);
}

void CUILine::AddSubLine(const xr_string& str, u32 color)
{
	CUISubLine sline;
	sline.m_color = color;
	sline.m_text = str;
	m_subLines.push_back(sline);
}

void CUILine::AddSubLine(const char* str, u32 color)
{
	CUISubLine sline;
	sline.m_color = color;
	sline.m_text = str;
	m_subLines.push_back(sline);
}

void CUILine::AddSubLine(const CUISubLine* subLine)
{
	m_subLines.push_back(*subLine);
}

void CUILine::Clear()
{
	m_subLines.clear();
}

void CUILine::ProcessNewLines()
{
	for (u32 Iter = 0; Iter < m_subLines.size(); Iter++)
	{
		StrSize Pos = m_subLines[Iter].m_text.find("\\n");

		if (Pos != npos)
		{
			CUISubLine SubLine;
			if (Pos)
			{
				SubLine = *m_subLines[Iter].Cut2Pos((int)Pos - 1);
			}

			SubLine.m_last_in_line = true;
			m_subLines.insert(m_subLines.begin() + Iter, SubLine);
			m_subLines[Iter + 1].m_text.erase(0, 2);

			if (m_subLines[Iter + 1].m_text.empty())
			{
				m_subLines.erase(m_subLines.begin() + Iter + 1);
			}
		}
	}
}

void CUILine::Draw(CGameFont* pFont, float x, float y, u32 colorOverride) const
{
	float length = 0;
	int size = (int)m_subLines.size();

	for (int i = 0; i < size; i++)
	{
		m_subLines[i].Draw(pFont, x + length, y, colorOverride);
		float ll = pFont->SizeOf_(m_subLines[i].m_text.c_str()); //. all ok
		UI().ClientToScreenScaledWidth(ll);
		length += ll;
	}
}

void CUILine::DrawWS(CGameFont* pFont, float x, float y, u32 colorOverride) const
{
	float length = 0;
	int size = (int)m_subLines.size();

	for (int i = 0; i < size; i++)
	{
		m_subLines[i].DrawWS(pFont, x + length, y, colorOverride);
		float ll = pFont->SizeOf_(m_subLines[i].m_text.c_str()); //. all ok
		// UI().ClientToScreenScaledWidth(ll);
		length += ll;
	}
}

namespace
{
void DrawJustifiedInternal(
	CGameFont* pFont,
	float x,
	float y,
	float targetWidth,
	u32 colorOverride,
	const xr_vector<CUISubLine>& subLines,
	bool worldSpace
)
{
	float naturalWidth = 0.f;
	int spaceCount = 0;
	const int size = (int)subLines.size();

	for (int i = 0; i < size; ++i)
	{
		const xr_string& text = subLines[i].m_text;
		float ll = pFont->SizeOf_(text.c_str());
		if (!worldSpace)
		{
			// SizeOf_ is in font/screen pixels; convert to UI units (same as CUILine::Draw).
			UI().ClientToScreenScaledWidth(ll);
		}
		naturalWidth += ll;

		for (char ch : text)
		{
			if (ch == ' ')
			{
				++spaceCount;
			}
		}
	}

	// targetWidth is already in the same space as naturalWidth (UI for HUD, raw for WS).
	if (spaceCount <= 0 || naturalWidth >= targetWidth)
	{
		if (worldSpace)
		{
			float length = 0.f;
			for (int i = 0; i < size; ++i)
			{
				subLines[i].DrawWS(pFont, x + length, y, colorOverride);
				length += pFont->SizeOf_(subLines[i].m_text.c_str());
			}
		}
		else
		{
			float length = 0.f;
			for (int i = 0; i < size; ++i)
			{
				subLines[i].Draw(pFont, x + length, y, colorOverride);
				float ll = pFont->SizeOf_(subLines[i].m_text.c_str());
				UI().ClientToScreenScaledWidth(ll);
				length += ll;
			}
		}
		return;
	}

	const float extraPerSpace = (targetWidth - naturalWidth) / float(spaceCount);
	pFont->SetAligment(CGameFont::alLeft);

	float cursor = x;
	for (int i = 0; i < size; ++i)
	{
		const CUISubLine& sbl = subLines[i];
		const xr_string& text = sbl.m_text;

		u32 drawColor = sbl.m_color;
		if (colorOverride != 0)
		{
			const u32 alpha = (color_get_A(colorOverride) * color_get_A(sbl.m_color)) / 255;
			drawColor = subst_alpha(colorOverride, alpha);
		}
		pFont->SetColor(drawColor);

		size_t pos = 0;
		while (pos < text.size())
		{
			if (text[pos] == ' ')
			{
				size_t end = pos;
				while (end < text.size() && text[end] == ' ')
				{
					++end;
				}

				const int spaces = int(end - pos);
				const xr_string spaceRun = text.substr(pos, end - pos);
				float runW = pFont->SizeOf_(spaceRun.c_str());
				if (!worldSpace)
				{
					UI().ClientToScreenScaledWidth(runW);
				}
				cursor += runW + extraPerSpace * float(spaces);
				pos = end;
				continue;
			}

			size_t end = pos;
			while (end < text.size() && text[end] != ' ')
			{
				++end;
			}

			const xr_string word = text.substr(pos, end - pos);
			if (worldSpace)
			{
				pFont->Out(cursor, y, "%s", word.c_str());
			}
			else
			{
				pFont->Out(UI().ClientToScreenScaledX(cursor), UI().ClientToScreenScaledY(y), "%s", word.c_str());
			}

			float wordW = pFont->SizeOf_(word.c_str());
			if (!worldSpace)
			{
				UI().ClientToScreenScaledWidth(wordW);
			}
			cursor += wordW;
			pos = end;
		}
	}
}
} // namespace

void CUILine::DrawJustified(CGameFont* pFont, float x, float y, float targetWidth, u32 colorOverride) const
{
	DrawJustifiedInternal(pFont, x, y, targetWidth, colorOverride, m_subLines, false);
}

void CUILine::DrawJustifiedWS(CGameFont* pFont, float x, float y, float targetWidth, u32 colorOverride) const
{
	DrawJustifiedInternal(pFont, x, y, targetWidth, colorOverride, m_subLines, true);
}

int CUILine::GetSize()
{
	int sz = 0;
	int size = (int)m_subLines.size();
	for (int i = 0; i < size; i++)
	{
		sz += (int)m_subLines[i].m_text.size();
	}

	return sz;
}

const CUILine* CUILine::GetEmptyLine()
{
	xr_delete(m_tmpLine);
	m_tmpLine = new CUILine();

	return m_tmpLine;
}

const CUILine* CUILine::Cut2Pos(Position& pos, bool to_first)
{
	xr_delete(m_tmpLine);
	m_tmpLine = new CUILine();

	int last;

	if (to_first || !pos.is_separated())
	{
		last = pos.curr_subline - 1;
	}
	else
	{
		last = pos.curr_subline;
	}

	for (int i = 0; i <= last; i++)
	{
		m_tmpLine->AddSubLine(&m_subLines[i]);

		if (m_subLines[i].m_last_in_line) // check if this subline must be last in line
		{
			for (int j = 0; j <= i; j++)
			{
				m_subLines.erase(m_subLines.begin());
			}
			return m_tmpLine;
		}
	}

	if (to_first)
	{
		m_tmpLine->AddSubLine(m_subLines[last + 1].Cut2Pos(pos.word_1.last_space()));
	}
	else
	{
		m_tmpLine->AddSubLine(m_subLines[last + 1].Cut2Pos(pos.word_2.last_space()));
	}

	for (int i = 0; i <= last; i++)
	{
		m_subLines.erase(m_subLines.begin());
	}

	return m_tmpLine;
}

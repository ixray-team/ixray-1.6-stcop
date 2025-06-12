// CUIEditBox.cpp: ввод строки с клавиатуры
// 
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "UIEditBox.h"
#include "UIFrameLineWnd.h"


CUIEditBox::CUIEditBox()
:m_frameLine(nullptr)
{
}

void CUIEditBox::InitCustomEdit(Fvector2 pos, Fvector2 size)
{
	if(m_frameLine)
	{
		m_frameLine->SetWndPos			(Fvector2().set(0,0));
		m_frameLine->SetWndSize			(size);
	}
	CUICustomEdit::InitCustomEdit	(pos, size);
}

bool CUIEditBox::InitTextureEx(LPCSTR texture, LPCSTR  shader, bool fatal)
{
	if(!m_frameLine)
	{
		m_frameLine = new CUIFrameLineWnd();
		AttachChild(m_frameLine);
		m_frameLine->SetAutoDelete(true);
	}
	const bool result = m_frameLine->InitTexture(texture, shader);
	m_frameLine->SetWndPos			(Fvector2().set(0,0));
	m_frameLine->SetWndSize			(GetWndSize());
	return result;
}

bool CUIEditBox::InitTexture(LPCSTR texture, bool fatal)
{
	return InitTextureEx(texture, "hud\\default", fatal);
}

void CUIEditBox::SetCurrentOptValue()
{
	CUIOptionsItem::SetCurrentOptValue	();
	SetText								(GetOptStringValue());
}

void CUIEditBox::SaveOptValue()
{
	CUIOptionsItem::SaveOptValue		();
	SaveOptStringValue					(GetText());
}

void CUIEditBox::SaveBackUpOptValue()
{
	CUIOptionsItem::SaveBackUpOptValue	();
	m_opt_backup_value					= GetText();
}

void CUIEditBox::UndoOptValue()
{
	SetText								(m_opt_backup_value.c_str());
	CUIOptionsItem::UndoOptValue		();
}

bool CUIEditBox::IsChangedOptValue() const
{
	return 0 != xr_strcmp(m_opt_backup_value.c_str(), GetText());
}




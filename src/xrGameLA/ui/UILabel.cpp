#include "StdAfx.h"

#include "UILabel.h"

CUILabel::CUILabel()
{
	AttachChild				(&m_text);

	m_textPos.set				(0,0);
	m_text.TextItemControl()->SetVTextAlignment	(valCenter);
	m_lainm_start_time			= -1.0f;
	m_lanim						= nullptr;	
}

void CUILabel::InitLabel(Fvector2 pos, Fvector2 size)
{
	CUIFrameLineWnd::SetWndPos	(pos);
	CUIFrameLineWnd::SetWndSize	(size);
	m_text.TextItemControl()->SetWndPos			(pos);
	m_text.TextItemControl()->SetWndSize			(size);
}

void CUILabel::Draw()
{
	CUIFrameLineWnd::Draw();
	Fvector2 p;
	CUIFrameLineWnd::GetAbsolutePos(p);
	m_text.TextItemControl()->Draw(p.x + m_textPos.x, p.y + m_textPos.y);
}

void CUILabel::SetWidth(float width)
{
	m_text.TextItemControl()->SetWidth(width);
	CUIFrameLineWnd::SetWidth(width);
}

void CUILabel::SetHeight(float height)
{
	m_text.TextItemControl()->SetHeight(height);
	CUIFrameLineWnd::SetHeight(height);
}

void CUILabel::SetLightAnim(LPCSTR lanim)
{
	if(lanim&&xr_strlen(lanim))
		m_lanim	= LALib.FindItem(lanim);
	else
		m_lanim	= nullptr;
}

void CUILabel::Update()
{
	CUIFrameLineWnd::Update();
	if (m_lanim)
	{
		if(m_lainm_start_time<0.0f) m_lainm_start_time = Device.fTimeGlobal; 
		int frame;
		u32 clr					= m_lanim->CalculateRGB(Device.fTimeGlobal-m_lainm_start_time,frame);
		SetTextureColor				(clr);
		m_text.TextItemControl()->SetTextColor			(clr);
	}
}
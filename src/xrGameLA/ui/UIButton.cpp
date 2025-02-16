#include "stdafx.h"
#include "UIButton.h"
#include "UIBtnHint.h"
#include "../UICursor.h"
#include "../ui_base.h"
#include "../../xrEngine/xr_input.h"	
#include "../xr_level_controller.h"

#define PUSH_OFFSET_RIGHT 1
#define PUSH_OFFSET_DOWN  1



CUIButton:: CUIButton()
{
	m_eButtonState				= BUTTON_NORMAL;
	m_bIsSwitch					= false;

	m_uAccelerator[0]			= 0;
	m_uAccelerator[1]			= 0;
	m_uAccelerator[2]			= -1;
	m_uAccelerator[3]			= -1;

	m_PushOffset.set			(PUSH_OFFSET_RIGHT, PUSH_OFFSET_DOWN);
	m_ShadowOffset.set			(0.0f,0.0f);

	m_HighlightColor			= 0xFFFFFFFF;
	m_bEnableTextHighlighting		= true;

	TextItemControl()->SetTextComplexMode			(false);
	TextItemControl()->SetTextAlignment			(CGameFont::alCenter); // this will create class instance for m_pLines
	TextItemControl()->SetVTextAlignment			(valCenter);

	m_bClickable				= true;
}

void CUIButton::Reset()
{
	m_bCursorOverWindow			= false;
	inherited::Reset			();
}

void CUIButton::Enable(bool status)
{
	CUIStatic::Enable			(status);

	if (!status)
		m_bCursorOverWindow		= false;
}

bool  CUIButton::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if( inherited::OnMouseAction(x, y, mouse_action) ) return true;

	if ( (	WINDOW_LBUTTON_DOWN==mouse_action	||
			WINDOW_LBUTTON_UP==mouse_action		||
			WINDOW_RBUTTON_DOWN==mouse_action	||
			WINDOW_RBUTTON_UP==mouse_action)	&& 
			HasChildMouseHandler())
		return false;

	switch (m_eButtonState)
	{
	case BUTTON_NORMAL:
		{
			if(mouse_action == WINDOW_LBUTTON_DOWN || mouse_action == WINDOW_LBUTTON_DB_CLICK)
			{
				SetButtonState(BUTTON_PUSHED);
				GetMessageTarget()->SendMessage(this, BUTTON_DOWN, NULL);
				return true;
			}
		}break;
	case BUTTON_PUSHED:
		{
			if(mouse_action == WINDOW_LBUTTON_UP)
			{
				if(m_bCursorOverWindow)
					OnClick();
			
				if (!m_bIsSwitch)
					SetButtonState(BUTTON_NORMAL);

				return true;
			}else 
			if(mouse_action == WINDOW_MOUSE_MOVE)
			{
				if(!m_bCursorOverWindow && !m_bIsSwitch)
					SetButtonState(BUTTON_UP);
			}
		}break;
	case BUTTON_UP:
		{
			if(mouse_action == WINDOW_MOUSE_MOVE)
			{
				if(m_bCursorOverWindow)
					SetButtonState(BUTTON_PUSHED);
			}else 
			if(mouse_action == WINDOW_LBUTTON_UP)
			{
				SetButtonState(BUTTON_NORMAL);
			}
		}break;
	};
	return false;
}

void CUIButton::OnClick()
{
	GetMessageTarget()->SendMessage(this, BUTTON_CLICKED);
}	

void CUIButton::DrawTexture()
{
	Frect rect; 
	GetAbsoluteRect		(rect);

	if(m_bTextureEnable && GetShader() && GetShader()->inited())
	{
		if(GetButtonState() == BUTTON_UP || GetButtonState() == BUTTON_NORMAL)
			m_UIStaticItem.SetPos(rect.left + m_TextureOffset.x, rect.top + m_TextureOffset.y);
		else
			m_UIStaticItem.SetPos(rect.left + m_PushOffset.x + m_TextureOffset.x, rect.top + m_PushOffset.y + m_TextureOffset.y);

		if(m_bStretchTexture)
			m_UIStaticItem.SetSize(Fvector2().set(rect.width(), rect.height()));
		else
			m_UIStaticItem.SetSize(Fvector2().set(m_UIStaticItem.GetTextureRect().width(), m_UIStaticItem.GetTextureRect().height()));

		if( Heading() )
			m_UIStaticItem.Render( GetHeading() );
		else
			m_UIStaticItem.Render();		
	}
}

void CUIButton::DrawHighlightedText()
{

	float right_offset;
	float down_offset;

	if(GetButtonState() == BUTTON_UP || GetButtonState() == BUTTON_NORMAL)
	{
		right_offset		= 0.0f;
		down_offset		= 0.0f;
	} else {
		right_offset		= m_PushOffset.x;
		down_offset		= m_PushOffset.y;
	}

	Fvector2			p;
	GetAbsolutePos		(p);

	u32 def_col = m_pTextControl->GetTextColor();
	m_pTextControl->SetTextColor	(m_HighlightColor);
	m_pTextControl->Draw		(p.x+right_offset+m_ShadowOffset.x, p.y+down_offset+m_ShadowOffset.y);
	m_pTextControl->SetTextColor	(def_col);
}

void CUIButton::DrawText()
{
	float right_offset;
	float down_offset;

	if(GetButtonState() == BUTTON_UP || GetButtonState() == BUTTON_NORMAL)
	{
		right_offset		= 0;
		down_offset		= 0;
	}
	else
	{
		right_offset		= m_PushOffset.x;
		down_offset		= m_PushOffset.y;
	}

	if (m_pTextControl)
	{
		if( !fsimilar(m_pTextControl->m_wndSize.x, m_wndSize.x) || !fsimilar(m_pTextControl->m_wndSize.y, m_wndSize.y))
		{
			m_pTextControl->m_wndSize		= m_wndSize;
			m_pTextControl->ParseText		(true);
		}

		if(IsHighlightText() && xr_strlen(m_pTextControl->GetText())>0 && m_bEnableTextHighlighting)
			DrawHighlightedText();		
		else {
			Fvector2			p;
			GetAbsolutePos		(p);
			m_pTextControl->Draw(p.x+right_offset, p.y+down_offset);
		}
	}

	if(g_btnHint->Owner()==this)
		g_btnHint->Draw_();
}


bool is_in2(const Frect& b1, const Frect& b2){
	return (b1.x1<b2.x1)&&(b1.x2>b2.x2)&&(b1.y1<b2.y1)&&(b1.y2>b2.y2);
}

void  CUIButton::Update()
{
	inherited::Update();

	if(CursorOverWindow() && m_hint_text.size() && !g_btnHint->Owner() && Device.dwTimeGlobal>m_dwFocusReceiveTime+700)
	{
		g_btnHint->SetHintText	(this, m_hint_text.c_str());

		Fvector2 c_pos			= GetUICursor().GetCursorPosition();
		Frect vis_rect;
		vis_rect.set			(0,0,UI_BASE_WIDTH, UI_BASE_HEIGHT);

		//select appropriate position
		Frect r;
		r.set					(0.0f, 0.0f, g_btnHint->GetWidth(), g_btnHint->GetHeight());
		r.add					(c_pos.x, c_pos.y);

		r.sub					(0.0f,r.height());
		if (false==is_in2(vis_rect,r))
			r.sub				(r.width(),0.0f);
		if (false==is_in2(vis_rect,r))
			r.add				(0.0f,r.height());

		if (false==is_in2(vis_rect,r))
			r.add				(r.width(), 45.0f);

		g_btnHint->SetWndPos(r.lt);
	}
}

void CUIButton::OnFocusLost()
{
	inherited::OnFocusLost();
	
	if(m_eButtonState==BUTTON_PUSHED && pInput->iGetAsyncBtnState(0) && !m_bIsSwitch)
		SetButtonState(BUTTON_NORMAL); //??? 

	if(g_btnHint->Owner()==this)
		g_btnHint->Discard	();
}

bool CUIButton::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if (WINDOW_KEY_PRESSED == keyboard_action)
	{
		if(IsAccelerator(dik) )
		{
			OnClick		();
			return		true;
		}
	}
	return inherited::OnKeyboardAction(dik, keyboard_action);
}

void CUIButton::SetAccelerator(int iAccel, int idx)	
{
	VERIFY(idx>=0 && idx<4); 
	m_uAccelerator[idx] = s16(iAccel);
}

const int CUIButton::GetAccelerator(int idx) const			
{
	VERIFY(idx>=0 && idx<4); 
	return m_uAccelerator[idx]; 
}

bool CUIButton::IsAccelerator(int iAccel) const		
{
	bool res = GetAccelerator(0)==iAccel || GetAccelerator(1)==iAccel;
	if(!res)
	{
		res =	((m_uAccelerator[2]!=-1) ? is_binded((EGameActions)GetAccelerator(2), iAccel) : false) || 
				((m_uAccelerator[3]!=-1) ? is_binded((EGameActions)GetAccelerator(3), iAccel) : false);
	}
	return res;
}

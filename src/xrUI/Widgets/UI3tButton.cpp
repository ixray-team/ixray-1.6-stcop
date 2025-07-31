#include "StdAfx.h"
#include "UI3tButton.h"
#include "UIXmlInit.h"
#include "UIHint.h"
#include "UIStatic.h"

CUI3tButton::CUI3tButton()
{
	m_bTextureEnable						= false;
	m_bUseTextColor[S_Disabled]				= true;
	m_bUseTextColor[S_Highlighted]			= false;
	m_bUseTextColor[S_Touched]				= false;

	m_dwTextColor[S_Enabled] 				= 0xFFFFFFFF;
	m_dwTextColor[S_Disabled] 				= 0xFFAAAAAA;
	m_dwTextColor[S_Highlighted]			= 0xFFFFFFFF;
	m_dwTextColor[S_Touched] 				= 0xFFFFFFFF;
	m_bEnableTextHighlighting				= false;

	m_background							= nullptr;
	m_back_frameline						= nullptr;
	m_back_framewindow						= nullptr;
	m_frameline_mode						= Framemode_None;

	m_BtnStatic								= nullptr;
	m_BtnStaticParams.m_bNeedClrChanging	= false;
	u32 def_clr								= color_rgba(255, 255, 255, 255);
	m_BtnStaticParams.m_ClrStateE			= def_clr;
	m_BtnStaticParams.m_ClrStateD			= def_clr;
	m_BtnStaticParams.m_ClrStateT			= def_clr;
	m_BtnStaticParams.m_ClrStateH			= def_clr;
}

CUI3tButton::~CUI3tButton()
{
	if (m_BtnStatic)
	{
		DetachChild(m_BtnStatic);
		xr_delete(m_BtnStatic);
	}
}

void CUI3tButton::AddStatic()
{
	if (!m_BtnStatic)
	{
		m_BtnStatic = new CUIStatic();
		m_BtnStatic->SetWndSize(Fvector2().set(80.f, 10.f));
		m_BtnStatic->SetWndPos(Fvector2().set(-(GetWidth() / 2.f), 0.f));
		m_BtnStatic->TextItemControl()->SetTextComplexMode(true);
		AttachChild(m_BtnStatic);
	}
}

void CUI3tButton::OnClick()
{
	CUIButton::OnClick	();
	PlaySoundT			();
}

bool CUI3tButton::OnMouseDown(int mouse_btn)
{
	return CUIButton::OnMouseDown(mouse_btn);
}

void CUI3tButton::OnFocusLost()
{
	inherited::OnFocusLost();
}

void CUI3tButton::OnFocusReceive()
{
	inherited::OnFocusReceive	();
	PlaySoundH					();
}

void CUI3tButton::InitSoundH(LPCSTR sound_file)
{
	::Sound->create		(m_sound_h, sound_file,st_Effect,sg_SourceType);
}

void CUI3tButton::InitSoundT(LPCSTR sound_file)
{
	::Sound->create		(m_sound_t, sound_file,st_Effect,sg_SourceType); 
}

void CUI3tButton::PlaySoundT()
{
	if (m_sound_t._handle())
		m_sound_t.play(nullptr, sm_2D);
}

void CUI3tButton::PlaySoundH()
{
	if (m_sound_h._handle())
		m_sound_h.play(nullptr, sm_2D);
}
void CUI3tButton::InitButton(Fvector2 pos, Fvector2 size)
{
	if ( m_frameline_mode == Framemode_Line )
	{
		if ( !m_back_frameline )
		{
			m_back_frameline = new CUI_IB_FrameLineWnd();
			m_back_frameline->SetAutoDelete	(true);
			AttachChild						(m_back_frameline);
		}
		m_back_frameline->SetWndPos		(Fvector2().set(0,0));
		m_back_frameline->SetWndSize	(size);
	}
	else if (m_frameline_mode == Framemode_Window)
	{
		if (!m_back_framewindow)
		{
			m_back_framewindow = new CUI_IB_FrameWindow();
			m_back_framewindow->SetAutoDelete(true);
			AttachChild(m_back_framewindow);
		}
		m_back_framewindow->SetWndPos(Fvector2().set(0, 0));
		m_back_framewindow->SetWndSize(size);
	}
	else
	{
		if ( !m_background )
		{
			m_background = new CUI_IB_Static();
			m_background->SetAutoDelete		(true);
			AttachChild						(m_background);
		}
		m_background->SetWndPos			(Fvector2().set(0,0));
		m_background->SetWndSize		(size);
	}
	CUIButton::SetWndPos			(pos);
	CUIButton::SetWndSize			(size);
}

void CUI3tButton::SetWidth(float width)
{
	CUIButton::SetWidth			(width);
	if ( m_background )				{	m_background->SetWidth		(width);	}
	else if ( m_back_frameline )	{	m_back_frameline->SetWidth	(width);	}
	else if ( m_back_framewindow )	{	m_back_framewindow->SetWidth(width);	}
}

void CUI3tButton::SetHeight(float height)
{
	CUIButton::SetHeight		(height);
	if ( m_background )	{		m_background->SetHeight		(height);	}
	else if ( m_back_frameline )	{	m_back_frameline->SetHeight	(height);	}
	else if ( m_back_framewindow )	{	m_back_framewindow->SetHeight(height);	}
}

bool CUI3tButton::InitTexture(LPCSTR tex_name, bool fatal)
{
	string_path 		tex_enabled;
	string_path 		tex_disabled;
	string_path 		tex_touched;
	string_path 		tex_highlighted;

	// enabled state texture
	xr_strcpy				(tex_enabled,    tex_name);
	xr_strcat				(tex_enabled,   "_e");

	// pressed state texture
	xr_strcpy				(tex_disabled,   tex_name);
	xr_strcat				(tex_disabled,   "_d");

	// touched state texture
	xr_strcpy				(tex_touched, tex_name);
	xr_strcat				(tex_touched, "_t");

	// touched state texture
	xr_strcpy				(tex_highlighted, tex_name);
	xr_strcat				(tex_highlighted, "_h");

	this->InitTexture	(tex_enabled, tex_disabled, tex_touched, tex_highlighted, fatal);	
	return true;
}

void CUI3tButton::InitTexture(LPCSTR tex_enabled, 
							  LPCSTR tex_disabled, 
							  LPCSTR tex_touched, 
							  LPCSTR tex_highlighted,
							  bool fatal)
{
	if ( m_background )
	{
		m_background->InitState				(S_Enabled,		tex_enabled, fatal);
		m_background->InitState				(S_Disabled,	tex_disabled, fatal);
		m_background->InitState				(S_Touched,		tex_touched, fatal);
		m_background->InitState				(S_Highlighted, tex_highlighted, fatal);
	}
	else if ( m_back_frameline )
	{
		m_back_frameline->InitState				(S_Enabled,		tex_enabled, fatal);
		m_back_frameline->InitState				(S_Disabled,	tex_disabled, fatal);
		m_back_frameline->InitState				(S_Touched,		tex_touched, fatal);
		m_back_frameline->InitState				(S_Highlighted, tex_highlighted, fatal);
	}
	else if (m_back_framewindow)
	{
		m_back_framewindow->InitState(S_Enabled, tex_enabled, fatal);
		m_back_framewindow->InitState(S_Disabled, tex_disabled, fatal);
		m_back_framewindow->InitState(S_Touched, tex_touched, fatal);
		m_back_framewindow->InitState(S_Highlighted, tex_highlighted, fatal);
	}

	this->m_bTextureEnable = true;
}

void CUI3tButton::SetTextureOffset(float x, float y)
{
	if ( m_background )
	{
		this->m_background->SetTextureOffset(x, y);
	}
}

void  CUI3tButton::Draw()
{
	inherited::Draw();
}

void CUI3tButton::DrawTexture()
{
	if ( m_bTextureEnable )
	{
		if ( m_background )				
		{
			m_background->SetStretchTexture(true);
			m_background->Draw();		
		}
		else if ( m_back_frameline )	
		{	
			m_back_frameline->Draw();	
		}
		else if (m_back_framewindow)
		{
			m_back_framewindow->Draw();
		}
	}
}

void CUI3tButton::Update()
{
	inherited::Update();

	if ( m_bTextureEnable )
	{
		if ( !m_bIsEnabled )
		{
			if ( m_background )				{	m_background->SetCurrentState( S_Disabled );	}
			else if ( m_back_frameline )	{	m_back_frameline->SetCurrentState( S_Disabled ); }
			else if ( m_back_framewindow )	{	m_back_framewindow->SetCurrentState( S_Disabled ); }
		}
		else if ( CUIButton::BUTTON_PUSHED == GetButtonState() )
		{
			if ( m_background )				{	m_background->SetCurrentState( S_Touched );		}
			else if ( m_back_frameline )	{	m_back_frameline->SetCurrentState( S_Touched );	}
			else if ( m_back_framewindow )	{	m_back_framewindow->SetCurrentState( S_Touched );	}
		}
		else if ( m_bCursorOverWindow )
		{
			if ( m_background )				{	m_background->SetCurrentState( S_Highlighted );		}
			else if ( m_back_frameline )	{	m_back_frameline->SetCurrentState( S_Highlighted );	}
			else if ( m_back_framewindow )	{	m_back_framewindow->SetCurrentState( S_Highlighted );	}
		}
		else
		{
			if ( m_background )				{	m_background->SetCurrentState( S_Enabled );		}
			else if ( m_back_frameline )	{	m_back_frameline->SetCurrentState( S_Enabled );	}
			else if ( m_back_framewindow )	{	m_back_framewindow->SetCurrentState( S_Enabled );	}
		}
	}

	u32 textColor;

	if (!m_bIsEnabled)
	{
		if (m_BtnStatic && m_BtnStaticParams.m_bNeedClrChanging)
			m_BtnStatic->TextItemControl()->SetTextColor(m_BtnStaticParams.m_ClrStateD);
		textColor = m_bUseTextColor[S_Disabled] ? m_dwTextColor[S_Disabled] : m_dwTextColor[S_Enabled];
	}
	else if (CUIButton::BUTTON_PUSHED == GetButtonState())
	{
		if (m_BtnStatic && m_BtnStaticParams.m_bNeedClrChanging)
			m_BtnStatic->TextItemControl()->SetTextColor(m_BtnStaticParams.m_ClrStateT);
		textColor = m_bUseTextColor[S_Touched] ? m_dwTextColor[S_Touched] : m_dwTextColor[S_Enabled];
	}
	else if (m_bCursorOverWindow)
	{
		if (m_BtnStatic && m_BtnStaticParams.m_bNeedClrChanging)
			m_BtnStatic->TextItemControl()->SetTextColor(m_BtnStaticParams.m_ClrStateH);
		textColor = m_bUseTextColor[S_Highlighted] ? m_dwTextColor[S_Highlighted] : m_dwTextColor[S_Enabled];
	}
	else
	{
		if (m_BtnStatic && m_BtnStaticParams.m_bNeedClrChanging)
			m_BtnStatic->TextItemControl()->SetTextColor(m_BtnStaticParams.m_ClrStateE);
		textColor = m_dwTextColor[S_Enabled];
	}

	TextItemControl()->SetTextColor		(textColor);
}

void CUI3tButton::SetBtnStaticClrE(u32 clr)
{
	m_BtnStaticParams.m_ClrStateD = clr;
}

void CUI3tButton::SetBtnStaticClrD(u32 clr)
{
	m_BtnStaticParams.m_ClrStateD = clr;
}

void CUI3tButton::SetBtnStaticClrT(u32 clr)
{
	m_BtnStaticParams.m_ClrStateT = clr;
}

void CUI3tButton::SetBtnStaticClrH(u32 clr)
{
	m_BtnStaticParams.m_ClrStateH = clr;
}

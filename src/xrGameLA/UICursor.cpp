#include "stdafx.h"
#include "uicursor.h"

#include "ui/UIStatic.h"
#include "ui/UIBtnHint.h"


#define C_DEFAULT	color_xrgb(0xff,0xff,0xff)

CUICursor::CUICursor()
:m_static(nullptr),m_b_use_win_cursor(false)
{    
	bVisible				= false;
	vPrevPos.set			(0.0f, 0.0f);
	vPos.set				(0.f,0.f);
	InitInternal			();
	Device.seqRender.Add	(this,-3/*2*/);
	Device.seqResolutionChanged.Add(this);
}
//--------------------------------------------------------------------
CUICursor::~CUICursor	()
{
	xr_delete				(m_static);
	Device.seqRender.Remove	(this);
	Device.seqResolutionChanged.Remove(this);
}

void CUICursor::OnScreenResolutionChanged()
{
	xr_delete					(m_static);
	InitInternal				();
}

void CUICursor::InitInternal()
{
	m_static					= new CUIStatic();
	m_static->InitTextureEx		("ui\\ui_ani_cursor", "hud\\cursor");
	Frect						rect;
	rect.set					(0.0f,0.0f,40.0f,40.0f);
	m_static->SetTextureRect	(rect);
	Fvector2					sz;
	sz.set						(rect.rb);
	sz.x						*= UI().get_current_kx();

	m_static->SetWndSize		(sz);
	m_static->SetStretchTexture	(true);

	u32 screen_size_x	= GetSystemMetrics( SM_CXSCREEN );
	u32 screen_size_y	= GetSystemMetrics( SM_CYSCREEN );
	m_b_use_win_cursor	= (screen_size_y >=Device.TargetHeight && screen_size_x>=Device.TargetWidth);
}

void CUICursor::Show()
{
	if (bVisible)
		return;

	u32 screenWidth = psCurrentVidMode[0];
	u32 screenHeight = psCurrentVidMode[1];

	SetUICursorPosition(Fvector2().set(512.0f, 384.0f));
	SDL_WarpMouseInWindow(g_AppInfo.Window, screenWidth / 2, screenHeight / 2);

	bVisible = true;
}

//--------------------------------------------------------------------
u32 last_render_frame = 0;
void CUICursor::OnRender	()
{
	g_btnHint->OnRender();
	g_statHint->OnRender();

	if( !IsVisible() ) return;
#ifdef DEBUG
	VERIFY(last_render_frame != Device.dwFrame);
	last_render_frame = Device.dwFrame;

	if(bDebug)
	{
	CGameFont* F		= UI().Font().pFontDI;
	F->SetAligment		(CGameFont::alCenter);
	F->SetHeight		(0.02f);
	F->OutSetI			(0.f,-0.9f);
	F->SetColor			(0xffffffff);
	Fvector2			pt = GetCursorPosition();
	F->OutNext			("%f-%f",pt.x, pt.y);
	}
#endif

	m_static->SetWndPos	(vPos);
	m_static->Update	();
	m_static->Draw		();
}

Fvector2 CUICursor::GetCursorPosition()
{
	return  vPos;
}

Fvector2 CUICursor::GetCursorPositionDelta()
{
	Fvector2 res_delta;

	res_delta.x = vPos.x - vPrevPos.x;
	res_delta.y = vPos.y - vPrevPos.y;
	return res_delta;
}

void CUICursor::UpdateCursorPosition(int _dx, int _dy)
{
	if (!CImGuiManager::Instance().IsCapturingInputs())
	{
		vPrevPos = vPos;
		SDL_GetMouseState(&vPos.x, &vPos.y);
		vPos.x = vPos.x * (UI_BASE_WIDTH / (float)Device.TargetWidth);
		vPos.y = vPos.y * (UI_BASE_HEIGHT / (float)Device.TargetHeight);
		clamp(vPos.x, 0.f, UI_BASE_WIDTH);
		clamp(vPos.y, 0.f, UI_BASE_HEIGHT);
	}
}

void CUICursor::SetUICursorPosition(Fvector2 pos)
{
	if (!CImGuiManager::Instance().IsCapturingInputs())
	{
		vPos = pos;
	}
}

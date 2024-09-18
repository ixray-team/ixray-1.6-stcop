#include "stdafx.h"
#include "uicursor.h"

#include "ui/UIStatic.h"
#include "ui/UIBtnHint.h"
#include "ui/UIXmlInit.h"
#include <UIHelper.h>


constexpr auto C_DEFAULT = color_xrgb(0xff, 0xff, 0xff);

CUICursor::CUICursor()
:m_static(nullptr)
{    
	bVisible				= false;
	vPrevPos.set			(0.0f, 0.0f);
	vPos.set				(0.f,0.f);
	InitInternal			();
	Device.seqRender.Add	(this,-1023/*2*/);
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

void CUICursor::InitInternal()
{
	CUIXml xml_doc;
	xml_doc.Load(CONFIG_PATH, UI_PATH, "cursor.xml");
	m_static = UIHelper::CreateStatic(xml_doc, "cursor", nullptr);
	m_static->SetWidth(m_static->GetWidth() * UI().get_current_kx());
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
	return vPos;
}

Fvector2 CUICursor::GetCursorPositionDelta()
{
	return { vPos.x - vPrevPos.x, vPos.y - vPrevPos.y };
}

void CUICursor::UpdateCursorPosition(int _dx, int _dy)
{
	if (!CImGuiManager::Instance().IsCapturingInputs())
	{
		vPrevPos = vPos;
		int mouseX, mouseY;
		SDL_GetMouseState(&mouseX, &mouseY);
		vPos.x = static_cast<float>(mouseX) * (UI_BASE_WIDTH / static_cast<float>(Device.TargetWidth));
		vPos.y = static_cast<float>(mouseY) * (UI_BASE_HEIGHT / static_cast<float>(Device.TargetHeight));
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

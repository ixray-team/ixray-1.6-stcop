#include "stdafx.h"
#include "UICursor.h"
#include "../xrEngine/xr_input.h"
#include "Widgets/UIStatic.h"
#include "Widgets/UI3dStatic.h"
#include "Widgets/UIBtnHint.h"
#include "UIXmlInit.h"
#include <UIHelper.h>
#include "../xrEngine/Render.h"
#include "../Include/xrRender/KinematicsAnimated.h"

ENGINE_API extern bool ui_3d_cursor;
extern ENGINE_API float	psMouseSens;
extern ENGINE_API float	psMouseUISens;

CUICursor::CUICursor()
    : m_static(nullptr),
    m_3dstatic(nullptr)
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
    xr_delete(m_static);
    xr_delete(m_3dstatic);
    Device.seqRender.Remove(this);
    Device.seqResolutionChanged.Remove(this);
}

void CUICursor::OnScreenResolutionChanged()
{
    xr_delete(m_static);
    xr_delete(m_3dstatic);
    InitInternal();
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
	xml_doc.Load(CONFIG_PATH, UI_PATH, "backend\\cursor.xml");
	m_static = UIHelper::CreateStatic(xml_doc, "cursor", nullptr);

	bool native_scale = xml_doc.ReadAttribBool("cursor", 0, "native_scale", false);
	if (native_scale)
	{
		m_static->SetWidth(m_static->GetWidth() * UI().get_current_kx());
		m_static->SetHeight(m_static->GetHeight() * UI().get_current_kx());
	}
	m_static->SetWidth(m_static->GetWidth() * UI().get_current_kx());

	m_static_text = new CUIStatic();
	m_static_text->SetWndSize(Fvector2().set(80.f, 10.f));
	m_static_text->SetWndPos(Fvector2().set(m_static->GetWidth(), m_static->GetHeight()));
	m_static_text->TextItemControl()->SetTextComplexMode(true);
	m_static->AttachChild(m_static_text);

	if (xml_doc.NavigateToNode("cursor_3d", 0))
	{
		m_3dstatic = UIHelper::Create3dStatic(xml_doc, "cursor_3d", nullptr);
		bool bUseModel = xml_doc.ReadAttribBool("cursor_3d", 0, "use_model", true);
		if (bUseModel)
		{
			XML_NODE* pNode = xml_doc.NavigateToNode("cursor_3d");
			xml_doc.SetLocalRoot(pNode);
			const char* model = xml_doc.Read("visual", 0, nullptr);
			float scale = xml_doc.ReadAttribFlt(pNode, "visual", 0, "scale", 1.f);
			m_3dstatic->SetVisual(model);
			m_3dstatic->SetScaleFactor(scale);
		}
		bool native_scale_3d = xml_doc.ReadAttribBool("cursor_3d", 0, "native_scale", false);
		if (native_scale_3d)
		{
			m_3dstatic->SetWidth(m_3dstatic->GetWidth() * UI().get_current_kx());
			m_3dstatic->SetHeight(m_3dstatic->GetHeight() * UI().get_current_kx());
		}
		m_3dstatic->SetWidth(m_3dstatic->GetWidth() * UI().get_current_kx());
	}
}

//--------------------------------------------------------------------
u32 last_render_frame = 0;
void CUICursor::OnRender	()
{
	if (pInput->GetControllerMode())
	{
		return;
	}

	if (last_render_frame == Device.dwFrame)
	{
		return;
	}

	last_render_frame = Device.dwFrame;

	g_btnHint->OnRender();
	g_statHint->OnRender();

	if( !IsVisible() ) return;
#ifdef DEBUG

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

    if (ui_3d_cursor && m_3dstatic != nullptr)
    {
        m_3dstatic->SetWndPos(vPos);
        m_3dstatic->Update();
        m_3dstatic->Draw();
    }
    else
    {
        m_static->SetWndPos(vPos);
        m_static->Update();
        m_static->Draw();
    }
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

		if (psDeviceFlags.test(rsFullscreen))
		{
			float sens = psMouseUISens;
			vPos.x += _dx * sens;
			vPos.y += _dy * sens;
		}
		else
		{
			SDL_GetMouseState(&vPos.x, &vPos.y);
			vPos.x = vPos.x * (UI_BASE_WIDTH / (float)Device.TargetWidth);
			vPos.y = vPos.y * (UI_BASE_HEIGHT / (float)Device.TargetHeight);
		}

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
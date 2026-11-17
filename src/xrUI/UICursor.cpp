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
	Device.seqRender.Add	(this, UI_CURSOR);
	Device.seqResolutionChanged.Add(this);
}
//--------------------------------------------------------------------
CUICursor::~CUICursor	()
{
	Hide();
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
	OnInputActivity();
}

void CUICursor::InitInternal()
{
	CUIXml xml_doc;
	xml_doc.Load(CONFIG_PATH, UI_PATH, "backend\\cursor.xml");
	m_static = UIHelper::CreateStatic(xml_doc, "cursor", nullptr);
	OriginalTextureOffset = m_static->GetTextureOffeset();

	const bool IsConfigured = pSettings->section_exist("ui_cursor") && pSettings->read_if_exists<bool>("ui_cursor", "enabled", false);
	DefaultTexture = xml_doc.Read("cursor:texture", 0, "ui\\ui_ani_cursor");
	TextureShader = xml_doc.ReadAttrib("cursor:texture", 0, "shader", "hud\\cursor");
	if (IsConfigured)
	{
		const char* CursorTexture = pSettings->read_if_exists<const char*>("ui_cursor", "cursor_texture", "");
		if (CursorTexture && *CursorTexture)
		{
			DefaultTexture = CursorTexture;
		}
	}
	ApplyCursorTexture();
	const Fvector2 TextureSize = IsConfigured ? pSettings->read_if_exists<Fvector2>("ui_cursor", "cursor_source_texture_size", Fvector2().set(0.0f, 0.0f)) : Fvector2().set(0.0f, 0.0f);
	const Fvector2 CursorSize = IsConfigured ? pSettings->read_if_exists<Fvector2>("ui_cursor", "cursor_target_size", Fvector2().set(0.0f, 0.0f)) : Fvector2().set(0.0f, 0.0f);
	const bool HasTextureSize = TextureSize.x > 0.0f && TextureSize.y > 0.0f && _valid(TextureSize.x) && _valid(TextureSize.y);
	const bool HasCursorSize = CursorSize.x > 0.0f && CursorSize.y > 0.0f && _valid(CursorSize.x) && _valid(CursorSize.y);
	if (HasTextureSize)
	{
		// Source pixels and UI dimensions are independent; preserve the XML texture origin.
		Frect TextureRect = m_static->GetTextureRect();
		TextureRect.x2 = TextureRect.x1 + TextureSize.x;
		TextureRect.y2 = TextureRect.y1 + TextureSize.y;
		m_static->SetTextureRect(TextureRect);
	}
	if (HasCursorSize)
	{
		m_static->SetWndSize(CursorSize);
	}
	if (IsConfigured)
	{
		m_static->SetStretchTexture(pSettings->read_if_exists<bool>("ui_cursor", "stretch_cursor_texture", m_static->GetStretchTexture()));
	}
	Hotspot = IsConfigured ? pSettings->read_if_exists<Fvector2>("ui_cursor", "cursor_hotspot_offset", Fvector2().set(0.0f, 0.0f)) : Fvector2().set(0.0f, 0.0f);
	if (!_valid(Hotspot.x) || !_valid(Hotspot.y))
	{
		Hotspot.set(0.0f, 0.0f);
	}
	ConfigureAnimations();

	bool native_scale = xml_doc.ReadAttribBool("cursor", 0, "native_scale", false);
	if (native_scale)
	{
		m_static->SetWidth(m_static->GetWidth() * UI().get_current_kx());
		m_static->SetHeight(m_static->GetHeight() * UI().get_current_kx());
	}
	m_width_initial = m_static->GetWidth(); 
	m_static->SetWidth(m_width_initial * UI().get_current_kx());

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
	{
		xrCriticalSectionGuard Lock(AnimationGuard);
		if (IsTextureChangePending)
		{
			ApplyCursorTexture();
		}
	}
	UpdateAnimation();
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
        m_3dstatic->DrawWithTransform();
    }
    else
    {
		const Frect& TextureRect = m_static->GetTextureRect();
		const float ScaleX = m_static->GetStretchTexture() && TextureRect.width() > 0.0f ? m_static->GetWidth() / TextureRect.width() : 1.0f;
		const float ScaleY = m_static->GetStretchTexture() && TextureRect.height() > 0.0f ? m_static->GetHeight() / TextureRect.height() : 1.0f;
		m_static->SetTextureOffset(OriginalTextureOffset.x - Hotspot.x * ScaleX, OriginalTextureOffset.y - Hotspot.y * ScaleY);
		m_static->SetWndPos(vPos);
        m_static->Update();
        m_static->DrawWithTransform();
    }
}

void CUICursor::SetCursorTexture(const char* Texture)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	const shared_str Next = Texture ? Texture : "";
	if (Next != TextureOverride)
	{
		TextureOverride = Next;
		IsTextureChangePending = true;
	}
}

void CUICursor::ApplyCursorTexture()
{
	// Texture resources are changed only on the UI/render thread, never by the Lua caller.
	const Frect OriginalRect = m_static->GetTextureRect();
	m_static->InitTextureEx(TextureOverride.size() ? *TextureOverride : *DefaultTexture, *TextureShader);
	m_static->SetTextureRect(OriginalRect);
	IsTextureChangePending = false;
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
	if (_dx || _dy)
	{
		OnInputActivity();
	}
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
		if (pos.x != vPos.x || pos.y != vPos.y)
		{
			OnInputActivity();
		}
		vPos = pos;
	}
}

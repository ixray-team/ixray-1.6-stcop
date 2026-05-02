#include "stdafx.h"
#include "UIStatic.h"
#include "UITextureMaster.h"
#include "../../xrEngine/LightAnimLibrary.h"
#include "UILines.h"
#include "../ui_base.h"

#include "../Include/xrRender/UIRender.h"

#include "UIBtnHint.h"
#include "UICursor.h"

bool is_in2(const Frect& b1, const Frect& b2);

namespace
{
	constexpr Fvector2 kOutlineDirs8[8] = {
		{-1.0f, -1.0f}, { 0.0f, -1.0f}, { 1.0f, -1.0f},
		{-1.0f,  0.0f},                 { 1.0f,  0.0f},
		{-1.0f,  1.0f}, { 0.0f,  1.0f}, { 1.0f,  1.0f}
	};
}

void lanim_cont::set_defaults()
{
	m_lanim					= nullptr;	
	m_lanim_start_time		= -1.0f;
	m_lanim_delay_time		= 0.0f;
	m_lanimFlags.zero		();
}
void lanim_cont_xf::set_defaults()
{
	lanim_cont::set_defaults();
	m_origSize.set			(0,0);
}

CUIStatic:: CUIStatic()
:m_bTextureEnable(true),
m_bTextEnable(true),
m_bStretchTexture(false),
m_bHeading(false),
m_bConstHeading(false),
m_fHeading(0.0f),
m_pTextControl(nullptr),
m_textureShadowEnabled(false),
m_textureShadowThickness(0.0f),
m_textureShadowColor(0)
{
	m_TextureOffset.set		(0.0f,0.0f);
	m_lanim_xform.set_defaults	();
	m_bEnableTextHighlighting = false;
}

void CUIStatic::SetTextureShadow(bool enabled, float thickness, u32 color)
{
	m_textureShadowEnabled = enabled;
	m_textureShadowThickness = thickness;
	m_textureShadowColor = color;
}

CUIStatic::~CUIStatic()
{
	xr_delete(m_pTextControl);
}

void CUIStatic::SetXformLightAnim(const char* lanim, bool bCyclic)
{
	if(lanim && lanim[0]!=0)
		m_lanim_xform.m_lanim			= LALib.FindItem(lanim);
	else
		m_lanim_xform.m_lanim			= nullptr;
	
	m_lanim_xform.m_lanimFlags.zero		();

	m_lanim_xform.m_lanimFlags.set		(LA_CYCLIC,			bCyclic);
	m_lanim_xform.m_origSize			= GetWndSize();
}

bool CUIStatic::InitTexture(const char* texture, bool fatal)
{
    return InitTextureEx(texture, "hud\\default", fatal);
}

bool CUIStatic::InitTexture(const char* raster_texture_name, const char* svg_texture_name)
{
	(void)raster_texture_name;
	bool result = CUITextureMaster::InitTexture(svg_texture_name, &m_UIStaticItem, GetWidth(), GetHeight(), _svgBinding.GetTint());

	Fvector2 p = GetWndPos();
	m_UIStaticItem.SetPos(p.x, p.y);

	return result;
}

void CUIStatic::InitSVG(CUIXml& xml_doc, const char* path, int index)
{
	_svgBinding.LoadFromXml(xml_doc, path, index);
}

bool CUIStatic::isSVGPresented(void) const
{
	return _svgBinding.IsActive();
}

const char* CUIStatic::getSVGFilename(CUIXml& xml_doc, const char* path, int index)
{
	(void)xml_doc;
	(void)path;
	(void)index;
	R_ASSERT(_svgBinding.IsActive() && "must be initialized!");
	return _svgBinding.GetFileName();
}

void CUIStatic::CreateShader(const char* tex, const char* sh)
{
    m_UIStaticItem.CreateShader(tex, sh);
}

bool CUIStatic::InitTextureEx(const char* texture, const char* shader, bool fatal)
{
	if (texture == nullptr)
	{
		return false;
	}

	const char* res_shname = UIRender->UpdateShaderName(texture, shader);
    bool result = CUITextureMaster::InitTexture(texture, &m_UIStaticItem, res_shname, fatal);

    Fvector2 p = GetWndPos();
    m_UIStaticItem.SetPos(p.x, p.y);
    return result;
}

void CUIStatic::Draw()
{
	DrawTexture();
	inherited::Draw();
	DrawText();
}

void CUIStatic::DrawText()
{
	if (!m_bTextEnable)
		return;

	if (m_pTextControl)
	{
		if( !fsimilar(m_pTextControl->m_wndSize.x, m_wndSize.x) || !fsimilar(m_pTextControl->m_wndSize.y, m_wndSize.y))
		{
			m_pTextControl->m_wndSize		= m_wndSize;
			m_pTextControl->ParseText		(true);
		}

		if (IsHighlightText() && xr_strlen(TextItemControl()->GetText()) > 0 && m_bEnableTextHighlighting)
			DrawHighlightedText();
		else
		{
			Fvector2			p;
			GetAbsolutePos(p);
			m_pTextControl->Draw(p.x, p.y);
		}
	}
	if(g_statHint->Owner()==this)
		g_statHint->Draw_();
}

#include "../../Include/xrRender/UIShader.h"

void CUIStatic::DrawTexturePass(u32 color, const Fvector2& extraOffset)
{
	Frect rect;
	GetAbsoluteRect(rect);
	m_UIStaticItem.SetPos(
		rect.left + m_TextureOffset.x + extraOffset.x,
		rect.top + m_TextureOffset.y + extraOffset.y);

	if (m_bStretchTexture)
	{
		if (Heading())
		{
			if (m_UIStaticItem.GetFixedLTWhileHeading())
			{
				const float t1 = rect.width();
				const float t2 = rect.height();
				rect.y2 = rect.y1 + t1;
				rect.x2 = rect.x1 + t2;
			}
		}
		m_UIStaticItem.SetSize(Fvector2().set(rect.width(), rect.height()));
	}
	else
	{
		Frect r = { 0.0f, 0.0f,
			m_UIStaticItem.GetTextureRect().width(),
			m_UIStaticItem.GetTextureRect().height() };

		if (Heading())
		{
			const float t1 = rect.width();
			const float t2 = rect.height();
			rect.y2 = rect.y1 + t1;
			rect.x2 = rect.x1 + t2;
		}

		m_UIStaticItem.SetSize(Fvector2().set(r.width(), r.height()));
	}

	const u32 prevColor = m_UIStaticItem.GetTextureColor();
	m_UIStaticItem.SetTextureColor(color);

	if (Heading())
		m_UIStaticItem.Render(GetHeading());
	else
		m_UIStaticItem.Render();

	m_UIStaticItem.SetTextureColor(prevColor);
}

void CUIStatic::DrawTexture()
{
	if (!m_bTextureEnable || !GetShader() || !GetShader()->inited())
		return;

	if (m_textureShadowEnabled && m_textureShadowThickness > 0.0f)
	{
		for (const Fvector2& d : kOutlineDirs8)
		{
			DrawTexturePass(m_textureShadowColor,
				Fvector2().set(d.x * m_textureShadowThickness, d.y * m_textureShadowThickness));
		}
	}

	DrawTexturePass(GetTextureColor(), Fvector2().set(0.0f, 0.0f));
}

void CUIStatic::Update()
{
	inherited::Update();
	//update light animation if defined
	UpdateColorAnimation();

	if (m_expression.IsCompiled())
	{
		ExpressionVarVariadic Result = m_expression.ExecuteExpression();

		xr_string NewText;
		switch (Result.VarType)
		{
		case ExpressionVarVariadic::EVariadicType::eFloat:	NewText = xr_string::ToString(Result.Flt); break;
		case ExpressionVarVariadic::EVariadicType::eStr:	NewText = Result.Str.c_str(); break;
		case ExpressionVarVariadic::EVariadicType::eInt:	NewText = xr_string::ToString(Result.Int); break;
		case ExpressionVarVariadic::EVariadicType::eBool:	NewText = Result.Boolean ? "true" : "false"; break;
		}

		m_pTextControl->SetText(NewText.c_str());
	}

	if(m_lanim_xform.m_lanim)
	{
		if(m_lanim_xform.m_lanim_start_time<0.0f)
			ResetXformAnimation();

		float t = Device.dwTimeGlobal/1000.0f;

		if(	m_lanim_xform.m_lanimFlags.test(LA_CYCLIC) || 
			t - m_lanim_xform.m_lanim_start_time < m_lanim_xform.m_lanim->Length_sec() )
		{
			int frame;
			u32 clr				= m_lanim_xform.m_lanim->CalculateRGB(t-m_lanim_xform.m_lanim_start_time,frame);
			
			EnableHeading_int	(true);
			float heading		= (PI_MUL_2/255.0f) * color_get_A(clr);
			SetHeading			(heading);

			float _value		= (float)color_get_R(clr);
			
			float f_scale		= _value / 64.0f;
			Fvector2 _sz;
			_sz.set				(m_lanim_xform.m_origSize.x*f_scale, m_lanim_xform.m_origSize.y*f_scale );
			SetWndSize			(_sz);
		}else
		{
			EnableHeading_int	( m_bHeading );
			SetWndSize			(m_lanim_xform.m_origSize);
		}
	}

	if(CursorOverWindow() && m_stat_hint_text.size() && !g_statHint->Owner() && Device.dwTimeContinual>m_dwFocusReceiveTime+700)
	{
		g_statHint->SetHintText	(this, m_stat_hint_text.c_str());

		Fvector2 c_pos			= GetUICursor().GetCursorPosition();
		Frect vis_rect;
		vis_rect.set			(0,0,UI_BASE_WIDTH, UI_BASE_HEIGHT);

		//select appropriate position
		Frect r;
		r.set					(0.0f, 0.0f, g_statHint->GetWidth(), g_statHint->GetHeight());
		r.add					(c_pos.x, c_pos.y);

		r.sub					(0.0f,r.height());
		if (false==is_in2(vis_rect,r))
			r.sub				(r.width(),0.0f);
		if (false==is_in2(vis_rect,r))
			r.add				(0.0f,r.height());

		if (false==is_in2(vis_rect,r))
			r.add				(r.width(), 45.0f);

		g_statHint->SetWndPos(r.lt);
	}
}

void CUIStatic::ResetXformAnimation()
{
	m_lanim_xform.m_lanim_start_time = Device.dwTimeGlobal/1000.0f;
}

void  CUIStatic::SetShader(const ui_shader& sh)
{
	m_UIStaticItem.SetShader(sh);
}

CUILines* CUIStatic::TextItemControl()
{
	if (!m_pTextControl) 
	{
		m_pTextControl = new CUILines(); 
		m_pTextControl->SetTextAlignment(CGameFont::alLeft);
	}
	return m_pTextControl;
}

void CUIStatic::AdjustHeightToText()
{
	if( !fsimilar(TextItemControl()->m_wndSize.x, GetWidth()) )
	{
		TextItemControl()->m_wndSize.x = GetWidth();
		TextItemControl()->ParseText(true);
	}
	SetHeight				(TextItemControl()->GetVisibleHeight());
}

void CUIStatic::AdjustWidthToText()
{
	if(!m_pTextControl)	return;
	float _len		= m_pTextControl->GetFont()->SizeOf_(m_pTextControl->GetText());
	UI().ClientToScreenScaledWidth(_len);
	SetWidth		(_len);
}

void CUIStatic::ColorAnimationSetTextureColor(u32 color, bool only_alpha)
{
	SetTextureColor( (only_alpha)?subst_alpha(GetTextureColor(),color) : color);
}

void CUIStatic::ColorAnimationSetTextColor(u32 color, bool only_alpha)
{
	TextItemControl()->SetTextColor( (only_alpha)?subst_alpha(TextItemControl()->GetTextColor(),color) : color);
}


void CUIStatic::OnFocusLost()
{
	inherited::OnFocusLost();
	
	if(g_statHint->Owner()==this)
		g_statHint->Discard	();
}

void CUIStatic::DrawHighlightedText() {
	Frect rect;
	GetAbsoluteRect(rect);
	u32 def_col = TextItemControl()->GetTextColor();
	TextItemControl()->SetTextColor(m_HighlightColor);
	/*
		m_pLines->Draw(	rect.left + 1 + m_iTextOffsetX, rect.top + 1 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left - 1 + m_iTextOffsetX, rect.top - 1 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left - 1 + m_iTextOffsetX, rect.top + 1 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left + 1 + m_iTextOffsetX, rect.top - 1 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left + 1 + m_iTextOffsetX, rect.top + 0 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left - 1 + m_iTextOffsetX, rect.top - 0 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left - 0 + m_iTextOffsetX,	rect.top + 1 + m_iTextOffsetY);
		m_pLines->Draw(	rect.left + 0 + m_iTextOffsetX, rect.top - 1 + m_iTextOffsetY);
	*/
	TextItemControl()->Draw(rect.left + 0 + TextItemControl()->m_TextOffset.x, rect.top - 0 + TextItemControl()->m_TextOffset.y);
	TextItemControl()->SetTextColor(def_col);
}

bool CUIStatic::IsHighlightText()
{
	return m_bCursorOverWindow;
}

void CUIStatic::SetFont(CGameFont* pFont) 
{
	CUIWindow::SetFont(pFont);
	TextItemControl()->SetFont(pFont);
}

CGameFont* CUIStatic::GetFont() 
{
	return TextItemControl()->GetFont();
}

void CUIStatic::SetText(const char* txt)
{
	TextItemControl()->SetText(txt); 
}

void CUIStatic::SetTextIfNodeExist(const char* txt)
{
	if (!m_text_control_exists)
		return;

	TextItemControl()->SetText(txt);
}

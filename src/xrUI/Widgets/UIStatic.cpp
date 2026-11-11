#include "stdafx.h"
#include "UIStatic.h"
#include "UITextureMaster.h"
#include "../../xrEngine/LightAnimLibrary.h"
#include "UILines.h"
#include "../ui_base.h"

#include "../Include/xrRender/UIRender.h"

#include "UIBtnHint.h"
#include "UICursor.h"
#include "../../xrEngine/xr_input.h"

bool is_in2(const Frect& b1, const Frect& b2);

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
	m_HighlightColor = 0xffffffff;
	m_text_control_exists = false;
}

void CUIStatic::SetTextureShadow(bool enabled, float thickness, u32 color)
{
	m_textureShadowEnabled = enabled;
	m_textureShadowThickness = thickness;
	m_textureShadowColor = color;
}

void CUIStatic::SetBlurredTextureShadow(bool Enabled, float Blur, float Opacity, float Radius, float LightX, float LightY)
{
	const auto Finite = [](float Value, float Fallback) { return std::isfinite(Value) ? Value : Fallback; };
	BlurredShadowEnabled = Enabled;
	BlurredShadowSigma = std::clamp(Finite(Blur, 0.0f), 0.0f, 128.0f);
	BlurredShadowOpacity = std::clamp(Finite(Opacity, 0.0f), 0.0f, 1.0f);
	BlurredShadowMinOpacity = std::min(BlurredShadowMinOpacity, BlurredShadowOpacity);
	BlurredShadowRadius = std::clamp(Finite(Radius, 0.0f), 0.0f, 128.0f);
	BlurredShadowLight.set(std::clamp(Finite(LightX, 512.0f), -8192.0f, 8192.0f),
		std::clamp(Finite(LightY, 0.0f), -8192.0f, 8192.0f));
}

void CUIStatic::SetBlurredTextureShadowHeight(float Height, float LightHeight)
{
	BlurredShadowHeight = std::isfinite(Height) ? std::clamp(Height, 0.0f, 1024.0f) : 4.0f;
	BlurredShadowLightHeight = std::isfinite(LightHeight) ? std::clamp(LightHeight, 1.0f, 4096.0f) : 300.0f;
}

void CUIStatic::SetBlurredTextureShadowLightRadius(float Radius)
{
	BlurredShadowLightRadius = std::isfinite(Radius) ? std::clamp(Radius, 0.0f, 4096.0f) : 0.0f;
}

void CUIStatic::SetBlurredTextureShadowFalloff(float Falloff)
{
	BlurredShadowFalloff = std::isfinite(Falloff) ? std::clamp(Falloff, 0.0f, 12.0f) : 0.0f;
}

void CUIStatic::SetBlurredTextureShadowOpacityRange(float Minimum, float Maximum)
{
	BlurredShadowOpacity = std::isfinite(Maximum) ? std::clamp(Maximum, 0.0f, 1.0f) : 0.0f;
	BlurredShadowMinOpacity = std::isfinite(Minimum) ? std::clamp(Minimum, 0.0f, BlurredShadowOpacity) : 0.0f;
}

void CUIStatic::DrawBlurredTextureShadow()
{
	if (!BlurredShadowEnabled || BlurredShadowOpacity <= 0.0f || BlurredShadowHeight >= BlurredShadowLightHeight)
	{
		return;
	}
	const u32 Alpha = color_get_A(GetTextureColor()) * color_get_A(BlurredShadowColor) / 255;
	const u32 Color = UIColors::ScaleAlpha(subst_alpha(BlurredShadowColor, Alpha), BlurredShadowOpacity);
	if (color_get_A(Color) == 0)
	{
		return;
	}
	PrepareTextureItem();
	const float ProjectionScale = BlurredShadowLightHeight / (BlurredShadowLightHeight - BlurredShadowHeight);
	m_UIStaticItem.RenderBlurredShadow(BlurredShadowLight, ProjectionScale, BlurredShadowLightHeight, BlurredShadowLightRadius, BlurredShadowRadius, BlurredShadowSigma, BlurredShadowFalloff,
		BlurredShadowMinOpacity / BlurredShadowOpacity, Color, Heading(), GetHeading(), IsAspectRatioCorrectionActive());
}

void CUIStatic::SetTextShadow(bool enabled, float thickness, u32 color)
{
	TextItemControl()->SetTextShadow(enabled, thickness, color);
}

void CUIStatic::SetTextShadow(const SUIOutlineParams& params)
{
	TextItemControl()->SetTextShadow(params);
}

CUIStatic::~CUIStatic()
{
	xr_delete(m_pTextControl);
}

void CUIStatic::SetHeading(float Angle)
{
	TargetHeading = Angle;
	if (ShouldSmoothRotate())
	{
		HeadingPending = HeadingProgress != Angle;
		if (HeadingPending)
		{
			QueueSmoothRotation();
		}
	}
	else
	{
		HeadingProgress = Angle;
		m_fHeading = Angle;
		HeadingPending = false;
	}
}

void CUIStatic::UpdateSmoothHeading(float Blend)
{
	if (HeadingPending)
	{
		HeadingPending = AdvanceSmoothHeading(HeadingProgress, TargetHeading, Blend);
		m_fHeading = static_cast<float>(HeadingProgress);
	}
}

bool CUIStatic::HitTestUntransformed(const Fvector2& Position)
{
	if (BorderEnabled)
	{
		Fvector2 Point;
		UI().ClientToScreenScaled(Point, Position.x, Position.y);
		GetTreeTransform().Apply(Point.x, Point.y);
		return GetBorderRect().Contains(Point.x, Point.y);
	}
	const bool TextureVisible = m_bTextureEnable && GetShader() && GetShader()->inited();
	const bool BackgroundVisible = !TextureVisible && color_get_A(BackgroundColor) != 0;
	if (!Heading() || (!TextureVisible && !BackgroundVisible))
	{
		return inherited::HitTestUntransformed(Position);
	}
	if (m_bTextEnable && m_pTextControl && m_pTextControl->GetText() && m_pTextControl->GetText()[0] && inherited::HitTestUntransformed(Position))
	{
		return true;
	}
	Fvector2 Origin;
	GetAbsolutePos(Origin);
	Fvector2 Size = GetWndSize();
	if (TextureVisible && !m_bStretchTexture)
	{
		Size.set(m_UIStaticItem.GetTextureRect().width(), m_UIStaticItem.GetTextureRect().height());
	}
	else if (TextureVisible && m_UIStaticItem.GetFixedLTWhileHeading())
	{
		std::swap(Size.x, Size.y);
	}
	const Fvector2 Pivot = m_UIStaticItem.uFlags.test(CUIStaticItem::flValidHeadingPivot) ?
		m_UIStaticItem.vHeadingPivot : Fvector2().set(Size.x / 2.0f, Size.y / 2.0f);
	if (BackgroundVisible)
	{
		const bool PhysicalRotation = UI().m_currentPointType != IUIRender::pttLIT && !IsAspectRatioCorrectionActive();
		const float ScaleX = PhysicalRotation ? UI().ClientToScreenScaledX(1.0f) : 1.0f;
		const float ScaleY = PhysicalRotation ? UI().ClientToScreenScaledY(1.0f) : 1.0f;
		const float X = (Position.x - Origin.x - m_UIStaticItem.vHeadingOffset.x - Pivot.x) * ScaleX;
		const float Y = (Position.y - Origin.y - m_UIStaticItem.vHeadingOffset.y - Pivot.y) * ScaleY;
		const float Cos = std::cos(m_fHeading);
		const float Sin = std::sin(m_fHeading);
		const float LocalX = (X * Cos - Y * Sin) / ScaleX + Pivot.x;
		const float LocalY = (X * Sin + Y * Cos) / ScaleY + Pivot.y;
		return LocalX >= 0.0f && LocalY >= 0.0f && LocalX <= Size.x && LocalY <= Size.y;
	}
	const float KX = IsAspectRatioCorrectionActive() ? 1.0f : UI().get_current_kx();
	const float X = (Position.x - Origin.x - m_TextureOffset.x - m_UIStaticItem.vHeadingOffset.x - Pivot.x) / KX;
	const float Y = Position.y - Origin.y - m_TextureOffset.y - m_UIStaticItem.vHeadingOffset.y - Pivot.y;
	const float Cos = std::cos(m_fHeading);
	const float Sin = std::sin(m_fHeading);
	const float LocalX = X * Cos - Y * Sin + Pivot.x;
	const float LocalY = X * Sin + Y * Cos + Pivot.y;
	return LocalX >= 0.0f && LocalY >= 0.0f && LocalX <= Size.x && LocalY <= Size.y;
}

bool CUIStatic::OnMouseAction(float X, float Y, EUIMessages MouseAction)
{
	if (inherited::ProcessMouseAction(X, Y, MouseAction))
	{
		return true;
	}

	if (!MouseEventsEnabled || !IsEnabled() || !IsShown())
	{
		return false;
	}

	// Input can still reach a previously hovered window after the cursor leaves it.
	Fvector2 Position;
	GetAbsolutePos(Position);
	Position.add(GetParent() ? Fvector2().set(X, Y) : cursor_pos);
	if (!HitTestUntransformed(Position))
	{
		return false;
	}

	switch (MouseAction)
	{
	case WINDOW_LBUTTON_DOWN:
		SendMessageToTarget(STATIC_MOUSE_LEFT_CLICK);
		break;
	case WINDOW_LBUTTON_DB_CLICK:
		SendMessageToTarget(STATIC_MOUSE_DOUBLE_CLICK);
		break;
	case WINDOW_RBUTTON_DOWN:
		SendMessageToTarget(STATIC_MOUSE_RIGHT_CLICK);
		break;
	case WINDOW_MOUSE_MOVE:
	{
		const bool Dragging = pInput && (pInput->iGetAsyncBtnState(0) || pInput->iGetAsyncBtnState(1));
		if (!SendMessageToTarget(STATIC_MOUSE_MOVE))
		{
			return false;
		}
		if (Dragging)
		{
			SendMessageToTarget(STATIC_MOUSE_DRAG);
		}
		break;
	}
	case WINDOW_MOUSE_WHEEL_UP:
		SendMessageToTarget(STATIC_MOUSE_WHEEL_UP);
		break;
	case WINDOW_MOUSE_WHEEL_DOWN:
		SendMessageToTarget(STATIC_MOUSE_WHEEL_DOWN);
		break;
	default:
		break;
	}

	// Keep the input propagation used by statics and their derived controls.
	return false;
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

void CUIStatic::SetBackgroundOpacity(float Value)
{
	if (m_bTextureEnable && GetShader() && GetShader()->inited())
	{
		SetTextureColor(UIColors::WithAlpha(GetTextureColor(), Value));
	}
	else if (BackgroundColorAssigned)
	{
		BackgroundColor = UIColors::WithAlpha(BackgroundColor, Value);
	}
}

void CUIStatic::SetBorderOpacity(float Value)
{
	if (!BorderEnabled)
	{
		return;
	}
	BorderColor = UIColors::WithAlpha(BorderColor, Value);
}

void CUIStatic::SetBackgroundColorARGB(int A, int R, int G, int B)
{
	BackgroundColorAssigned = true;
	BackgroundColor = color_argb(std::clamp(A, 0, 255), std::clamp(R, 0, 255), std::clamp(G, 0, 255), std::clamp(B, 0, 255));
}

void CUIStatic::DrawBackground()
{
	const u32 Color = BackgroundColor;
	if (color_get_A(Color) == 0 || (m_bTextureEnable && GetShader() && GetShader()->inited()))
	{
		return;
	}

	Fvector2 Origin;
	GetAbsolutePos(Origin);
	const Fvector2 Size = GetWndSize();
	Fvector2 Pivot = m_UIStaticItem.uFlags.test(CUIStaticItem::flValidHeadingPivot) ? m_UIStaticItem.vHeadingPivot : Fvector2().set(Size.x / 2.0f, Size.y / 2.0f);
	if (Heading())
	{
		Origin.add(m_UIStaticItem.vHeadingOffset);
	}
	Pivot.add(Origin);
	const Fvector2 LogicalPivot = Pivot;
	const bool CorrectedAspect = IsAspectRatioCorrectionActive();
	UI().ClientToScreenScaled(Pivot);

	sPoly2D Polygon;
	Polygon.resize(4);
	Polygon[0].set(Origin.x, Origin.y, 0.0f, 0.0f);
	Polygon[1].set(Origin.x + Size.x, Origin.y, 0.0f, 0.0f);
	Polygon[2].set(Origin.x + Size.x, Origin.y + Size.y, 0.0f, 0.0f);
	Polygon[3].set(Origin.x, Origin.y + Size.y, 0.0f, 0.0f);
	const float Cos = std::cos(m_fHeading);
	const float Sin = std::sin(m_fHeading);
	for (auto& Vertex : Polygon)
	{
		if (Heading() && CorrectedAspect)
		{
			Vertex.rotate_pt(LogicalPivot, Cos, Sin, 1.0f);
		}
		UI().ClientToScreenScaled(Vertex.pt);
		if (Heading() && !CorrectedAspect)
		{
			Vertex.rotate_pt(Pivot, Cos, Sin, 1.0f);
		}
	}

	sPoly2D Clipped;
	sPoly2D* Result = UI().m_currentPointType == IUIRender::pttLIT ? UI().ScreenFrustumLIT().ClipPoly(Polygon, Clipped) : UI().ScreenFrustum().ClipPoly(Polygon, Clipped);
	if (!Result || Result->size() < 3)
	{
		return;
	}
	const ui_shader& Shader = UI().GetBackgroundShader();
	UIRender->SetShader(*Shader);
	UIRender->StartPrimitive((Result->size() - 2) * 3, IUIRender::ptTriList, UI().m_currentPointType);
	for (u32 Index = 1; Index + 1 < Result->size(); ++Index)
	{
		for (const u32 VertexIndex : {u32(0), Index, Index + 1})
		{
			const Fvector2& Point = (*Result)[VertexIndex].pt;
			UIRender->PushPoint(Point.x, Point.y, 0.0f, Color, 0.0f, 0.0f);
		}
	}
	UIRender->FlushPrimitive();
}

void CUIStatic::Draw()
{
	DrawBackground();
	DrawTexture();
	inherited::Draw();
	DrawWidgetText();
}

void CUIStatic::DrawWidgetText()
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

void CUIStatic::PrepareTextureItem()
{
	Frect rect;
	GetAbsoluteRect(rect);
	m_UIStaticItem.SetPos(
		rect.left + m_TextureOffset.x,
		rect.top + m_TextureOffset.y);

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

}

void CUIStatic::DrawTexturePass(u32 color, const Fvector2& extraOffset)
{
	PrepareTextureItem();
	m_UIStaticItem.vPos.add(extraOffset);
	const u32 prevColor = m_UIStaticItem.GetTextureColor();
	m_UIStaticItem.SetTextureColor(color);

	if (Heading())
		m_UIStaticItem.Render(GetHeading(), IsAspectRatioCorrectionActive());
	else
		m_UIStaticItem.Render();

	m_UIStaticItem.SetTextureColor(prevColor);
}

void CUIStatic::DrawTexture()
{
	if (!m_bTextureEnable || !GetShader() || !GetShader()->inited())
		return;

	if (BlurredShadowEnabled && BlurredShadowOpacity > 0.0f)
	{
		DrawBlurredTextureShadow();
	}
	const u32 textureColor = GetTextureColor();
	if (m_textureShadowEnabled && m_textureShadowThickness > 0.0f)
	{
		const u32 textureAlpha = color_get_A(textureColor);
		const u32 shadowAlpha = (color_get_A(m_textureShadowColor) * textureAlpha) / 255;
		if (shadowAlpha > 0)
		{
			const u32 shadowColor = subst_alpha(m_textureShadowColor, shadowAlpha);
			for (const Fvector2& d : UIOutline::kDirs8)
			{
				DrawTexturePass(shadowColor,
					Fvector2().set(d.x * m_textureShadowThickness, d.y * m_textureShadowThickness));
			}
		}
	}

	DrawTexturePass(textureColor, Fvector2().set(0.0f, 0.0f));
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
		case ExpressionVarVariadic::EVariadicType::eStr:	NewText = Result.Str.c_str() ? Result.Str.c_str() : ""; break;
		case ExpressionVarVariadic::EVariadicType::eInt:	NewText = xr_string::ToString(Result.Int); break;
		case ExpressionVarVariadic::EVariadicType::eUint:	NewText = xr_string::ToString(Result.UInt); break;
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
	if (!m_pTextControl || !m_pTextControl->GetFont())
	{
		return;
	}
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

void CUIStatic::SetBorder(float Thickness, float Radius, u32 Color)
{
	BorderEnabled = true;
	BorderThickness = std::isfinite(Thickness) ? std::max(Thickness, 0.0f) : 0.0f;
	BorderRadius = std::isfinite(Radius) ? std::max(Radius, 0.0f) : 0.0f;
	BorderColor = Color;
}

UIRoundedRect CUIStatic::GetBorderRect()
{
	UIRoundedRect Rect;
	const bool TextureVisible = m_bTextureEnable && GetShader() && GetShader()->inited();
	Fvector2 Size = GetWndSize();
	if (TextureVisible)
	{
		if (!m_bStretchTexture)
		{
			Size.set(m_UIStaticItem.GetTextureRect().width(), m_UIStaticItem.GetTextureRect().height());
		}
		else if (Heading() && m_UIStaticItem.GetFixedLTWhileHeading())
		{
			std::swap(Size.x, Size.y);
		}
	}
	Rect.Width = std::max(Size.x, 0.0f);
	Rect.Height = std::max(Size.y, 0.0f);
	const float Limit = std::min(Rect.Width, Rect.Height) * 0.5f;
	Rect.Radius = std::min(BorderRadius, Limit);
	Rect.Thickness = std::min(BorderThickness, Limit);
	Rect.Color = UIColors::ScaleAlpha(BorderColor, UIRender->VertexOpacity);
	Rect.ScreenSpace = UI().m_currentPointType != IUIRender::pttLIT;
	const float ScaleX = Rect.ScreenSpace ? UI().ClientToScreenScaledX(1.0f) : 1.0f;
	const float ScaleY = Rect.ScreenSpace ? UI().ClientToScreenScaledY(1.0f) : 1.0f;
	Fvector2 Origin;
	GetAbsolutePos(Origin);
	if (TextureVisible)
	{
		Origin.add(m_TextureOffset);
	}
	UITransform2D Local;
	Local.XX = ScaleX;
	Local.YY = ScaleY;
	Local.TX = Origin.x * ScaleX;
	Local.TY = Origin.y * ScaleY;
	if (Heading())
	{
		const Fvector2 Pivot = m_UIStaticItem.uFlags.test(CUIStaticItem::flValidHeadingPivot) ? m_UIStaticItem.vHeadingPivot : Fvector2().set(Size.x * 0.5f, Size.y * 0.5f);
		const float Cos = std::cos(m_fHeading), Sin = std::sin(m_fHeading);
		const bool CorrectedAspect = IsAspectRatioCorrectionActive();
		const float KX = TextureVisible && !CorrectedAspect ? UI().get_current_kx() : 1.0f;
		// Textures retain the legacy aspect correction; solid backgrounds rotate in draw space.
		Local.XX = ScaleX * KX * Cos;
		Local.XY = (TextureVisible || CorrectedAspect ? ScaleX * KX : ScaleY) * Sin;
		Local.YX = -(TextureVisible || CorrectedAspect ? ScaleY : ScaleX) * Sin;
		Local.YY = ScaleY * Cos;
		Local.TX += (Pivot.x + m_UIStaticItem.vHeadingOffset.x) * ScaleX - Local.XX * Pivot.x - Local.XY * Pivot.y;
		Local.TY += (Pivot.y + m_UIStaticItem.vHeadingOffset.y) * ScaleY - Local.YX * Pivot.x - Local.YY * Pivot.y;
	}
	Rect.Transform = GetTreeTransform().Compose(Local);
	if (Rect.ScreenSpace)
	{
		// A circle in logical UI coordinates becomes an ellipse on widescreen displays.
		// Evaluate the rounded contour in physical units while preserving its corners and pivot.
		const float AxisX = std::hypot(Rect.Transform.XX, Rect.Transform.YX);
		const float AxisY = std::hypot(Rect.Transform.XY, Rect.Transform.YY);
		if (AxisX > EPS_S && AxisY > EPS_S)
		{
			Rect.Width *= AxisX;
			Rect.Height *= AxisY;
			Rect.Transform.XX /= AxisX;
			Rect.Transform.YX /= AxisX;
			Rect.Transform.XY /= AxisY;
			Rect.Transform.YY /= AxisY;
			const float PhysicalLimit = std::min(Rect.Width, Rect.Height) * 0.5f;
			Rect.Radius = std::min(BorderRadius * ScaleY, PhysicalLimit);
			Rect.Thickness = std::min(BorderThickness * ScaleY, PhysicalLimit);
		}
	}
	return Rect;
}

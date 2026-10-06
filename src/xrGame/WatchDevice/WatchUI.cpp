#include "StdAfx.h"
#include "WatchUI.h"
#include "../../xrEngine/FontManager.h"
#include "../../xrEngine/string_table.h"
#include "../../xrUI/ui_base.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/xrUIXmlParser.h"

namespace
{
constexpr const char* WatchXml = "watch_hud.xml";
constexpr const char* WatchTextShader = "hud\\watch_text";
constexpr const char* WatchGlowShader = "hud\\watch_glow";
constexpr const char* WatchGlowTexShader = "hud\\watch_glow_tex";
constexpr const char* WatchGlassShader = "hud\\watch_glass";
constexpr const char* WatchLedShader = "hud\\watch_led";
constexpr u32 WatchLedSegments = 16;
constexpr u32 WatchLedMaxVerts = WatchLedSegments * 12;
constexpr u32 WatchGlassDefaultLitAlpha = 90;

constexpr float WatchGlassCapNormal = 2.0f;

struct SLedVertex
{
	Fvector P;
	Fvector2 N;
};

struct SLedCircle
{
	Fvector2 Points[WatchLedSegments + 1];

	SLedCircle()
	{
		for (u32 Index = 0; Index <= WatchLedSegments; ++Index)
		{
			const float Angle = PI_MUL_2 * float(Index) / float(WatchLedSegments);
			Points[Index].set(std::cos(Angle), std::sin(Angle));
		}
	}
};

const SLedCircle& LedCircle()
{
	static const SLedCircle Circle;
	return Circle;
}
}

bool CUIWatchText::Init(CUIXml& Xml, const char* Path, const shared_str& FontName)
{
	Color = CUIXmlInit::GetColor(Xml, Path, 0, Color);
	DrawColor = Color;

	const char* AlignText = Xml.ReadAttrib(Path, 0, "align", "l");
	if (AlignText[0] == 'c')
	{
		Align = EAlign::Center;
	}
	else if (AlignText[0] == 'r')
	{
		Align = EAlign::Right;
	}

	if (!FontName.size() || !pSettings->section_exist(FontName.c_str()) || !pSettings->line_exist(FontName.c_str(), "shader"))
	{
		Msg("! [watch] %s: font section [%s] not found or has no shader", Path, FontName.size() ? FontName.c_str() : "");
		return false;
	}

	Font = UI().Font().GetFont(FontName);

	string128 Texture = {};
	xr_sprintf(Texture, "$user$%s", FontName.c_str());
	Shader->create(Xml.ReadAttrib(Path, 0, "shader", WatchTextShader), Texture);
	return true;
}

void CUIWatchText::SetText(const char* Text)
{
	Quads.clear();
	TextWidth = 0.0f;

	u32 AtlasWidth = 0;
	u32 AtlasHeight = 0;
	if (!Font || !Text || !Font->GetAtlasTexSize(AtlasWidth, AtlasHeight))
	{
		return;
	}

	const float Cell = Font->GetHeight();
	if (Cell < EPS || !AtlasWidth || !AtlasHeight)
	{
		return;
	}

	const float InvWidth = 1.0f / float(AtlasWidth);
	const float InvHeight = 1.0f / float(AtlasHeight);

	float X = 0.0f;
	for (const char* Cursor = Text; *Cursor; ++Cursor)
	{
		const CGameFont::Glyph* Glyph = Font->GetGlyphInfo(u8(*Cursor));
		if (!Glyph)
		{
			continue;
		}

		if (Cursor != Text)
		{
			X += float(Glyph->Abc.abcA);
		}

		const float Right = X + float(Glyph->Abc.abcB);
		TextWidth = Right;

		const RECT& Tc = Glyph->TextureCoord;
		SGlyphQuad& Quad = Quads.emplace_back();
		Quad.Pos.set(X, float(Glyph->yOffset), Right, Cell + float(Glyph->yOffset));
		Quad.Uv.set(
			float(Tc.left) * InvWidth,
			float(Tc.top) * InvHeight,
			float(Tc.right) * InvWidth,
			float(Tc.bottom) * InvHeight
		);

		X = Right + float(Glyph->Abc.abcC) + Font->GetLetterSpacing();
	}
}

void CUIWatchText::SetIntensity(float Intensity)
{
	Intensity = clampr(Intensity, 0.0f, 1.0f);
	const u32 R = u32(clampr(iFloor(float(color_get_R(Color)) * Intensity + 0.5f), 0, 255));
	const u32 G = u32(clampr(iFloor(float(color_get_G(Color)) * Intensity + 0.5f), 0, 255));
	const u32 B = u32(clampr(iFloor(float(color_get_B(Color)) * Intensity + 0.5f), 0, 255));
	const u32 A = u32(clampr(iFloor(float(color_get_A(Color)) * Intensity + 0.5f), 0, 255));
	DrawColor = color_rgba(R, G, B, A);
}

void CUIWatchText::Draw()
{
	if (Quads.empty() || !Shader->inited())
	{
		return;
	}

	const float Scale = GetHeight() / Font->GetHeight();

	Fvector2 Origin;
	GetAbsolutePos(Origin);
	if (Align == EAlign::Center)
	{
		Origin.x += (GetWidth() - TextWidth * Scale) * 0.5f;
	}
	else if (Align == EAlign::Right)
	{
		Origin.x += GetWidth() - TextWidth * Scale;
	}

	UIRender->SetShader(*Shader);
	UIRender->StartPrimitive(u32(Quads.size()) * 6, IUIRender::ptTriList, IUIRender::pttLIT);
	for (const SGlyphQuad& Quad : Quads)
	{
		const float X1 = Origin.x + Quad.Pos.x1 * Scale;
		const float Y1 = Origin.y + Quad.Pos.y1 * Scale;
		const float X2 = Origin.x + Quad.Pos.x2 * Scale;
		const float Y2 = Origin.y + Quad.Pos.y2 * Scale;

		UIRender->PushPoint(X1, Y1, 0.0f, DrawColor, Quad.Uv.x1, Quad.Uv.y1);
		UIRender->PushPoint(X2, Y1, 0.0f, DrawColor, Quad.Uv.x2, Quad.Uv.y1);
		UIRender->PushPoint(X2, Y2, 0.0f, DrawColor, Quad.Uv.x2, Quad.Uv.y2);
		UIRender->PushPoint(X1, Y1, 0.0f, DrawColor, Quad.Uv.x1, Quad.Uv.y1);
		UIRender->PushPoint(X2, Y2, 0.0f, DrawColor, Quad.Uv.x2, Quad.Uv.y2);
		UIRender->PushPoint(X1, Y2, 0.0f, DrawColor, Quad.Uv.x1, Quad.Uv.y2);
	}
	UIRender->FlushPrimitive();
}

bool CUIWatchWnd::Init(const SWatchDisplay& Display, const SWatchFonts& Fonts)
{
	CUIXml Xml;

	string_path XmlPath = {};
	xr_sprintf(XmlPath, "%s\\%s", UI_PATH, Xml.correct_file_name(UI_PATH, WatchXml).c_str());
	CXml::RemoveFromCache(CONFIG_PATH, XmlPath);

	if (!Xml.Load(CONFIG_PATH, UI_PATH, WatchXml))
	{
		Msg("! [watch] cannot load [%s]", WatchXml);
		return false;
	}

	if (!Xml.NavigateToNode("watch", 0))
	{
		Msg("! [watch] %s: node <watch> not found", WatchXml);
		return false;
	}

	Time = CreateText(Xml, "watch:time", Fonts.Time);
	Date = CreateText(Xml, "watch:date", Fonts.Date);
	Barometer = CreateText(Xml, "watch:barometer", Fonts.Barometer);
	BarometerUnit = CreateText(Xml, "watch:barometer_unit", Fonts.BarometerUnit);
	if (BarometerUnit)
	{
		BarometerUnit->SetText(g_pStringTable ? g_pStringTable->translate(Display.BarometerUnit).c_str() : Display.BarometerUnit.c_str());
		BarometerUnit->Show(false);
	}
	const bool CompassGlow = CreateGlow(Xml, "watch:compass_light", Glows[u32(EWatchGlow::Compass)]);
	const bool AnomalyGlow = CreateGlow(Xml, "watch:anomaly_indicator", Glows[u32(EWatchGlow::Anomaly)]);
	const bool MotionGlow = CreateGlow(Xml, "watch:motion_indicator", Glows[u32(EWatchGlow::Motion)]);
	EnsureGlow(Xml, "watch:noise_indicator", Glows[u32(EWatchGlow::Noise)], color_rgba(255, 220, 80, 255));
	CreateGlass(Xml, "watch:anomaly_glass", Glasses[u32(EWatchGlow::Anomaly)]);
	CreateGlass(Xml, "watch:motion_glass", Glasses[u32(EWatchGlow::Motion)]);
	EnsureGlass(Xml, "watch:noise_glass", Glasses[u32(EWatchGlow::Noise)]);
	SetLayout(Display);
	return Time || Date || Barometer || CompassGlow || AnomalyGlow || MotionGlow || Glows[u32(EWatchGlow::Noise)].Shader->inited();
}

bool CUIWatchWnd::CreateGlow(CUIXml& Xml, const char* Path, SGlow& Glow)
{
	if (!Xml.NavigateToNode(Path, 0))
	{
		return false;
	}

	Glow.Color = CUIXmlInit::GetColor(Xml, Path, 0, Glow.Color);
	const char* Texture = Xml.ReadAttrib(Path, 0, "texture", "");
	const bool Textured = Texture[0] != 0;
	Glow.Shader->create(Xml.ReadAttrib(Path, 0, "shader", Textured ? WatchGlowTexShader : WatchGlowShader), Textured ? Texture : nullptr);
	return Glow.Shader->inited();
}

bool CUIWatchWnd::CreateGlass(CUIXml& Xml, const char* Path, SGlass& Glass)
{
	if (!Xml.NavigateToNode(Path, 0))
	{
		return false;
	}

	Glass.LitAlpha = u32(clampr(Xml.ReadAttribInt(Path, 0, "lit", int(Glass.LitAlpha)), 0, 255));
	Glass.Shader->create(Xml.ReadAttrib(Path, 0, "shader", WatchGlassShader));
	Glass.Core->create(Xml.ReadAttrib(Path, 0, "core_shader", WatchLedShader));
	return Glass.Shader->inited();
}

void CUIWatchWnd::EnsureGlow(CUIXml& Xml, const char* Path, SGlow& Glow, u32 DefaultColor)
{
	if (CreateGlow(Xml, Path, Glow))
	{
		return;
	}

	Glow.Color = DefaultColor;
	Glow.Shader->create(WatchGlowShader, nullptr);
}

void CUIWatchWnd::EnsureGlass(CUIXml& Xml, const char* Path, SGlass& Glass)
{
	if (CreateGlass(Xml, Path, Glass))
	{
		return;
	}

	Glass.LitAlpha = WatchGlassDefaultLitAlpha;
	Glass.Shader->create(WatchGlassShader);
	Glass.Core->create(WatchLedShader);
}

void CUIWatchWnd::SetLayout(const SWatchDisplay& Display)
{
	auto Apply = [](CUIWatchText* Text, const Fvector4& Rect)
	{
		if (Text)
		{
			Text->SetWndPos(Fvector2().set(Rect.x, Rect.y));
			Text->SetWndSize(Fvector2().set(Rect.z, Rect.w));
		}
	};

	Apply(Time, Display.UiTime);
	Apply(Date, Display.UiDate);
	Apply(Barometer, Display.UiBarometer);
	Apply(BarometerUnit, Display.UiBarometerUnit);
}

CUIWatchText* CUIWatchWnd::CreateText(CUIXml& Xml, const char* Path, const shared_str& FontName)
{
	if (!Xml.NavigateToNode(Path, 0))
	{
		return nullptr;
	}

	CUIWatchText* Text = new CUIWatchText();
	if (!Text->Init(Xml, Path, FontName))
	{
		xr_delete(Text);
		return nullptr;
	}

	Text->SetAutoDelete(true);
	AttachChild(Text);
	return Text;
}

void CUIWatchWnd::SetTime(const char* Text)
{
	if (Time)
	{
		Time->SetText(Text);
	}
}

void CUIWatchWnd::SetDate(const char* Text)
{
	if (Date)
	{
		Date->SetText(Text);
	}
}

void CUIWatchWnd::SetBarometer(const char* Text)
{
	if (Barometer)
	{
		Barometer->SetText(Text);
	}
	if (BarometerUnit)
	{
		BarometerUnit->Show(Text && *Text);
	}
}

void CUIWatchWnd::SetIntensity(float Intensity)
{
	for (CUIWatchText* Text : {Time, Date, Barometer, BarometerUnit})
	{
		if (Text)
		{
			Text->SetIntensity(Intensity);
		}
	}
}

void CUIWatchWnd::Render(const Fmatrix& Xform)
{
	const IUIRender::ePointType PointType = UI().m_currentPointType;
	UI().m_currentPointType = IUIRender::pttLIT;

	UIRender->CacheSetXformWorld(Xform);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);

	CUIWindow::Draw();

	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
	UI().m_currentPointType = PointType;
}

u32 CUIWatchWnd::GetGlowColor(EWatchGlow Id) const
{
	return Glows[u32(Id)].Color;
}

void CUIWatchWnd::RenderGlow(EWatchGlow Id, const Fmatrix& Xform, const Fvector2& Size, float Intensity)
{
	SGlow& Glow = Glows[u32(Id)];
	const u32 Alpha = iFloor(float(color_get_A(Glow.Color)) * clampr(Intensity, 0.0f, 1.0f) + 0.5f);
	if (!Alpha || !Glow.Shader->inited())
	{
		return;
	}

	const u32 Color = subst_alpha(Glow.Color, Alpha);
	const float HalfWidth = Size.x * 0.5f;
	const float HalfHeight = Size.y * 0.5f;

	UIRender->CacheSetXformWorld(Xform);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);
	UIRender->SetShader(*Glow.Shader);
	UIRender->StartPrimitive(6, IUIRender::ptTriList, IUIRender::pttLIT);
	UIRender->PushPoint(-HalfWidth, -HalfHeight, 0.0f, Color, 0.0f, 0.0f);
	UIRender->PushPoint(HalfWidth, -HalfHeight, 0.0f, Color, 1.0f, 0.0f);
	UIRender->PushPoint(HalfWidth, HalfHeight, 0.0f, Color, 1.0f, 1.0f);
	UIRender->PushPoint(-HalfWidth, -HalfHeight, 0.0f, Color, 0.0f, 0.0f);
	UIRender->PushPoint(HalfWidth, HalfHeight, 0.0f, Color, 1.0f, 1.0f);
	UIRender->PushPoint(-HalfWidth, HalfHeight, 0.0f, Color, 0.0f, 1.0f);
	UIRender->FlushPrimitive();
	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
}

void CUIWatchWnd::RenderLed(EWatchGlow Id, const Fmatrix& Xform, const Fvector2& GlassSize, const Fvector2& CoreSize, float Intensity, float Brightness)
{
	const SGlow& Glow = Glows[u32(Id)];
	const SGlass& Glass = Glasses[u32(Id)];
	if (!Glass.Shader->inited())
	{
		return;
	}

	Fmatrix InvXform;
	InvXform.invert(Xform);
	Fvector Eye;
	InvXform.transform_tiny(Eye, Device.vCameraPosition);

	const float Lit = clampr(Intensity, 0.0f, 1.0f);
	const u32 GlassColor = subst_alpha(Glow.Color, u32(iFloor(float(Glass.LitAlpha) * Lit + 0.5f)));
	const float HalfLength = GlassSize.x * 0.5f;
	const float Radius = GlassSize.y;

	SLedVertex Back[WatchLedMaxVerts];
	SLedVertex Front[WatchLedMaxVerts];
	u32 BackCount = 0;
	u32 FrontCount = 0;

	auto Push = [&](bool Facing, const Fvector& P, float Nu, float Nv)
	{
		SLedVertex& V = Facing ? Front[FrontCount++] : Back[BackCount++];
		V.P = P;
		V.N.set(Nu, Nv);
	};

	const SLedCircle& Circle = LedCircle();
	for (u32 Index = 0; Index < WatchLedSegments; ++Index)
	{
		const Fvector N0 = {0.0f, Circle.Points[Index].x, Circle.Points[Index].y};
		const Fvector N1 = {0.0f, Circle.Points[Index + 1].x, Circle.Points[Index + 1].y};
		const Fvector P00 = {-HalfLength, N0.y * Radius, N0.z * Radius};
		const Fvector P01 = {HalfLength, N0.y * Radius, N0.z * Radius};
		const Fvector P10 = {-HalfLength, N1.y * Radius, N1.z * Radius};
		const Fvector P11 = {HalfLength, N1.y * Radius, N1.z * Radius};

		Fvector SideN;
		SideN.add(N0, N1).normalize_safe();
		Fvector ToEye;
		ToEye.mad(Eye, SideN, -Radius);
		const bool SideFacing = ToEye.dotproduct(SideN) > 0.0f;
		Push(SideFacing, P00, N0.y, N0.z);
		Push(SideFacing, P01, N0.y, N0.z);
		Push(SideFacing, P11, N1.y, N1.z);
		Push(SideFacing, P00, N0.y, N0.z);
		Push(SideFacing, P11, N1.y, N1.z);
		Push(SideFacing, P10, N1.y, N1.z);

		const Fvector CapC0 = {-HalfLength, 0.0f, 0.0f};
		const Fvector CapC1 = {HalfLength, 0.0f, 0.0f};
		Push(Eye.x < -HalfLength, CapC0, -WatchGlassCapNormal, 0.0f);
		Push(Eye.x < -HalfLength, P10, -WatchGlassCapNormal, 0.0f);
		Push(Eye.x < -HalfLength, P00, -WatchGlassCapNormal, 0.0f);
		Push(Eye.x > HalfLength, CapC1, WatchGlassCapNormal, 0.0f);
		Push(Eye.x > HalfLength, P01, WatchGlassCapNormal, 0.0f);
		Push(Eye.x > HalfLength, P11, WatchGlassCapNormal, 0.0f);
	}

	auto Flush = [&](const SLedVertex* Verts, u32 Count)
	{
		if (!Count)
		{
			return;
		}

		UIRender->SetShader(*Glass.Shader);
		UIRender->StartPrimitive(Count, IUIRender::ptTriList, IUIRender::pttLIT);
		for (u32 Index = 0; Index < Count; ++Index)
		{
			UIRender->PushPoint(Verts[Index].P.x, Verts[Index].P.y, Verts[Index].P.z, GlassColor, Verts[Index].N.x, Verts[Index].N.y);
		}
		UIRender->FlushPrimitive();
	};

	UIRender->CacheSetXformWorld(Xform);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);
	Flush(Back, BackCount);

	const u32 Gain = u32(iFloor(255.0f * clampr(Lit * Brightness / WatchLedMaxBrightness, 0.0f, 1.0f) + 0.5f));
	if (Gain && Glass.Core->inited())
	{
		Fvector View = Eye;
		View.normalize_safe();
		Fvector Axis = {1.0f, 0.0f, 0.0f};
		Axis.mad(View, -View.x);
		if (Axis.square_magnitude() < EPS_S)
		{
			Axis.set(0.0f, 1.0f, 0.0f);
		}
		Axis.normalize();
		Fvector Side;
		Side.crossproduct(View, Axis).normalize_safe();
		Axis.mul(CoreSize.x * 0.5f);
		Side.mul(CoreSize.y * 0.5f);

		const u32 Color = subst_alpha(Glow.Color, Gain);
		const Fvector C00 = Fvector().sub(Fvector().invert(Axis), Side);
		const Fvector C10 = Fvector().sub(Axis, Side);
		const Fvector C11 = Fvector().add(Axis, Side);
		const Fvector C01 = Fvector().sub(Side, Axis);

		UIRender->SetShader(*Glass.Core);
		UIRender->StartPrimitive(6, IUIRender::ptTriList, IUIRender::pttLIT);
		UIRender->PushPoint(C00.x, C00.y, C00.z, Color, 0.0f, 0.0f);
		UIRender->PushPoint(C10.x, C10.y, C10.z, Color, 1.0f, 0.0f);
		UIRender->PushPoint(C11.x, C11.y, C11.z, Color, 1.0f, 1.0f);
		UIRender->PushPoint(C00.x, C00.y, C00.z, Color, 0.0f, 0.0f);
		UIRender->PushPoint(C11.x, C11.y, C11.z, Color, 1.0f, 1.0f);
		UIRender->PushPoint(C01.x, C01.y, C01.z, Color, 0.0f, 1.0f);
		UIRender->FlushPrimitive();
	}

	Flush(Front, FrontCount);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
}

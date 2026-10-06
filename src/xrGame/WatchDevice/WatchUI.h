#pragma once

#include "../../xrUI/ui_defs.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "WatchTypes.h"

class CGameFont;
class CUIXml;

class CUIWatchText final : public CUIWindow
{
public:
	bool Init(CUIXml& Xml, const char* Path, const shared_str& FontName);
	void SetText(const char* Text);
	void SetIntensity(float Intensity);
	void Draw() override;

private:
	enum class EAlign : u8
	{
		Left,
		Center,
		Right
	};

	struct SGlyphQuad
	{
		Frect Pos;
		Frect Uv;
	};

	CGameFont* Font = nullptr;
	ui_shader Shader;
	xr_vector<SGlyphQuad> Quads;
	float TextWidth = 0.0f;
	u32 Color = 0xFFFFFFFF;
	u32 DrawColor = 0xFFFFFFFF;
	EAlign Align = EAlign::Left;
};

class CUIWatchWnd final : public CUIWindow
{
public:
	bool Init(const SWatchDisplay& Display, const SWatchFonts& Fonts);
	void SetLayout(const SWatchDisplay& Display);
	void SetTime(const char* Text);
	void SetDate(const char* Text);
	void SetBarometer(const char* Text);
	void SetIntensity(float Intensity);
	void Render(const Fmatrix& Xform);
	void RenderGlow(EWatchGlow Id, const Fmatrix& Xform, const Fvector2& Size, float Intensity);
	void RenderLed(EWatchGlow Id, const Fmatrix& Xform, const Fvector2& GlassSize, const Fvector2& CoreSize, float Intensity, float Brightness);
	u32 GetGlowColor(EWatchGlow Id) const;

private:
	struct SGlow
	{
		ui_shader Shader;
		u32 Color = 0xFFFFFFFF;
	};

	struct SGlass
	{
		ui_shader Shader;
		ui_shader Core;
		u32 LitAlpha = 90;
	};

	CUIWatchText* CreateText(CUIXml& Xml, const char* Path, const shared_str& FontName);
	bool CreateGlow(CUIXml& Xml, const char* Path, SGlow& Glow);
	bool CreateGlass(CUIXml& Xml, const char* Path, SGlass& Glass);
	void EnsureGlow(CUIXml& Xml, const char* Path, SGlow& Glow, u32 DefaultColor);
	void EnsureGlass(CUIXml& Xml, const char* Path, SGlass& Glass);

	SGlow Glows[u32(EWatchGlow::Count)];
	SGlass Glasses[u32(EWatchGlow::Count)];

	CUIWatchText* Time = nullptr;
	CUIWatchText* Date = nullptr;
	CUIWatchText* Barometer = nullptr;
	CUIWatchText* BarometerUnit = nullptr;
};

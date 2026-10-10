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
	void RenderConditionIcon(EWatchCondition Id, const Fmatrix& Xform, const Fvector2& Size, float Intensity, u32 Color);
	void RenderConditionBar(EWatchCondition Id, const Fmatrix& Xform, const Fvector2& Size, float Fill, float Intensity, u32 Color);
	u32 GetGlowColor(EWatchGlow Id) const;
	u32 GetConditionIconColor(EWatchCondition Id) const;
	u32 GetConditionBarColor(EWatchCondition Id) const;
	bool HasConditionIcon(EWatchCondition Id) const;
	bool HasConditionBar(EWatchCondition Id) const;

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

	enum class EBarFill : u8
	{
		Left,
		Right
	};

	struct SConditionVisual
	{
		SGlow Icon;
		SGlow Bar;
		EBarFill Fill = EBarFill::Left;
		bool IconOk = false;
		bool BarOk = false;
	};

	CUIWatchText* CreateText(CUIXml& Xml, const char* Path, const shared_str& FontName);
	bool CreateGlow(CUIXml& Xml, const char* Path, SGlow& Glow);
	bool CreateGlass(CUIXml& Xml, const char* Path, SGlass& Glass);
	void EnsureGlow(CUIXml& Xml, const char* Path, SGlow& Glow, u32 DefaultColor);
	void EnsureGlass(CUIXml& Xml, const char* Path, SGlass& Glass);
	void CreateConditionVisual(CUIXml& Xml, EWatchCondition Id);
	void RenderGlowShader(SGlow& Glow, const Fmatrix& Xform, const Fvector2& Size, float Intensity, float U0, float U1, u32 Color);

	SGlow Glows[u32(EWatchGlow::Count)];
	SGlass Glasses[u32(EWatchGlow::Count)];
	SConditionVisual Conditions[WatchConditionCount];

	CUIWatchText* Time = nullptr;
	CUIWatchText* Date = nullptr;
	CUIWatchText* Barometer = nullptr;
	CUIWatchText* BarometerUnit = nullptr;
};

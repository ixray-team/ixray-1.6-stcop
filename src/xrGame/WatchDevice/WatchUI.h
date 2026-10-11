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
	bool Init(const SWatchConfig& Config);
	void SetLayout(const SWatchDisplay& Display);
	void SetTime(const char* Text);
	void SetDate(const char* Text);
	void SetBarometer(const char* Text);
	void SetIntensity(float Intensity);
	void Render(const Fmatrix& Xform);

	void RenderCompassGlow(const Fmatrix& Xform, const Fvector2& Size, float Intensity, u32 Color);
	u32 GetCompassColor() const;

	void RenderLedGlow(u32 Led, const Fmatrix& Xform, const Fvector2& Size, float Intensity);
	void RenderLed(u32 Led, const Fmatrix& Xform, const Fvector2& GlassSize, const Fvector2& CoreSize, float Intensity, float Brightness);
	u32 GetLedColor(u32 Led) const;

	void RenderGaugeIcon(u32 Gauge, const Fmatrix& Xform, const Fvector2& Size, float Intensity, u32 Color);
	void RenderGaugeBar(u32 Gauge, const Fmatrix& Xform, const Fvector2& Size, float Fill, float Intensity, u32 Color);
	u32 GetGaugeIconColor(u32 Gauge) const;
	u32 GetGaugeBarColor(u32 Gauge) const;
	bool HasGaugeIcon(u32 Gauge) const;
	bool HasGaugeBar(u32 Gauge) const;

private:
	struct SGlow
	{
		ui_shader Shader;
		u32 Color = 0xFFFFFFFF;
	};

	struct SLedSegment
	{
		Fvector P00;
		Fvector P01;
		Fvector P10;
		Fvector P11;
		Fvector SideNormal;
		Fvector2 N0;
		Fvector2 N1;
	};

	struct SLed
	{
		SGlow Glow;
		ui_shader Glass;
		ui_shader Core;
		u32 LitAlpha = 90;
		Fvector2 GeometrySize = {-1.0f, -1.0f};
		xr_vector<SLedSegment> Segments;
	};

	enum class EBarFill : u8
	{
		Left,
		Right
	};

	struct SGauge
	{
		SGlow Icon;
		SGlow Bar;
		EBarFill Fill = EBarFill::Left;
		bool IconOk = false;
		bool BarOk = false;
	};

	CUIWatchText* CreateText(CUIXml& Xml, const char* Path, const shared_str& FontName);
	bool CreateGlow(CUIXml& Xml, const char* Path, SGlow& Glow);
	void CreateLed(CUIXml& Xml, const shared_str& Name, SLed& Led);
	void CreateGauge(CUIXml& Xml, const shared_str& Name, SGauge& Gauge);
	void BuildLedGeometry(SLed& Led, const Fvector2& GlassSize);
	void RenderGlowShader(SGlow& Glow, const Fmatrix& Xform, const Fvector2& Size, float Intensity, float U0, float U1, u32 Color);

	SGlow CompassGlow;
	xr_vector<SLed> Leds;
	xr_vector<SGauge> Gauges;

	CUIWatchText* Time = nullptr;
	CUIWatchText* Date = nullptr;
	CUIWatchText* Barometer = nullptr;
	CUIWatchText* BarometerUnit = nullptr;
};

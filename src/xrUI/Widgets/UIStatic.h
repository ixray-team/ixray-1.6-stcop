#pragma once

#include "UILanimController.h"
#include "UIStaticItem.h"
#include "../../xrScripts/script_export_space.h"
#include "../../Include/xrRender/SVGTypes.h"
#include "../UIVectorBinding.h"
#include "UILines.h"

class CUIFrameWindow;
class CLAItem;
class CUIXml;

struct UI_API lanim_cont
{
	CLAItem*				m_lanim;
	float					m_lanim_start_time;
	float					m_lanim_delay_time;
	Flags8					m_lanimFlags;
	void					set_defaults		();
};

struct UI_API lanim_cont_xf :
	public lanim_cont
{
	Fvector2				m_origSize;
	void					set_defaults		();
};

class UI_API CUIStatic :
	public CUIWindow,
	public ITextureOwner,
	public CUILightAnimColorConrollerImpl
{
	friend class CUIXmlInit;
	friend class CUIWindow;
	bool BlurredShadowDrawnInParent = false;
private:
	typedef CUIWindow inherited;
	lanim_cont_xf			m_lanim_xform;
	void					EnableHeading_int		(bool b)				{m_bHeading = b;}
	CUIVectorBinding _svgBinding;
	bool MouseEventsEnabled = false;
	bool BlurredShadowEnabled = false;
	float BlurredShadowSigma = 3.0f;
	float BlurredShadowOpacity = 0.5f;
	float BlurredShadowFalloff = 0.0f;
	float BlurredShadowMinOpacity = 0.0f;
	float BlurredShadowRadius = 9.0f;
	Fvector2 BlurredShadowLight = { 512.0f, 0.0f };
	float BlurredShadowHeight = 4.0f;
	float BlurredShadowLightHeight = 300.0f;
	float BlurredShadowLightRadius = 0.0f;
	u32 BlurredShadowColor = 0xff000000;
	void PrepareTextureItem();
	u32 BackgroundColor = 0;
	bool BackgroundColorAssigned = false;
	bool BorderEnabled = false;
	UIFlashlight Flashlight;
	ui_shader FlashlightMaskShader;
	shared_str FlashlightMaskName;
	bool FlashlightMaskUseMouse = false;
	float FlashlightMaskX = 0.0f, FlashlightMaskY = 0.0f;
	float FlashlightMaskWidth = 400.0f, FlashlightMaskHeight = 400.0f, FlashlightMaskScale = 1.0f;
	CUIStaticItem AltTextureItem;
	bool AltTextureEnabled = false;
	ui_shader TextureBlendMaskShader;
	shared_str TextureBlendMaskName;
	bool TextureBlendMaskAssigned = false;
	bool TextureBlendMaskUseMouse = false;
	float TextureBlendMaskX = 0.0f, TextureBlendMaskY = 0.0f;
	float TextureBlendMaskWidth = 400.0f, TextureBlendMaskHeight = 400.0f, TextureBlendMaskScale = 1.0f;
	void ApplyTextureBlendMask();
	UITextureBlendMask ResolveTextureMask(IUIShader& Shader, float X, float Y, float Width, float Height, float Scale, bool UseMouse);
	float BorderThickness = 0.0f, BorderRadius = 0.0f;
	u32 BorderColor = 0;
	UIRoundedRect GetBorderRect();
	void DrawBackground();
	float TargetHeading = 0.0f;
	double HeadingProgress = 0.0;
	bool HeadingPending = false;
	void UpdateSmoothHeading(float Blend);
	bool HitTestUntransformed(const Fvector2& Position) override;
public:

							CUIStatic				();
	virtual					~CUIStatic				();

	virtual void			Draw					();
	virtual void			Update					();
	virtual void			OnFocusLost				();
	bool OnMouseAction(float X, float Y, EUIMessages MouseAction) override;
	void EnableMouseEvents(bool Enabled) { MouseEventsEnabled = Enabled; }
	bool InitTexture(const char* raster_texture_name, const char* svg_texture_name) override;
	virtual const char* GetText() { return TextItemControl()->GetText(); }
	virtual void SetText(const char* txt);
			void SetTextIfNodeExist(const char* txt);
	virtual void SetTextST(const char* txt) { TextItemControl()->SetTextST(txt); }
	void SetBackgroundColorARGB(int A, int R, int G, int B);
	void SetBorder(float Thickness, float Radius, u32 Color);
	virtual void SetBackgroundOpacity(float Value);
	void SetBorderOpacity(float Value);
	// Applies once to the composited control and its children, within its bounds.
	void SetFlashlight(bool Enabled, float Radius, float Softness, float Darkness, float Saturation, float Multiply);
	void SetFlashlightPosition(float X, float Y);
	void SetFlashlightIsolation(bool Enabled);
	void SetFlashlightMask(const char* Texture, float X, float Y, float Width, float Height, float Scale, bool IsUseMousePosition);
	void SetFlashlightMaskPosition(float X, float Y);
	void SetFlashlightVolume(bool Enabled, float SourceX, float SourceY, float DistancePixels, float Density, int Steps);
	void SetFlashlightVolumeColor(float Red, float Green, float Blue);
	void SetFlashlightSurfaceColor(float Red, float Green, float Blue);
	void SetFlashlightDustIntensity(float Intensity);
	void SetFlashlightDustColor(float Red, float Green, float Blue);
	void SetFlashlightVolumeNearClip(float DistancePixels);
	void SetFlashlightDust(bool Enabled, int Count, float Size, float Speed, float Opacity);
	// Explicit accumulated phases prevent position jumps when Lua changes flow speed.
	void SetFlashlightDustAnimation(bool Enabled, float MotionPhase, float GlintPhase, float GlintIntensity);
	// Instantaneous brightness loss; Lua schedules and smooths intermittent bursts.
	void SetFlashlightInterference(bool Enabled, float Strength);
	bool GetFlashlightEnabled() const { return Flashlight.Enabled; }
	void InitAltTexture(const char* Texture);
	void SetTextureBlendMask(const char* Texture, float X, float Y, float Width, float Height, float Scale, bool IsUseMousePosition);
	// Switches to manual positioning, relative to this static's top-left corner.
	void SetTextureBlendMaskPosition(float X, float Y);
	virtual void SetTextColor(u32 clr) { TextItemControl()->SetTextColor(clr); }
	void InitSVG(CUIXml& xml_doc, const char* path, int index) override;
	bool isSVGPresented(void) const override;
	const char* getSVGFilename(CUIXml& xml_doc, const char* path, int index = 0) override;
	const SVGTintRGBA& GetVectorTint() const { return _svgBinding.GetTint(); }
	void SetTextColor_script(int a, int r, int g, int b)
	{
		TextItemControl()->SetTextColor(color_argb(a, r, g, b));
	}

	u32 GetTextAlign_script()
	{
		return static_cast<u32>(TextItemControl()->GetTextAlignment());
	}

	void SetTextAlign_script(u32 align)
	{
		TextItemControl()->SetTextAlignment((CGameFont::EAligment)align);
		if (CGameFont* Font = TextItemControl()->GetFont())
		{
			Font->SetAligment((CGameFont::EAligment)align);
		}
	}
	void ReloadText() { TextItemControl()->ReloadText(); }

	virtual void			CreateShader			(const char* tex, const char* sh = "hud\\default");
	ui_shader&				GetShader				()							{return m_UIStaticItem.GetShader();};

	virtual void			SetTextureColor			(u32 color)					{ m_UIStaticItem.SetTextureColor(color);}
	virtual u32				GetTextureColor			() const					{ return m_UIStaticItem.GetTextureColor();}
	virtual void			SetTextureRect			(const Frect& r)			{m_UIStaticItem.SetTextureRect(r);}
	virtual const Frect&	GetTextureRect			() const					{return m_UIStaticItem.GetTextureRect();}

	virtual bool			InitTexture				(const char* tex_name, bool fatal = true);
	virtual bool			InitTextureEx			(const char* tex_name, const char* sh_name="hud\\default", bool fatal = true);
	CUIStaticItem*			GetStaticItem			()							{return &m_UIStaticItem;}
	void ResetOriginalRect() { m_UIStaticItem.ResetOriginalRect(); }
			void			SetTextureRect_script	(Frect* pr)					{m_UIStaticItem.SetTextureRect(*pr);}
	const	Frect*			GetTextureRect_script	()							{return &m_UIStaticItem.GetTextureRect();}

			void			SetHeadingPivot			(const Fvector2& p, const Fvector2& offset, bool fixedLT)				{m_UIStaticItem.SetHeadingPivot(p,offset,fixedLT);}
			void			ResetHeadingPivot		()							{m_UIStaticItem.ResetHeadingPivot();}
	virtual void			SetTextureOffset		(float x, float y)			{ m_TextureOffset.set(x, y); }
			Fvector2		GetTextureOffeset		() const					{ return m_TextureOffset; }
			void			TextureOn				()							{ m_bTextureEnable = true; }
			void			TextureOff				()							{ m_bTextureEnable = false; }
			void			TextOn				()								{ m_bTextEnable = true; }
			void			TextOff				()								{ m_bTextEnable = false; }
			void			SetTextOffset			(float x, float y)			{ TextItemControl()->m_TextOffset.x = x; TextItemControl()->m_TextOffset.y = y; }
			void			SetTextAlignment		(ETextAlignment al)			{TextItemControl()->SetTextAlignment(al);}
			void			SetTextComplexMode		(bool mode = true)			{TextItemControl()->SetTextComplexMode(mode);}
			void			SetVTextAlignment		(EVTextAlignment al)		{TextItemControl()->SetVTextAlignment(al);}
	virtual void			SetFont					(CGameFont* pFont);
	virtual CGameFont*		GetFont					();
			u32				GetTextColor			()							{return TextItemControl()->GetTextColor();}
			void			HighlightText			(bool bHighlight)			{ m_bEnableTextHighlighting = bHighlight; }
	virtual bool			IsHighlightText			();

			void			SetTextX				(float x)					{TextItemControl()->m_TextOffset.x = x;}
			float			GetTextX				()							{return TextItemControl()->m_TextOffset.x;}
			void			SetTextY				(float y)					{TextItemControl()->m_TextOffset.y = y;}
			float			GetTextY				()							{return TextItemControl()->m_TextOffset.y;}


	// own
	virtual void			SetHighlightColor		(const u32 uColor)			{ m_HighlightColor = uColor; }
			void			EnableTextHighlighting	(bool value)				{ m_bEnableTextHighlighting = value; }
			void			SetXformLightAnim		(const char* lanim, bool bCyclic);
			void			ResetXformAnimation		();

	virtual void			DrawTexture				();
			void			DrawTexturePass			(u32 color, const Fvector2& extraOffset);
	virtual void			DrawWidgetText				();
	virtual void			DrawHighlightedText		();

			void 			AdjustHeightToText		();
			void 			AdjustWidthToText		();


			void			SetShader				(const ui_shader& sh);
			CUIStaticItem&	GetUIStaticItem			()						{return m_UIStaticItem;}

			void			SetStretchTexture		(bool stretch_texture)	{m_bStretchTexture = stretch_texture;}
			bool			GetStretchTexture		()						{return m_bStretchTexture;}
			void			SetTextureShadow		(bool enabled, float thickness, u32 color);
	void SetBlurredTextureShadow(bool Enabled, float Blur, float Opacity, float Radius, float LightX, float LightY);
	void SetBlurredTextureShadowHeight(float Height, float LightHeight);
	void SetBlurredTextureShadowLightRadius(float Radius);
	void SetBlurredTextureShadowFalloff(float Falloff);
	void SetBlurredTextureShadowOpacityRange(float Minimum, float Maximum);
	void SetBlurredTextureShadowColor(u32 Color) { BlurredShadowColor = Color; }
	bool GetBlurredTextureShadowEnabled() const { return BlurredShadowEnabled; }
	void DrawBlurredTextureShadow();
			bool			GetTextureShadowEnabled	() const					{return m_textureShadowEnabled;}
			void			SetTextShadow			(bool enabled, float thickness, u32 color);
			void			SetTextShadow			(const SUIOutlineParams& params);
			bool			GetTextShadowEnabled	() const					{return m_pTextControl && m_pTextControl->GetTextShadowEnabled();}
			void			SetEllipsis				(int pos, int indent)	{ TextItemControl()->SetEllipsis(pos != 0); }
			void			SetEllipsis_script		(bool mode)				{ TextItemControl()->SetEllipsis(mode); }

			void			SetHeading				(float Angle);
			float			GetHeading				()						{return m_fHeading;}
			bool			Heading					()						{return m_bHeading && IsHeadingEnabled();}
			void			EnableHeading			(bool b) override		{m_bHeading = b; inherited::EnableHeading(b);}

			void			SetConstHeading			(bool b)				{m_bConstHeading = b;};
			bool			GetConstHeading			()						{return m_bConstHeading;}

	virtual void			ColorAnimationSetTextureColor	(u32 color, bool only_alpha);
	virtual void			ColorAnimationSetTextColor		(u32 color, bool only_alpha);

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return this; }
	virtual ITextureOwner* ui_cast_texture_owner() { return this; }
	virtual CUILightAnimColorConroller* ui_cast_light_anim_color_controller() { return this; }

protected:
	CUILines*		m_pTextControl;
	bool			m_bEnableTextHighlighting;
	// Цвет подсветки
	u32				m_HighlightColor;

	bool			m_bStretchTexture;
	bool			m_bTextureEnable;
	bool			m_bTextEnable;
	CUIStaticItem	m_UIStaticItem;

	bool			m_bHeading;
	bool			m_bConstHeading;
	float			m_fHeading;

	Fvector2		m_TextureOffset;
	bool			m_textureShadowEnabled;
	float			m_textureShadowThickness;
	u32				m_textureShadowColor;
	bool			m_text_control_exists;

public:
	CUILines*		TextItemControl						();
	shared_str		m_stat_hint_text;
	void SetHint(const char* Text) { m_stat_hint_text = Text ? Text : ""; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

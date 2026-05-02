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
private:
	typedef CUIWindow inherited;
	lanim_cont_xf			m_lanim_xform;
	void					EnableHeading_int		(bool b)				{m_bHeading = b;}
	CUIVectorBinding _svgBinding;
public:

							CUIStatic				();
	virtual					~CUIStatic				();

	virtual void			Draw					();
	virtual void			Update					();
	virtual void			OnFocusLost				();
	bool InitTexture(const char* raster_texture_name, const char* svg_texture_name) override;
	virtual const char* GetText() { return TextItemControl()->GetText(); }
	virtual void SetText(const char* txt);
			void SetTextIfNodeExist(const char* txt);
	virtual void SetTextST(const char* txt) { TextItemControl()->SetTextST(txt); }
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
		TextItemControl()->GetFont()->SetAligment((CGameFont::EAligment)align);
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
	virtual void			DrawText				();
	virtual void			DrawHighlightedText		();

			void 			AdjustHeightToText		();
			void 			AdjustWidthToText		();

	
			void			SetShader				(const ui_shader& sh);
			CUIStaticItem&	GetUIStaticItem			()						{return m_UIStaticItem;}

			void			SetStretchTexture		(bool stretch_texture)	{m_bStretchTexture = stretch_texture;}
			bool			GetStretchTexture		()						{return m_bStretchTexture;}
			void			SetTextureShadow		(bool enabled, float thickness, u32 color);
			bool			GetTextureShadowEnabled	() const					{return m_textureShadowEnabled;}
			void			SetEllipsis				(int pos, int indent)	{ TextItemControl()->SetEllipsis(pos != 0); }
			void			SetEllipsis_script		(bool mode)				{ TextItemControl()->SetEllipsis(mode); }

			void			SetHeading				(float f)				{m_fHeading = f;};
			float			GetHeading				()						{return m_fHeading;}
			bool			Heading					()						{return m_bHeading;}
			void			EnableHeading			(bool b)				{m_bHeading = b;}

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

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

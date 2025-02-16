#pragma once

#include "UILanimController.h"
#include "../uistaticitem.h"
#include "../../xrScripts/script_export_space.h"
#include "uilines.h"

class CUIFrameWindow;
class CLAItem;
class CUIXml;

struct lanim_cont{
	CLAItem*				m_lanim;
	float					m_lanim_start_time;
	float					m_lanim_delay_time;
	Flags8					m_lanimFlags;
	void					set_defaults		();
};

struct lanim_cont_xf :public lanim_cont{
	Fvector2				m_origSize;
	void					set_defaults		();
};

class CUIStatic : public CUIWindow, public ITextureOwner, public CUILightAnimColorConrollerImpl
{
	friend class CUIXmlInit;
private:
	typedef CUIWindow							inherited;
	typedef CUILightAnimColorConrollerImpl		inherited1;

	lanim_cont_xf			m_lanim_xform;
	void					EnableHeading_int		(bool b)				{m_bHeading = b;}
public:

							CUIStatic				();
	virtual					~CUIStatic				();

	virtual void			Draw					();
	virtual void			Update					();
	virtual void			OnFocusLost				();

	virtual void			CreateShader			(LPCSTR tex, LPCSTR sh = "hud\\default");
	ui_shader&				GetShader				()							{return m_UIStaticItem.GetShader();};

	virtual void			SetTextureColor			(u32 color)					{ m_UIStaticItem.SetTextureColor(color);}
	virtual u32				GetTextureColor			() const					{ return m_UIStaticItem.GetTextureColor();}
	virtual void			SetTextureRect			(const Frect& r)			{m_UIStaticItem.SetTextureRect(r);}
	virtual void			SetTextureRect			(const float x, const float y, const float w, const float h)			{m_UIStaticItem.SetTextureRect(Frect().set(x,y,w,h));}
	virtual const Frect&	GetTextureRect			() const					{return m_UIStaticItem.GetTextureRect();}
	
	virtual void			InitTexture				(LPCSTR tex_name);
	virtual void			InitTextureEx			(LPCSTR tex_name, LPCSTR sh_name="hud\\default");
	CUIStaticItem*			GetStaticItem			()							{return &m_UIStaticItem;}
			void			SetTextureRect_script	(Frect* pr)					{m_UIStaticItem.SetTextureRect(*pr);}
	const	Frect*			GetTextureRect_script	()							{return &m_UIStaticItem.GetTextureRect();}

			void			SetHeadingPivot			(const Fvector2& p, const Fvector2& offset, bool fixedLT)				{m_UIStaticItem.SetHeadingPivot(p,offset,fixedLT);}
			void			ResetHeadingPivot		()							{m_UIStaticItem.ResetHeadingPivot();}
	virtual void		SetColor					(u32 color)					{ m_UIStaticItem.SetTextureColor(color);		}
	u32					GetColor					() const					{ return m_UIStaticItem.GetTextureColor();		}
	virtual void			SetTextureOffset		(float x, float y)			{ m_TextureOffset.set(x, y); }
			Fvector2		GetTextureOffeset		() const					{ return m_TextureOffset; }
			void			TextureOn				()							{ m_bTextureEnable = true; }
			bool			IsTextureOn				() const					{ return m_bTextureEnable; }
			void			TextureOff				()							{ m_bTextureEnable = false; }

			void	SetTextAlign_script			(u32 align);
			u32		GetTextAlign_script			();

	virtual void			InitTexture_script				(LPCSTR tex_name);


	// own
			void			SetXformLightAnim		(LPCSTR lanim, bool bCyclic);
			void			ResetXformAnimation		();

	virtual void		Init						(LPCSTR tex_name, float x, float y, float width, float height);	
		void		InitEx						(LPCSTR tex_name, LPCSTR sh_name, float x, float y, float width, float height);

			
	virtual void			DrawTexture				();
	virtual void			DrawText				();
	virtual void			DrawHighlightedText			();

			void 			AdjustHeightToText		();
			void 			AdjustWidthToText		();
			void			HighlightText			(bool bHighlight)		{m_bEnableTextHighlighting = bHighlight;}
	virtual bool			IsHighlightText			();

	virtual void			ResetColorAnimation		()						{ inherited1::ResetColorAnimation();  }
	
			void			SetShader				(const ui_shader& sh);
			CUIStaticItem&	GetUIStaticItem			()						{return m_UIStaticItem;}

			void			SetStretchTexture		(bool stretch_texture)	{m_bStretchTexture = stretch_texture;}
			bool			GetStretchTexture		()						{return m_bStretchTexture;}

	// Анализируем текст на помещаемость его по длинне в заданную ширину, и если нет, то всталяем 
	// "\n" реализуем таким образом wordwrap
	enum EElipsisPosition
	{
		eepNone,
		eepBegin,
		eepEnd,
		eepCenter
	};

			void			SetElipsis							(EElipsisPosition pos, int indent);

			
			void			SetHeading				(float f)				{m_fHeading = f;};
			float			GetHeading				()						{return m_fHeading;}
			bool			Heading					()						{return m_bHeading;}
			void			EnableHeading			(bool b)				{m_bHeading = b;}

			void			SetConstHeading			(bool b)				{m_bConstHeading = b;};
			bool			GetConstHeading			()						{return m_bConstHeading;}


			void			SetText					(LPCSTR txt)				{TextItemControl()->SetText(txt);}
			void			SetTextST				(LPCSTR txt)				{TextItemControl()->SetTextST(txt);}
			LPCSTR			GetText					()							{return TextItemControl()->GetText();}
			void			SetFont					(CGameFont* F)				{TextItemControl()->SetFont(F);}
			CGameFont*		GetFont					()							{return TextItemControl()->GetFont();}
			void			SetTextColor			(u32 color)					{TextItemControl()->SetTextColor(color);}
			void			SetTextColor			(u32 a, u32 r, u32 g, u32 b)					{TextItemControl()->SetTextColor(color_argb(a,r,g,b));}
			u32			GetTextColor			()							{return TextItemControl()->GetTextColor();}
			void			SetTextComplexMode		(bool mode = true)			{TextItemControl()->SetTextComplexMode(mode);}
			bool			IsTextComplexMode			()						{return TextItemControl()->IsTextComplexMode();}
			void			SetTextAlignment		(ETextAlignment al)			{TextItemControl()->SetTextAlignment(al);}
			void			SetVTextAlignment		(EVTextAlignment al)		{TextItemControl()->SetVTextAlignment(al);}
			void			SetEllipsis				(bool mode)					{TextItemControl()->SetEllipsis(mode);}
			void			SetCutWordsMode			(bool mode)					{TextItemControl()->SetCutWordsMode(mode);}
			void			SetTextOffset			(float x, float y)			{TextItemControl()->m_TextOffset.x = x; TextItemControl()->m_TextOffset.y = y;}

			void			SetTextPosX			(float x)					{TextItemControl()->m_TextOffset.x = x;}
			float			GetTextPosX			()							{return TextItemControl()->m_TextOffset.x;}
			void			SetTextPosY			(float y)					{TextItemControl()->m_TextOffset.y = y;}
			float			GetTextPosY			()							{return TextItemControl()->m_TextOffset.y;}

	virtual void			SetHighlightColor				(const u32 uColor)	{ m_HighlightColor = uColor; }
			void			EnableTextHighlighting			(bool value)		{ m_bEnableTextHighlighting = value; }
	virtual void			ColorAnimationSetTextureColor	(u32 color, bool only_alpha);
	virtual void			ColorAnimationSetTextColor		(u32 color, bool only_alpha);

			void			SetMask							(CUIFrameWindow* mask);

protected:
	// возможность подсветки
	bool			m_bEnableTextHighlighting;
	// Цвет подсветки
	u32			m_HighlightColor;

	CUILines*		m_pTextControl;

	bool			m_bStretchTexture;
	bool			m_bTextureEnable;
	CUIStaticItem	m_UIStaticItem;

	bool			m_bHeading;
	bool			m_bConstHeading;
	float			m_fHeading;

	Fvector2		m_TextureOffset;

public:
	// Обрезка надписи
	EElipsisPosition	m_ElipsisPos;

	int			m_iElipsisIndent;

	CUILines*		TextItemControl						();
	shared_str		m_stat_hint_text;

	DECLARE_SCRIPT_REGISTER_FUNCTION

private:
	CUIFrameWindow* m_mask;
};

class CUITextWnd :public CUIWindow, public CUILightAnimColorConrollerImpl
{
	typedef CUIWindow	inherited;
	CUILines			m_lines;
public:
						CUITextWnd				();
	virtual				~CUITextWnd				(){};
	virtual void		Draw					();
	virtual void		Update					();

			void 		AdjustHeightToText		();
			void 		AdjustWidthToText		();

			void		SetText					(LPCSTR txt)				{TextItemControl().SetText(txt);}
			void		SetTextST				(LPCSTR txt)				{TextItemControl().SetTextST(txt);}
			LPCSTR		GetText					()							{return TextItemControl().GetText();}
			void		SetFont					(CGameFont* F)				{TextItemControl().SetFont(F);}
			CGameFont*	GetFont					()							{return TextItemControl().GetFont();}
			void		SetTextColor			(u32 color)					{TextItemControl().SetTextColor(color);}
			u32			GetTextColor			()							{return TextItemControl().GetTextColor();}
			void		SetTextComplexMode		(bool mode = true)			{TextItemControl().SetTextComplexMode(mode);}
			void		SetTextAlignment		(ETextAlignment al)			{TextItemControl().SetTextAlignment(al);}
			void		SetVTextAlignment		(EVTextAlignment al)		{TextItemControl().SetVTextAlignment(al);}
			void		SetEllipsis				(bool mode)					{TextItemControl().SetEllipsis(mode);}
			void		SetCutWordsMode			(bool mode)					{TextItemControl().SetCutWordsMode(mode);}
			void		SetTextOffset			(float x, float y)			{TextItemControl().m_TextOffset.x = x; TextItemControl().m_TextOffset.y = y;}

	virtual void		ColorAnimationSetTextColor(u32 color, bool only_alpha);

	CUILines&			TextItemControl			()							{return m_lines;}
};
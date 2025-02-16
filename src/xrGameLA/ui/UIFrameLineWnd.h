#pragma once

#include "UIWindow.h"
#include "UIStatic.h"

class CUIFrameLineWnd: public CUIWindow
{
	typedef CUIWindow inherited;
public:
				 CUIFrameLineWnd	();
			void InitFrameLineWnd	(LPCSTR base_name, Fvector2 pos, Fvector2 size, bool horizontal = true);
			void InitFrameLineWnd	(Fvector2 pos, Fvector2 size, bool horizontal = true);
			void InitTexture		(LPCSTR tex_name,LPCSTR sh_name="hud\\default");
	virtual void Draw				();

			float GetTextureHeight	() const						{return m_tex_rect[0].height();}
			float GetTextureWidth	() const						{return m_tex_rect[0].width();}
			void SetTextureColor	(u32 cl)						{m_texture_color=cl;}
			bool IsHorizontal		()								{return bHorizontal;}
			void SetHorizontal		(bool horiz)					{bHorizontal = horiz;}
			void SetStretchBETextures	(bool stretch)					{m_bStretchBETextures = stretch;}

	// Also we can display textual caption on the frame
	CUIStatic		UITitleText; //SkyLoader: dont change class to CUITextWnd because it is needed for scripts
	CUIStatic*		GetTitleStatic() {return &UITitleText;};

	virtual void Init_script		(LPCSTR base_name, float x, float y, float width, float height, bool horizontal = true); //SkyLoader: for scripts only

protected:
	bool			bHorizontal;
			bool					inc_pos(Frect& rect, int counter, int i, Fvector2& LTp, Fvector2& RBp, Fvector2& LTt, Fvector2& RBt);
	enum{
		flFirst = 0,	// Left or top
		flBack,			// Center texture
		flSecond,		// Right or bottom
		flMax
	};
	u32					m_texture_color;
	bool				m_bTextureVisible;
	bool				m_bStretchBETextures;
	void				DrawElements		();
	Fvector2				GetStretchingKoeff		();

	ui_shader			m_shader;
	Frect				m_tex_rect			[flMax];
	shared_str			dbg_tex_name;
};

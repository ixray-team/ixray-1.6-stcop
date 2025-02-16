#pragma once
#include "UIButton.h"
#include "UI_IB_Static.h"
#include "UI_IB_FrameLineWnd.h"

class CUI3tButton : public CUIButton 
{
	typedef CUIButton	inherited;
	friend class CUIXmlInit;
//.	using CUIButton::SetTextColor;
public:
					CUI3tButton					();
	virtual			~CUI3tButton				();
	// appearance

	virtual	void 	InitButton					(Fvector2 pos, Fvector2 size);
	virtual void 	InitTexture					(LPCSTR tex_name);
	virtual void 	InitTexture					(LPCSTR tex_enabled, LPCSTR tex_disabled, LPCSTR tex_touched, LPCSTR tex_highlighted);

	virtual void	Init_script					(float x, float y, float width, float height) { InitButton(Fvector2().set(x,y), Fvector2().set(width, height));} //SkyLoader: for scripts

//.			void 	SetTextColor				(u32 color);
	virtual void 	SetTextureOffset			(float x, float y);	
	virtual void 	SetWidth					(float width);
	virtual void 	SetHeight					(float height);
			void 	InitSoundH					(LPCSTR sound_file);
			void 	InitSoundT					(LPCSTR sound_file);

	// check button
	bool			GetCheck					() {return m_eButtonState == BUTTON_PUSHED;}
	void			SetCheck					(bool ch) {m_eButtonState = ch ? BUTTON_PUSHED : BUTTON_NORMAL;}
	
	// behavior
	virtual void 	OnClick						();
	virtual void 	OnFocusReceive				();
	virtual void	OnFocusLost					();

	virtual void	DrawTexture					();
	virtual void	Update						();

	virtual void	Init						(LPCSTR tex_name, float x, float y, float width, float height);	
	
	virtual bool 	OnMouseDown					(int mouse_btn);
	void			SetStretch					(bool stretch_texture)	{m_background->SetStretchTexture(stretch_texture);}
	void			SetStateTextColor			(u32 color, IBState state){m_dwTextColor[state] = color; m_bUseTextColor[state] = true;}
	u32				m_dwTextColor[4];
	bool			m_bUseTextColor[4]; // note: 0 index will be ignored


	bool					m_frameline_mode;
	bool					vertical;

	CUI_IB_Static*			m_background;
	CUI_IB_FrameLineWnd*		m_back_frameline;

private:	
			void		PlaySoundH					();
			void		PlaySoundT					();

	ref_sound			m_sound_h;
	ref_sound			m_sound_t;	

}; // class CUI3tButton

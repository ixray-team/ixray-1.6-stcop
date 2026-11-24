#pragma once

#include "UIWindow.h"

class CLAItem;

#define LA_CYCLIC			(1<<0)
#define LA_ONLYALPHA		(1<<1)
#define LA_TEXTCOLOR		(1<<2)
#define LA_TEXTURECOLOR		(1<<3)

struct UI_API color_animation
{
							color_animation		();
	CLAItem*				m_lanim;
	float					m_lanim_start_time;
	float					m_lanim_delay_time;
	Flags8					m_lanimFlags;
};

struct UI_API xform_animation :public color_animation
{
							xform_animation		();
	Fvector2				m_origSize;
	void					set_defaults		();
};

class UI_API CUILightAnimColorConroller
{
public:
	virtual bool	IsColorAnimationPresent			()											= 0;
	virtual void	ResetColorAnimation				()											= 0;
	virtual void	SetColorAnimation				(const char* lanim, u8 const& flags, float delay=0.0f)	= 0;
	virtual void	ColorAnimationSetTextureColor	(u32 color, bool only_alpha)				{};
	virtual void	ColorAnimationSetTextColor		(u32 color, bool only_alpha)				{};
};

class UI_API CUILightAnimColorConrollerImpl :
	public CUILightAnimColorConroller
{
	color_animation			m_lanim_clr;
public:

	virtual CUILightAnimColorConroller* ui_cast_light_anim_color_controller() { return this; }

	void SetColorAnimation(const char* lanim, u8 const& flags, float delay = 0.0f) override;
	void ResetColorAnimation() override;
	bool IsColorAnimationPresent() override;
	void UpdateColorAnimation();
};

class UI_API CUIColorAnimConrollerContainer :	
	public CUIWindow,
	public CUILightAnimColorConrollerImpl
{
	typedef CUIWindow		inherited;
public:
	virtual void			Update							();
	virtual void			ColorAnimationSetTextureColor	(u32 color, bool only_alpha);
	virtual void			ColorAnimationSetTextColor		(u32 color, bool only_alpha);

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUILightAnimColorConroller* ui_cast_light_anim_color_controller() { return this; }
};
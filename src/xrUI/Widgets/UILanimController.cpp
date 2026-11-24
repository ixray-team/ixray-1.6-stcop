#include "stdafx.h"
#include "UILanimController.h"
#include "../../xrEngine/LightAnimLibrary.h"

color_animation::color_animation() : m_lanim(nullptr), m_lanim_start_time(-1.0f), m_lanim_delay_time(0.0f)
{
	m_lanimFlags.zero();
}

xform_animation::xform_animation()
{
	m_origSize.set(0.0f, 0.0f);
}

void CUILightAnimColorConrollerImpl::SetColorAnimation(const char* lanim, u8 const& flags, float delay)
{
	if (lanim && lanim[0] != 0)
	{
		m_lanim_clr.m_lanim = LALib.FindItem(lanim);
	}
	else
	{
		m_lanim_clr.m_lanim = NULL;
		return;
	}

	m_lanim_clr.m_lanim_delay_time = delay;
	m_lanim_clr.m_lanimFlags.assign(flags);
	R_ASSERT((m_lanim_clr.m_lanim == NULL) || m_lanim_clr.m_lanimFlags.test(LA_TEXTCOLOR | LA_TEXTURECOLOR));
}

void CUILightAnimColorConrollerImpl::ResetColorAnimation()
{
	m_lanim_clr.m_lanim_start_time = Device.dwTimeContinual / 1000.0f + m_lanim_clr.m_lanim_delay_time / 1000.0f;
}

bool CUILightAnimColorConrollerImpl::IsColorAnimationPresent()
{
	if (m_lanim_clr.m_lanim == NULL)
	{
		return false;
	}

	if (m_lanim_clr.m_lanimFlags.test(LA_CYCLIC) || m_lanim_clr.m_lanim_start_time < 0.0f)
	{
		return true;
	}

	const float Time = Device.dwTimeContinual / 1000.0f;
	return Time - m_lanim_clr.m_lanim_start_time < m_lanim_clr.m_lanim->Length_sec();
}

void CUILightAnimColorConrollerImpl::UpdateColorAnimation()
{
	if (m_lanim_clr.m_lanim == NULL)
	{
		return;
	}
	if (m_lanim_clr.m_lanim_start_time < 0.0f)
	{
		ResetColorAnimation();
	}

	float t = Device.dwTimeContinual / 1000.0f;

	if (t < m_lanim_clr.m_lanim_start_time) // consider animation delay
	{
		return;
	}

	if (m_lanim_clr.m_lanimFlags.test(LA_CYCLIC) || t - m_lanim_clr.m_lanim_start_time < m_lanim_clr.m_lanim->Length_sec())
	{

		int Frame;
		const u32 Color = m_lanim_clr.m_lanim->CalculateRGB(t - m_lanim_clr.m_lanim_start_time, Frame);

		if (m_lanim_clr.m_lanimFlags.test(LA_TEXTURECOLOR))
		{
			if (m_lanim_clr.m_lanimFlags.test(LA_ONLYALPHA))
			{
				ColorAnimationSetTextureColor(color_get_A(Color), true);
			}
			else
			{
				ColorAnimationSetTextureColor(Color, false);
			}
		}
		if (m_lanim_clr.m_lanimFlags.test(LA_TEXTCOLOR))
		{
			if (m_lanim_clr.m_lanimFlags.test(LA_ONLYALPHA))
			{
				ColorAnimationSetTextColor(color_get_A(Color), true);
			}
			else
			{
				ColorAnimationSetTextColor(Color, false);
			}
		}
	}
}

void CUIColorAnimConrollerContainer::Update()
{
	inherited::Update();
	UpdateColorAnimation();
}

void CUIColorAnimConrollerContainer::ColorAnimationSetTextureColor(u32 color, bool only_alpha)
{
	xrCriticalSectionGuard guard(csUi);

	for (CUIWindow* child : m_ChildWndList)
	{
		if (ITextureOwner* TO = child->ui_cast_texture_owner())
		{
			TO->SetTextureColor((only_alpha) ? subst_alpha(TO->GetTextureColor(), color) : color);
		}
	}
}

void CUIColorAnimConrollerContainer::ColorAnimationSetTextColor(u32 color, bool only_alpha)
{
	xrCriticalSectionGuard guard(csUi);

	for (CUIWindow* child : m_ChildWndList)
	{
		if (CUILightAnimColorConroller* TO = child->ui_cast_light_anim_color_controller())
		{
			TO->ColorAnimationSetTextColor(color, only_alpha);
		}
	}
}

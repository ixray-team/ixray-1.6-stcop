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
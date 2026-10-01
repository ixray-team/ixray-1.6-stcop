#include "stdafx.h"

void CRenderTarget::phase_cas()
{
	const u32 element = ps_r4_sharpening_mode == 0 ? 1 : 0;

	DrawSQ(s_cas, rt_Back_Buffer_AA, element, []
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Stencil(false);
		RCache.set_c("sharpening_intensity", ps_r4_cas_sharpening);
	});

	ResolveSurface(rt_Back_Buffer, rt_Back_Buffer_AA);
}

void CRenderTarget::phase_ui_postprocess(Fcolor* color, bool HasBackground)
{
	DrawPassSQ(s_cas, 3, [&]
	{
		RCache.set_c("ui_has_background", HasBackground ? 1.0f : 0.0f, 0.0f, 0.0f, 0.0f);

		if (color)
			RCache.set_c("static_color", color->r, color->g, color->b, color->a);
		else
			RCache.set_c("static_color", 1, 1, 1, 1);
	});
}

void CRenderTarget::phase_ui_postprocess_copy()
{
	DrawPassSQ(s_cas, 2);
}

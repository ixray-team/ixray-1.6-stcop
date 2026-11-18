#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear()
{
	GRHI->ClearTarget(rt_smap_surf->pRT, ERTColor::Black);
	GRHI->ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

RHIViewport CRenderTarget::GetSpotShadowViewport(const light& Light)
{
	constexpr float Border = 2.0f;
	const auto& Shadow = Light.X.S;
	const float Size = float(Shadow.size) - 2.0f * Border;
	return {float(Shadow.posX) + Border, float(Shadow.posY) + Border, Size, Size, 0.0f, 1.0f};
}

void CRenderTarget::phase_smap_spot(light* L)
{
	// Targets + viewport
	u_setrt(rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);

	RHIViewport Viewport = GetSpotShadowViewport(*L);
	GRHI->SetViewport(Viewport);

	// Misc - draw only front-faces //back-faces
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Stencil(false);

	// no transparency
	#pragma todo("can optimize for multi-lights covering more than say 50%...")

	RCache.set_ColorWriteEnable(false);
}

void CRenderTarget::phase_smap_spot_tsh(light* L)
{
	RCache.set_ColorWriteEnable();
}

#include "stdafx.h"

void CRenderTarget::phase_smap_spot_clear()
{
	GRHI->ClearTarget(rt_smap_surf->pRT, ERTColor::Black);
	GRHI->ClearDepthStencil(rt_smap_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
}

RHIViewport CRenderTarget::GetSpotShadowSamplingViewport(const light& Light)
{
	const auto& Shadow = Light.X.S;
	const float Size = float(Shadow.size) - 2.0f * SpotShadowBorder;
	return {float(Shadow.posX) + SpotShadowBorder, float(Shadow.posY) + SpotShadowBorder, Size, Size, 0.0f, 1.0f};
}

Fmatrix CRenderTarget::GetSpotShadowRasterProjection(const light& Light)
{
	const float TileSize = float(Light.X.S.size);
	const float Scale = (TileSize - 2.0f * SpotShadowBorder) / TileSize;

	// Render geometry into the PCF border. The original light frustum still maps to the inner viewport.
	Fmatrix Projection = Light.X.S.project;
	Projection._11 *= Scale;
	Projection._22 *= Scale;
	return Projection;
}

void CRenderTarget::phase_smap_spot(light* L)
{
	// Targets + viewport
	u_setrt(rt_smap_surf, nullptr, nullptr, rt_smap_depth->pZRT);

	const auto& Shadow = L->X.S;
	const float Size = float(Shadow.size);
	RHIViewport Viewport = {float(Shadow.posX), float(Shadow.posY), Size, Size, 0.0f, 1.0f};
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

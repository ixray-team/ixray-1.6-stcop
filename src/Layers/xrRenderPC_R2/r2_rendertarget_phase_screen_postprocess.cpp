#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"

bool UseGasmak = false;
bool UseRainDrops = false;

void CRenderTarget::RenderEffect(ScreenPostProcessType postProcessType) {
	u_setrt(rt_Generic_0, nullptr, nullptr, nullptr);

	u32 Offset = 0;
	float d_Z = EPS_S;
	float d_W = 1.0f;
	constexpr u32 color = color_rgba(0, 0, 0, 255);
	Fvector2 p0, p1;

	float _w = float(Device.TargetWidth);
	float _h = float(Device.TargetHeight);

	p0.set(.5f / _w, .5f / _h);
	p1.set((_w + .5f) / _w, (_h + .5f) / _h);

	// Configure rendering settings
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);

	// Lock and set vertices
	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(0, Device.TargetHeight, d_Z, d_W, color, p0.x, p1.y); pv++;
	pv->set(0, 0, d_Z, d_W, color, p0.x, p0.y); pv++;
	pv->set(Device.TargetWidth, Device.TargetHeight, d_Z, d_W, color, p1.x, p1.y); pv++;
	pv->set(Device.TargetWidth, 0, d_Z, d_W, color, p1.x, p0.y); pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	// Set shader and geometry
	RCache.set_Element(s_spp->E[postProcessType]);
	RCache.set_Geometry(g_combine);

	// Render
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 4, 0, 2);
	GRHI->CopySurface(rt_Color->pRT, rt_Generic_0->pRT);
}

void CRenderTarget::PhaseAberration() {
	RenderEffect(ScreenPostProcessType::Aberration);
}

void CRenderTarget::PhaseVignette() {
	RenderEffect(ScreenPostProcessType::Vignette);
}

void CRenderTarget::PhaseSaturation() {
	RenderEffect(ScreenPostProcessType::Saturation);
}

void CRenderTarget::PhaseRaindrops()
{
	const auto& Params = g_pGamePersistent->ShaderParams;
	const bool HasGlass = Params.ItemCfgHudRainDropsAvailable && Params.HelmetCondition >= 0.f;
	g_pGamePersistent->VisorRain.Update(UseRainDrops && HasGlass);
	if (g_pGamePersistent->VisorRain.GetAmount() < EPS_L)
	{
		return;
	}

	RenderEffect(g_RainDropsMode == 1 ? ScreenPostProcessType::ProceduralRaindrops : ScreenPostProcessType::Raindrops);
}
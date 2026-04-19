#include "stdafx.h"

void CRenderTarget::phase_scene_forward()
{
	u_setrt(rt_Generic_0, 0, 0, RDepth); // LDR RT
}

// startup
void CRenderTarget::phase_scene_prepare()
{
	GPU_EVENT(phase_scene_prepare);

	//	TODO: DX10: Check if we need to set RT here.
	u_setrt((u32)RCache.get_width(), (u32)RCache.get_height(), rt_Normal->pRT, NULL, NULL, RDepth);

	static float ColorRGBA[4] = { 0.5f, 0.5f, 1.0f, 1.0f };
	GRHI->ClearTarget(rt_Normal->pRT, ColorRGBA);
	GRHI->ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.0f, 0L);

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric = false;
}

void CRenderTarget::phase_scene_begin()
{
	// Enable ANISO
	SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);

	u_setrt(rt_Color, rt_Normal, rt_Surface, rt_Velocity, RDepth);

	// Stencil - write 0x1 at pixel pos
	RCache.set_Stencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_ColorWriteEnable();
}

void CRenderTarget::disable_aniso()
{
	// Disable ANISO
	SSManager.SetMaxAnisotropy(1);
}

void CRenderTarget::phase_scene_end()
{
	disable_aniso();
}

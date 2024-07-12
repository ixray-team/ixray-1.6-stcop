#include "stdafx.h"

// startup
void	CRenderTarget::phase_scene_prepare	()
{
	PIX_EVENT(phase_scene_prepare);

	//	TODO: DX10: Check if we need to set RT here.
	u_setrt((u32)RCache.get_width(), (u32)RCache.get_height(), rt_Normal->pRT, NULL, NULL, RDepth);

	FLOAT ColorRGBA[4] = { 0.5f, 0.5f, 1.0f, 1.0f };

	RContext->ClearRenderTargetView(rt_Normal->pRT, ColorRGBA);
	RContext->ClearDepthStencilView(RDepth, D3D_CLEAR_DEPTH | D3D_CLEAR_STENCIL, 1.0f, 0);

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric = false;
	//	Clear later if try to draw volumetric
}

// begin
void	CRenderTarget::phase_scene_begin	()
{
	// Enable ANISO
	SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);

   u_setrt(rt_Normal, rt_Color, rt_Surface, rt_Velocity, RDepth);

	// Stencil - write 0x1 at pixel pos
   RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x01, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);

   RCache.set_CullMode(CULL_CCW);
   RCache.set_ColorWriteEnable();
}

void	CRenderTarget::disable_aniso		()
{
	// Disable ANISO
	SSManager.SetMaxAnisotropy(1);
}

// end
void	CRenderTarget::phase_scene_end		()
{
	disable_aniso();
}

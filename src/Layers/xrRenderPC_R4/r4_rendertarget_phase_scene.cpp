#include "stdafx.h"

// startup
void	CRenderTarget::phase_scene_prepare	()
{
	PIX_EVENT(phase_scene_prepare);

	//	TODO: DX10: Check if we need to set RT here.
	u_setrt(RCache.get_width(), RCache.get_height(), rt_Position->pRT, rt_Velocity->pRT, NULL, RDepth);

	FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};

	RContext->ClearRenderTargetView(rt_Position->pRT, ColorRGBA);
	RContext->ClearRenderTargetView(rt_Velocity->pRT, ColorRGBA);

	RContext->ClearDepthStencilView(RDepth, D3D_CLEAR_DEPTH | D3D_CLEAR_STENCIL, 1.0f, 0);

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric				= false;
	//	Clear later if try to draw volumetric
}

// begin
void	CRenderTarget::phase_scene_begin	()
{
	// Enable ANISO
	SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);

   ID3DDepthStencilView* pZB = RDepth;

   dwWidth = get_width();
   dwHeight = get_height();

   if (!RImplementation.o.dx10_gbuffer_opt)
   {
	   u_setrt(rt_Position, rt_Normal, rt_Color, rt_Velocity, pZB);
   }
   else
   {
	   u_setrt(rt_Position, rt_Color, rt_Velocity, pZB);
   }

	// Stencil - write 0x1 at pixel pos
   RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x01, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);

	// Misc		- draw only front-faces
	//	TODO: DX10: siable two-sided stencil here
	//CHK_DX(RDevice->SetRenderState	( D3DRS_TWOSIDEDSTENCILMODE,FALSE				));
	RCache.set_CullMode					( CULL_CCW );
	RCache.set_ColorWriteEnable			( );
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

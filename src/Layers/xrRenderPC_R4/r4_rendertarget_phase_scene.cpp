#include "stdafx.h"

// startup
void	CRenderTarget::phase_scene_prepare	()
{
	PIX_EVENT(phase_scene_prepare);

	CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
	float fValue = E.m_fSunShaftsIntensity;
	if ( RImplementation.o.advancedpp && (
				ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES|R2FLAG_DOF) ||
				( (ps_r_sun_shafts>0) && (fValue>=0.0001) ) ||
				(ps_r_ssao>0)
	))
	{
		//	TODO: DX10: Check if we need to set RT here.
   		u_setrt(RCache.get_width(), RCache.get_height(), rt_Position->pRT, NULL, NULL, rt_HWDepth->pZRT);
      
		//CHK_DX	( HW.pDevice->Clear	( 0L, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
		FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};
		HW.pContext->ClearRenderTargetView(rt_Position->pRT, ColorRGBA);
        HW.pContext->ClearDepthStencilView(rt_HWDepth->pZRT, D3D_CLEAR_DEPTH|D3D_CLEAR_STENCIL, 1.0f, 0);
   }
	else
	{
		//	TODO: DX10: Check if we need to set RT here.
        u_setrt	(RCache.get_width(), RCache.get_height(), HW.pBaseRT, NULL, NULL, rt_HWDepth->pZRT);
        HW.pContext->ClearDepthStencilView(rt_HWDepth->pZRT, D3D_CLEAR_DEPTH|D3D_CLEAR_STENCIL, 1.0f, 0);
	}

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric				= false;
	//	Clear later if try to draw volumetric
}

// begin
void	CRenderTarget::phase_scene_begin	()
{
	// Enable ANISO
	SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);

   ID3DDepthStencilView* pZB = rt_HWDepth->pZRT;

   dwWidth = get_width();
   dwHeight = get_height();
   if (!RImplementation.o.dx10_gbuffer_opt)
   {
	   RCache.set_RT(rt_Position->pRT, 0);
	   RCache.set_RT(rt_Normal->pRT, 1);
	   RCache.set_RT(rt_Color->pRT, 2);
	   RCache.set_RT(rt_Motion->pRT, 3);
   }
   else
   {
	   RCache.set_RT(rt_Position->pRT, 0);
	   RCache.set_RT(rt_Color->pRT, 1);
	   RCache.set_RT(rt_Motion->pRT, 2);
   }
	
	// Stencil - write 0x1 at pixel pos
	RCache.set_Stencil					( TRUE,D3DCMP_ALWAYS,0x01,0xff,0x7f,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);

	// Misc		- draw only front-faces
	//	TODO: DX10: siable two-sided stencil here
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
	disable_aniso	();
}

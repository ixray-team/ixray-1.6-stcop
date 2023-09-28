#include "stdafx.h"

// startup
void	CRenderTarget::phase_scene_prepare	()
{
	PIX_EVENT(phase_scene_prepare);
	// Clear depth & stencil
	//u_setrt	( Device.TargetWidth,Device.TargetHeight,HW.pBaseRT,NULL,NULL,HW.pBaseZB );
	//CHK_DX	( HW.pDevice->Clear	( 0L, NULL, D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
	//	Igor: soft particles

	CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
	float fValue = E.m_fSunShaftsIntensity;
	//	TODO: add multiplication by sun color here
	//if (fValue<0.0001) FlagSunShafts = 0;

	//	TODO: DX10: Check if complete clear of _ALL_ rendertargets will increase
	//	FPS. Make check for SLI configuration.
	if ( RImplementation.o.advancedpp &&
			(
				ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES|R2FLAG_DOF) ||
				( (ps_r_sun_shafts>0) && (fValue>=0.0001) ) ||
				(ps_r_ssao>0)
			)
		)
	{
		//	TODO: DX10: Check if we need to set RT here.
		u_setrt(Device.TargetWidth, Device.TargetHeight, rt_Position->pRT, NULL, NULL, HW.pBaseZB);

		//CHK_DX	( HW.pDevice->Clear	( 0L, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
		FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};
		HW.pContext->ClearRenderTargetView(rt_Position->pRT, ColorRGBA);
		//HW.pContext->ClearRenderTargetView(rt_Normal->pRT, ColorRGBA);
		//HW.pContext->ClearRenderTargetView(rt_Color->pRT, ColorRGBA);
		HW.pContext->ClearDepthStencilView(HW.pBaseZB, D3D_CLEAR_DEPTH | D3D_CLEAR_STENCIL, 1.0f, 0);
	}
	else
	{
		u_setrt(Device.TargetWidth, Device.TargetHeight, HW.pBaseRT, NULL, NULL, HW.pBaseZB);
		//CHK_DX	( HW.pDevice->Clear	( 0L, NULL, D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
		HW.pContext->ClearDepthStencilView(HW.pBaseZB, D3D_CLEAR_DEPTH | D3D_CLEAR_STENCIL, 1.0f, 0);
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

   ID3DDepthStencilView* pZB = HW.pBaseZB;

	// Targets, use accumulator for temporary storage
   if( !RImplementation.o.dx10_gbuffer_opt )
   {
	   u_setrt(rt_Position, rt_Normal, rt_Color, pZB);
   }
   else
   {
	   u_setrt(rt_Position, rt_Color, pZB);
	   //else								u_setrt		(rt_Position,	rt_Color, rt_Normal,		pZB);
   }

	// Stencil - write 0x1 at pixel pos
	RCache.set_Stencil					( TRUE,D3DCMP_ALWAYS,0x01,0xff,0x7f,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);

	// Misc		- draw only front-faces
	//	TODO: DX10: siable two-sided stencil here
	//CHK_DX(HW.pDevice->SetRenderState	( D3DRS_TWOSIDEDSTENCILMODE,FALSE				));
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

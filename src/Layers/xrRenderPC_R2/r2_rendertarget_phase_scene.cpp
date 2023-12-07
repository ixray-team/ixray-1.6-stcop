#include "stdafx.h"

// startup
void	CRenderTarget::phase_scene_prepare	()
{
	// Clear depth & stencil
	//u_setrt	( Device.TargetWidth,Device.TargetHeight,RTarget,NULL,NULL,RDepth );
	//CHK_DX	( RDevice->Clear	( 0L, NULL, D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
	//	Igor: soft particles

	// we need to clean up G-buffer every frame to avoid some glithces
	u_setrt(rt_Position, rt_Normal, rt_Color, 0);
	CHK_DX(RDevice->Clear(0L, NULL, D3DCLEAR_TARGET, 0x0, 1.0f, 0L));

	CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
	float fValue = E.m_fSunShaftsIntensity;
	//	TODO: add multiplication by sun color here
	//if (fValue<0.0001) FlagSunShafts = 0;

	if (ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES | R2FLAG_DOF) ||
		((ps_r_sun_shafts > 0) && (fValue >= 0.0001)) ||
		(ps_r_ssao > 0))
	{
		u_setrt	( RCache.get_width(),RCache.get_height(),rt_Position->pRT,NULL,NULL,RDepth );
		CHK_DX	( RDevice->Clear	( 0L, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
	}
	else
	{
		u_setrt	( RCache.get_width(),RCache.get_height(),RTarget,NULL,NULL,RDepth );
		CHK_DX	( RDevice->Clear	( 0L, NULL, D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
	}

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric				= false;
	//	Clear later if try to draw volumetric
}

// begin
void	CRenderTarget::phase_scene_begin	()
{
	// Enable ANISO
	for (u32 i=0; i<dxRenderDeviceRender::Instance().Caps.raster.dwStages; i++)
		CHK_DX(RDevice->SetSamplerState( i, D3DSAMP_MAXANISOTROPY, ps_r__tf_Anisotropic	));

	// Targets, use accumulator for temporary storage
	u_setrt(rt_Position, rt_Normal, rt_Color, RDepth);

	// Stencil - write 0x1 at pixel pos
	RCache.set_Stencil					( TRUE,D3DCMP_ALWAYS,0x01,0xff,0xff,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);

	// Misc		- draw only front-faces
	CHK_DX(RDevice->SetRenderState	( D3DRS_TWOSIDEDSTENCILMODE,FALSE				));
	RCache.set_CullMode					( CULL_CCW );
	RCache.set_ColorWriteEnable			( );
}

void	CRenderTarget::disable_aniso		()
{
	// Disable ANISO
	for (u32 i=0; i<dxRenderDeviceRender::Instance().Caps.raster.dwStages; i++)
		CHK_DX(RDevice->SetSamplerState( i, D3DSAMP_MAXANISOTROPY, 1	));
}

// end
void	CRenderTarget::phase_scene_end		()
{
	disable_aniso	();
}

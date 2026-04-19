#include "stdafx.h"

// startup
void	CRenderTarget::phase_scene_prepare	()
{
	// Clear depth & stencil
	//u_setrt	( Device.TargetWidth,Device.TargetHeight,RTarget,nullptr,nullptr,RDepth );
	//CHK_DX	( RDevice->Clear	( 0L, nullptr, D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L) );
	//	Igor: soft particles

	// we need to clean up G-buffer every frame to avoid some glithces
	u_setrt(rt_Position, rt_Normal, rt_Color, 0);
	GRHI->ClearTarget(RCache.get_RT());

	CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
	float fValue = E.m_fSunShaftsIntensity;

	u_setrt(RCache.get_width(), RCache.get_height(), rt_Position->pRT, nullptr, nullptr, RDepth);
	GRHI->ClearTarget(RCache.get_RT());
	GRHI->ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.0f, 0);

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric				= false;
	//	Clear later if try to draw volumetric
}

// begin
void	CRenderTarget::phase_scene_begin	()
{
	// Enable ANISO
	for (u32 i=0; i<Caps.raster.dwStages; i++)
		CHK_DX(RDevice->SetSamplerState( i, D3DSAMP_MAXANISOTROPY, ps_r__tf_Anisotropic	));

	// Targets, use accumulator for temporary storage
	u_setrt(rt_Position, rt_Normal, rt_Color, RDepth);

	// Stencil - write 0x1 at pixel pos
	RCache.set_Stencil					( true,D3DCMP_ALWAYS,0x01,0xff,0xff,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);

	// Misc		- draw only front-faces
	CHK_DX(RDevice->SetRenderState	( D3DRS_TWOSIDEDSTENCILMODE,false				));
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_ColorWriteEnable			( );
}

void	CRenderTarget::disable_aniso		()
{
	// Disable ANISO
	for (u32 i=0; i<Caps.raster.dwStages; i++)
		CHK_DX(RDevice->SetSamplerState( i, D3DSAMP_MAXANISOTROPY, 1	));
}

// end
void	CRenderTarget::phase_scene_end		()
{
	disable_aniso	();
}

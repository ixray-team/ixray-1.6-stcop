#include "stdafx.h"

void CRenderTarget::phase_scene_forward()
{
	u_setrt(rt_Generic_0, rt_Velocity, 0, RDepth);
}

// startup
void CRenderTarget::phase_scene_prepare()
{
	GPU_EVENT(phase_scene_prepare);

	GRHI->ClearTarget(rt_Normal->pRT, ERTColor::Gray);
	GRHI->ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.0f, 0L);

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric = false;
}

void CRenderTarget::phase_scene_begin()
{
	SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);

	if (!RImplementation.o.dx11_disable_motion_vectors)
	{
		u_setrt(rt_Color, rt_Normal, rt_Surface, rt_Velocity, RDepth);
	}
	else
	{
		u_setrt(rt_Color, rt_Normal, rt_Surface, RDepth);
	}

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

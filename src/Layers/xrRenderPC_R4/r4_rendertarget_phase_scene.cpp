#include "stdafx.h"
#include "StateManager/dx10SamplerStateCache.h"

void CRenderTarget::phase_scene_forward(GfxCmdList& Cmd)
{
	if (RImplementation.o.dx11_disable_motion_vectors)
	{
		if (RImplementation.o.dx11_allow_wboit_transparency)
		{
			u_setrt(Cmd, rt_Forward, rt_Revealage, RDepth);
		}
		else
		{
			u_setrt(Cmd, rt_Generic_0, nullptr, RDepth);
		}
	}
	else 
	{
		if (RImplementation.o.dx11_allow_wboit_transparency)
		{
			u_setrt(Cmd, rt_Forward, rt_Velocity, rt_Revealage, RDepth);
		}
		else
		{
			u_setrt(Cmd, rt_Generic_0, rt_Velocity, RDepth);
		}
	}
}

// startup
void CRenderTarget::phase_scene_prepare()
{
	GPU_EVENT(phase_scene_prepare);

	RImplementation.Imm().ClearTarget(rt_Normal->pRT, ERTColor::Gray);
	RImplementation.Imm().ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.0f, 0L);

	//	Igor: for volumetric lights
	m_bHasActiveVolumetric = false;
}

void CRenderTarget::phase_scene_begin(GfxCmdList& Cmd)
{
	SSManager.SetMaxAnisotropy(ps_r__tf_Anisotropic);

	if (!RImplementation.o.dx11_disable_motion_vectors)
	{
		u_setrt(Cmd, rt_Color, rt_Normal, rt_Surface, rt_Velocity, RDepth);
	}
	else
	{
		u_setrt(Cmd, rt_Color, rt_Normal, rt_Surface, RDepth);
	}

	Cmd.SetStencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0x7f, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
	
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetColorWriteEnable();
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

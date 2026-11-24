#include "stdafx.h"

void CRenderTarget::phase_smaa(GfxCmdList& Cmd)
{
	DrawSQ(Cmd, s_smaa, rt_smaa_edgetex, 0, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Cmd.SetStencil(true, D3DCMP_ALWAYS, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		Cmd.ClearTarget(Cmd.GetRenderTarget());
	});

	DrawSQ(Cmd, s_smaa, rt_smaa_blendtex, 1, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Cmd.SetStencil(true, D3DCMP_EQUAL, 0x1, 0, 0, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		Cmd.ClearTarget(Cmd.GetRenderTarget());
	});

	DrawSQ(Cmd, s_smaa, rt_Generic_2, 2, [&]
	{
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Cmd.SetStencil(false);
	});

	ResolveSurface(rt_Generic_0, rt_Generic_2);
}

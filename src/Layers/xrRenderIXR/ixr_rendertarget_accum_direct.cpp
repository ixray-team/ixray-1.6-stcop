#include "stdafx.h"
#include "SunCloudMask.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

void CRenderTarget::accum_direct_volumetric(GfxCmdList& Cmd)
{
	GPU_EVENT(accum_direct_volumetric);

	if (!need_to_render_sunshafts())
		return;

	light* fuckingsun = (light*)RImplementation.Lights.sun_adapted._get();

	Fvector L_clr;
	L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);

	Fvector L_dir;
	L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);

	RFrame.mView.transform_dir(L_dir, fuckingsun->direction);
	L_dir.normalize();

	phase_vol_accumulator(Cmd);
	Cmd.SetColorWriteEnable();

	DrawPassSQ(Cmd, s_accum_direct_volumetric, 0, [&]
	{
		Cmd.Constants.Write<CBLight>().Ldynamic_dir.set(L_dir.x, L_dir.y, L_dir.z, 0);
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, 0);
	});
}

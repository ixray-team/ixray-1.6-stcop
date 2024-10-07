#include "stdafx.h"

#include "../Include/xrRender/DrawUtils.h"
#include "Render.h"
#include "IGame_Persistent.h"

#include "IGame_Level.h"
#include "CustomHUD.h"

extern BOOL bNeed_re_create_env;

void CRenderDevice::_Destroy	(BOOL bKeepTextures)
{
	DU->OnDeviceDestroy();

	// before destroy
	b_is_Ready					= FALSE;
	Statistic->OnDeviceDestroy	();
	::Render->destroy			();
	m_pRender->OnDeviceDestroy(bKeepTextures);

	Memory.mem_compact			();
}

void CRenderDevice::Destroy()
{
	if (!b_is_Ready)			
		return;

	Log("Destroying Direct3D...");

	SDL_ShowCursor();
	m_pRender->ValidateHW();

	_Destroy					(FALSE);

	// real destroy
	m_pRender->DestroyHW();

	seqRender.R.clear			();
	seqAppActivate.R.clear		();
	seqAppDeactivate.R.clear	();
	seqAppStart.R.clear			();
	seqAppEnd.R.clear			();
	seqFrame. R.clear			();
	seqFrameMT.R.clear			();
	seqDeviceReset.R.clear		();
	seqParallel.clear			();
	seqParallelRender.clear		();
	m_time_callbacks.clear		();
	RenderFactory->DestroyRenderDeviceRender(m_pRender);
	m_pRender = 0;
	xr_delete					(Statistic);

	DestroyRenderDevice();
}

extern ENGINE_API float ps_render_scale;
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API u32 ps_r_scale_mode;
extern ENGINE_API u32 ps_proxy_r_scale_mode;

void CRenderDevice::Reset(bool precache)
{
	ps_r_scale_mode = ps_proxy_r_scale_mode;
	u32 dwWidth_before = TargetWidth;
	u32 dwHeight_before = TargetHeight;
	float RenderScale_before = g_RenderRHI->m_RenderScale;

	u32 tm_start = TimerAsync();

	if (ps_render_scale > 1.0f && ps_proxy_r_scale_mode > 1)
	{
		ps_render_scale = 1.0f;
	}

	if(ps_render_scale_preset < 5 && ps_proxy_r_scale_mode > 1) {
		static float ScalePresets[] = {1.0f, 1.5f, 1.724f, 2.0f, 3.0f};
		g_RenderRHI->m_RenderScale = 1.0f / ScalePresets[ps_render_scale_preset];
		ps_render_scale = g_RenderRHI->m_RenderScale;
	}
	else {
		g_RenderRHI->m_RenderScale = ps_render_scale;
	}

	m_pRender->Reset(g_AppInfo.Window, TargetWidth, TargetHeight, HalfTargetWidth, HalfTargetHeight);

	if (g_pGamePersistent)
	{
		g_pGamePersistent->Environment().bNeed_re_create_env = TRUE;
	}
	_SetupStates();
	
	if (precache)
		PreCache(20, true, false);

	u32 tm_end = TimerAsync();
	Msg("*** RESET [%d ms]", tm_end - tm_start);

	//	TODO: Remove this! It may hide crash
	Memory.mem_compact();

	if (!g_dedicated_server)
	{
		SDL_ShowCursor();
	}

	seqDeviceReset.Process(rp_DeviceReset);

	if (dwWidth_before != TargetWidth || dwHeight_before != TargetHeight || RenderScale_before != g_RenderRHI->m_RenderScale)
	{
		seqResolutionChanged.Process(rp_ScreenResolutionChanged);
	}
}

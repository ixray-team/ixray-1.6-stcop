#include "stdafx.h"

#include "../Include/xrRender/DrawUtils.h"
#include "Render.h"
#include "IGame_Persistent.h"

#include "IGame_Level.h"
#include "CustomHUD.h"

extern bool bNeed_re_create_env;

void CRenderDevice::_Destroy	(bool bKeepTextures)
{
	DU->OnDeviceDestroy();

	// before destroy
	b_is_Ready					= false;
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

	_Destroy					(false);
	
	auto& hGameRef = Engine.External.hGame;
	if (hGameRef)
	{
		using xrGameRenderPreDestroy = void();
		xrGameRenderPreDestroy* pxrGameRenderPreDestroy = (xrGameRenderPreDestroy*)Platform::GetAddress(hGameRef, "xrGameRenderPreDestroy");
		R_ASSERT(pxrGameRenderPreDestroy);
		pxrGameRenderPreDestroy();
	}

	// real destroy
	m_pRender->DestroyHW();

	seqRender.pure_objects.clear			();
	seqAppActivate.pure_objects.clear		();
	seqAppDeactivate.pure_objects.clear	();
	seqAppStart.pure_objects.clear			();
	seqAppEnd.pure_objects.clear			();
	seqFrame. pure_objects.clear			();
	seqFrameMT.pure_objects.clear			();
	seqDeviceReset.pure_objects.clear		();
	seqParallel.clear			();
	seqParallelRender.clear		();
	m_time_callbacks.clear		();
	RenderFactory->DestroyRenderDeviceRender(m_pRender);
	m_pRender = 0;

	xr_delete(Statistic);

	DestroyRenderDevice();
}

extern ENGINE_API float ps_render_scale;
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API u32 ps_r_scale_mode;
extern ENGINE_API u32 ps_proxy_r_scale_mode;

void CRenderDevice::Reset(bool precache)
{
	PROF_EVENT("CRenderDevice::Reset");
	ps_r_scale_mode = ps_proxy_r_scale_mode;
	u32 dwWidth_before = TargetWidth;
	u32 dwHeight_before = TargetHeight;
	float RenderScale_before = GRHI->DevicePtr->RenderScale;

	u32 tm_start = TimerAsync();

	if (ps_render_scale > 1.0f && ps_proxy_r_scale_mode > 1)
	{
		ps_render_scale = 1.0f;
	}

	if(ps_render_scale_preset < 5 && ps_proxy_r_scale_mode > 1)
	{
		static float ScalePresets[] = {1.0f, 1.5f, 1.724f, 2.0f, 3.0f};
		GRHI->DevicePtr->RenderScale = 1.0f / ScalePresets[ps_render_scale_preset];

		m_pRender->GetRenderScale(GRHI->DevicePtr->RenderScale);
	}
	else
	{
		GRHI->DevicePtr->RenderScale = ps_render_scale;
	}

	m_pRender->Reset(g_AppInfo.Window, TargetWidth, TargetHeight);

	if (g_pGamePersistent)
	{
		g_pGamePersistent->Environment().bNeed_re_create_env = true;
	}

	_SetupStates();
	
	if (precache)
	{
		PreCache(20, true, false);
	}

	u32 tm_end = TimerAsync();
	Msg("*** RESET [%d ms]", tm_end - tm_start);

	//	TODO: Remove this! It may hide crash
	Memory.mem_compact();

	if (!g_dedicated_server)
	{
		SDL_ShowCursor();
	}

	seqDeviceReset.Process<&pureDeviceReset::OnDeviceReset>();

	if (dwWidth_before != TargetWidth || dwHeight_before != TargetHeight || RenderScale_before != GRHI->DevicePtr->RenderScale)
	{
		seqResolutionChanged.Process<&pureScreenResolutionChanged::OnScreenResolutionChanged>();
	}
}

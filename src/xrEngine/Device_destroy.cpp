#include "stdafx.h"

#include "../Include/xrRender/DrawUtils.h"
#include "render.h"
#include "IGame_Persistent.h"
#include "xr_IOConsole.h"

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

void CRenderDevice::Destroy	(void) {
	if (!b_is_Ready)			return;

	Log("Destroying Direct3D...");

	ShowCursor	(TRUE);
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

	RenderFactory->DestroyRenderDeviceRender(m_pRender);
	m_pRender = 0;
	xr_delete					(Statistic);
}

#include "IGame_Level.h"
#include "CustomHUD.h"
extern BOOL bNeed_re_create_env;
void CRenderDevice::Reset		(bool precache)
{
	u32 dwWidth_before		= TargetWidth;
	u32 dwHeight_before		= TargetHeight;

	ShowCursor				(TRUE);
	u32 tm_start			= TimerAsync();

	m_pRender->Reset( m_hWnd, TargetWidth, TargetHeight, HalfTargetWidth, HalfTargetHeight);

	if (g_pGamePersistent)
	{
		g_pGamePersistent->Environment().bNeed_re_create_env = TRUE;
	}
	_SetupStates			();
	if (precache)
		PreCache			(20, true, false);
	u32 tm_end				= TimerAsync();
	Msg						("*** RESET [%d ms]",tm_end-tm_start);

	//	TODO: Remove this! It may hide crash
	Memory.mem_compact();

	if (!g_dedicated_server) {
		ShowCursor(FALSE);
	}

	seqDeviceReset.Process(rp_DeviceReset);

	if(dwWidth_before!= TargetWidth || dwHeight_before!= TargetHeight)
	{
		seqResolutionChanged.Process(rp_ScreenResolutionChanged);
	}
}

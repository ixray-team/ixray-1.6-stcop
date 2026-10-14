#include "stdafx.h"
#include "r4_rendertarget.h"
#include "../../xrEngine/IGame_Persistent.h"

void CRenderTarget::phase_nvg()
{
	const float TargetInterference = clampr(Device.hudViewportData.CurrentElectronicsProblemsCnt / 10.0f, 0.0f, 1.0f);
	if (!NightVisionInterferenceInitialized)
	{
		// A device enabled during an emission must start with its current interference.
		NightVisionInterference = TargetInterference;
		NightVisionInterferenceInitialized = true;
	}
	else
	{
		const float MaxDelta = 0.2f * clampr(Device.fTimeDelta, 0.0f, 0.1f);
		NightVisionInterference += clampr(TargetInterference - NightVisionInterference, -MaxDelta, MaxDelta);
	}

	u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(false);
	RCache.set_Element(s_nvg->E[0]);
	RCache.set_c("NightVisionPostprocess", g_pGamePersistent->ShaderParams.NightVisionPostprocess);
	RCache.set_c("NightVisionInterference", NightVisionInterference, 0.0f, 0.0f, 0.0f);
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	GRHI->CopySurface(rt_Back_Buffer->pSurface, rt_Back_Buffer_AA->pSurface);
}

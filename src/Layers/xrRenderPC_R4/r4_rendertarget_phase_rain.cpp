#include "stdafx.h"

void CRenderTarget::phase_rain(GfxCmdList& Cmd)
{
	u_setrt(Cmd, rt_Color, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();
}
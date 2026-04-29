#include "stdafx.h"

void CRenderTarget::phase_rain()
{
	u_setrt(rt_Color, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();
}
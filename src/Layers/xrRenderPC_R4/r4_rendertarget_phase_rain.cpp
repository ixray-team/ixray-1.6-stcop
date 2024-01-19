#include "stdafx.h"

void CRenderTarget::phase_rain()
{
	u_setrt(rt_Color, nullptr, nullptr, RDepth);
	//u_setrt	(rt_Normal,nullptr,nullptr,RDepth);
	RImplementation.rmNormal();
}
#include "stdafx.h"

void CRenderTarget::phase_rain()
{
	u_setrt(rt_Color, NULL, NULL, RDepth);
	//u_setrt	(rt_Normal,NULL,NULL,RDepth);
	RImplementation.rmNormal();
}
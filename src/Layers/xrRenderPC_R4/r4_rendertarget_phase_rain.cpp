#include "stdafx.h"

void CRenderTarget::phase_rain()
{
	u_setrt(rt_Color, NULL, NULL, HW.pBaseZB);
	//u_setrt	(rt_Normal,NULL,NULL,HW.pBaseZB);
	RImplementation.rmNormal();
}
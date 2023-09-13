#include "stdafx.h"

void CRenderTarget::phase_rain()
{
    u_setrt	(rt_Color,NULL,NULL, rt_HWDepth->pZRT);
	RImplementation.rmNormal();
}
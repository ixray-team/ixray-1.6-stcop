#include "stdafx.h"

void CRenderTarget::phase_scale()
{
	DrawSQ(s_scale, rt_Generic, ps_r_scale_mode, []
	{
		RImplementation.rmNormal();
	});
}

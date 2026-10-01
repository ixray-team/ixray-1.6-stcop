#include "stdafx.h"

void CRenderTarget::phase_fxaa()
{
	DrawSQ(s_fxaa, rt_Generic_2);
	ResolveSurface(rt_Generic_0, rt_Generic_2);
}

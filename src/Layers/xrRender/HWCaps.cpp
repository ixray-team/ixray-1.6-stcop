#include "stdafx.h"
#pragma hdrstop

#include "hwcaps.h"
#include "hw.h"

#ifndef _EDITOR
u32 GetGpuNum()
{
	u32 res = 0;
	res = _max(res, 2);
	res = _min(res, CHWCaps::MAX_GPUS);

	//	It's vital to have at least one GPU, else
	//	code will fail.
	VERIFY(res>0);

	EngineLog("* Starting rendering as {}-GPU.", res);
	
	return res;
}
#else
u32 GetGpuNum()
{
	return 1;
}
#endif
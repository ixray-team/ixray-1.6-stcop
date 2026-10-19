#include "stdafx.h"

#if defined(IXR_APPLE_SERIES)

void CRenderTarget::init_fsr()
{
}

bool CRenderTarget::phase_fsr()
{
	return false;
}

void CRenderTarget::init_dlss()
{
}

bool CRenderTarget::phase_dlss()
{
	return false;
}
#endif

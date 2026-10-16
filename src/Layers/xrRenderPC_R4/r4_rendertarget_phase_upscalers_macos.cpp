#include "stdafx.h"

#if defined(IXR_MACOS)
// AI: DLSS and FSR3 come from Windows-only vendor SDKs and the macOS plan leaves both upscalers
// AI: out, so their real sources are excluded for APPLE in CMakeLists.txt. The renderer's entry
// AI: points still have to exist, otherwise the link fails; here they answer "not handled" and
// AI: the renderer falls back to its own anti-aliasing path. Windows keeps the real ones.

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

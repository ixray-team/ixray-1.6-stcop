#include "../RHI.h"
#include "AMDAntiLag.h"

#ifdef IXR_WINDOWS
#   include "../D3D11/Device.h"
#endif

CAMDAntiLag::CAMDAntiLag()
{
	if (GRHI->APILevel != ERHI_API_LAYER::D3D11)
	{
		return;
	}

#ifdef IXR_WINDOWS
	const HRESULT hr = AMD::AntiLag2DX11::Initialize(&Context);
	IsSupported = hr == S_OK;
#endif
}

CAMDAntiLag::~CAMDAntiLag()
{
	if (!IsSupported)
	{
		return;
	}

#ifdef IXR_WINDOWS
	AMD::AntiLag2DX11::DeInitialize(&Context);
#endif
}

void CAMDAntiLag::Update()
{
	if (!IsSupported)
	{
		return;
	}

#ifdef IXR_WINDOWS
	AMD::AntiLag2DX11::Update(&Context, true, 0);
#endif
}
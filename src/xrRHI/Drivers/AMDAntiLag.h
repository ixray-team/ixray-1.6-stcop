#pragma once

#ifdef IXR_WINDOWS
#   include <d3d11.h>
#   include <AMD/AntiLag/ffx_antilag2_dx11.h>
#endif

class CAMDAntiLag
{
public:
	CAMDAntiLag();
	~CAMDAntiLag();

	void Update();

private:
	bool IsSupported = false;
#ifdef IXR_WINDOWS
	AMD::AntiLag2DX11::Context Context;
#endif
};
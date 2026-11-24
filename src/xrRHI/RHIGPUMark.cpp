/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "RHI.h"
#include "RHIGPUMark.h"

#include "D3D11/DX11GPUEventWrapper.h"
#ifdef IXR_WINDOWS
#include "D3D12/GPUEvents.h"
#endif

CRHIGPUMark::CRHIGPUMark(const char* name, const wchar_t* wname, const void* profiler_location)
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: Annotation = new InternalDX11GPUEventWrapper(name, wname, profiler_location); break;
        case ERHI_API_LAYER::D3D12: Annotation = new InternalDX12GPUEventWrapper(name, profiler_location); break;
#endif
	}
}

CRHIGPUMark::~CRHIGPUMark()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: xr_delete((InternalDX11GPUEventWrapper*)Annotation); break;
        case ERHI_API_LAYER::D3D12: xr_delete((InternalDX12GPUEventWrapper*)Annotation); break;
#endif
	}
}

void
CRHI::CollectGPUProfiler()
{
#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
    if (g_tracyD3D12GPUContext)
    {
        TracyD3D12Collect(g_tracyD3D12GPUContext);
    }
	if (g_tracyD3D11GPUContext) {
		TracyD3D11Collect(g_tracyD3D11GPUContext);
	}
#endif
}
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
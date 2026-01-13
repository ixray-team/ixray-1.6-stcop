#include "RHI.h"
#include "RHIGPUMark.h"

#include "D3D11/DX11GPUEventWrapper.h"
#include "D3D9/DX9GPUEventWrapper.h"

CRHIGPUMark::CRHIGPUMark(const char* name, const wchar_t* wname)
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: Annotation = new InternalDX11GPUEventWrapper(name, wname); break;
		case ERHI_API_LAYER::D3D9:  Annotation = new InternalDX9GPUEventWrapper(name, wname);  break;
#endif
	}
}

CRHIGPUMark::~CRHIGPUMark()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: xr_delete((InternalDX11GPUEventWrapper*)Annotation); break;
		case ERHI_API_LAYER::D3D9:  xr_delete((InternalDX9GPUEventWrapper*)Annotation);  break;
#endif
	}
}
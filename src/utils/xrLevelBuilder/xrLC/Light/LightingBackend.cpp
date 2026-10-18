#include "stdafx.h"
#include "LightingBackend.h"
#include "../../UI/CompilersUI.h"
#include "Embree/EmbreeLightingBackend.h"

#ifdef LCCUDA_BUILD
#	include "CUDA/CudaLightingBackend.h"
#endif

ILightingBackend& GetLightingBackend()
{
	static EmbreeLightingBackend Embree;

#ifdef LCCUDA_BUILD
	static CudaLightingBackend Cuda;
	if (gCompilerMode.CUDA)
	{
		return Cuda;
	}
#endif

	return Embree;
}

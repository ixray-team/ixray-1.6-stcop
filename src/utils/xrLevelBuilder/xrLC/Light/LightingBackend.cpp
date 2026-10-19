#include "stdafx.h"
#include "LightingBackend.h"
#include "../../UI/CompilersUI.h"
#include "Embree/EmbreeLightingBackend.h"

#ifdef LCGPU_BUILD
#	include "GPU/GpuLightingBackend.h"
#	ifdef LCCUDA_BUILD
#		include "CUDA/CudaRayTracer.h"
#	endif
#	ifdef LCVULKAN_BUILD
#		include "Vulkan/VulkanRayTracer.h"
#	endif
#endif

ILightingBackend& GetLightingBackend()
{
	static EmbreeLightingBackend Embree;

#ifdef LCCUDA_BUILD
	static CudaRayTracer CudaTracer;
	static GpuLightingBackend Cuda(CudaTracer);
	extern bool IsCudaAvailable();
	if (gCompilerMode.CUDA && IsCudaAvailable())
	{
		return Cuda;
	}
#endif

#ifdef LCVULKAN_BUILD
	static VulkanRayTracer VulkanTracer;
	static GpuLightingBackend Vulkan(VulkanTracer);
	if (gCompilerMode.Vulkan)
	{
		return Vulkan;
	}
#endif

	return Embree;
}

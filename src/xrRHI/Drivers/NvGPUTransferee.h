#pragma once
// NVidia detection system

#include "../RHI.h"
#include <nvapi\nvapi.h>

#define NVAPI_MAX_USAGES_PER_GPU 34
class IUnknown;

class CNvReader :
	public IRHIGPU
{
	using NvAPI_QueryInterface_t = int *(*)(unsigned int offset);
	using NvAPI_Initialize_t = int(*)();
	using NvAPI_EnumPhysicalGPUs_t = int(*)(int **handles, unsigned long *count);
	using NvAPI_EnumLogicalGPUs_t = int(*)(int **handles, unsigned long *count);
	using NvAPI_GPU_GetUsages_t = int(*)(int *handle, unsigned int *usages);
	using NvAPI_PhysicalFromLogical = int(*)(int* handle1, int** handle, unsigned long* count);
	using NvAPI_D3D11_SetDepthBoundsTest_t = int(*)(IUnknown* pDeviceOrContext, unsigned int bEnable, float fMinDepth, float fMaxDepth);

private:
	NvAPI_QueryInterface_t      NvAPI_QueryInterface;
	NvAPI_Initialize_t          NvAPI_Initialize;
	NvAPI_EnumPhysicalGPUs_t    NvAPI_EnumPhysicalGPUs;
	NvAPI_EnumLogicalGPUs_t     NvAPI_EnumLogicalGPUs;
	NvAPI_GPU_GetUsages_t       NvAPI_GPU_GetUsages;
	NvAPI_PhysicalFromLogical   NvAPI_GPU_PhysicalFromLogical;
	NvAPI_D3D11_SetDepthBoundsTest_t NvAPI_D3D11_SetDepthBoundsTest;

	int*	gpuHandlesPh[NVAPI_MAX_PHYSICAL_GPUS];
	int*	gpuHandlesLg[NVAPI_MAX_LOGICAL_GPUS];
	u32		gpuUsages[NVAPI_MAX_USAGES_PER_GPU];
	ULONG	AdapterID;
	u64		AdapterFinal;

	HMODULE hNvAPIDLL;

private:
	void	InitDeviceInfo();
	void	MakeGPUCount();

public:
	CNvReader();
	~CNvReader();

	virtual void	Initialize();
	virtual u32		GetPercentActive();
	virtual u32		GetGPUCount();

	virtual CNvReader* GetNV() override { return this; }
	virtual bool SetDepthBounds(bool, float zMin, float zMax) override;

public:
	static bool bSupport;
};
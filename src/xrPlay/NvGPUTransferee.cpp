#include "../xrEngine/stdafx.h"
#include "NvGPUTransferee.h"

extern "C"
{
	// https://docs.nvidia.com/gameworks/content/technologies/desktop/optimus.htm
	__declspec(dllexport) DWORD NvOptimusEnablement = 0x00000001; // NVIDIA Optimus
}

bool CNvReader::bSupport = false;

CNvReader::CNvReader() : 
	AdapterID(0),
	hNvAPIDLL(nullptr)
{}

CNvReader::~CNvReader()
{
	if (hNvAPIDLL != nullptr)
	{
		FreeModule(hNvAPIDLL);
	}
}

void CNvReader::Initialize()
{
	if (bSupport) return;
	hNvAPIDLL = LoadLibraryA("nvapi64.dll");
	if (hNvAPIDLL != nullptr)
	{
		NvAPI_QueryInterface = (NvAPI_QueryInterface_t)GetProcAddress(hNvAPIDLL, "nvapi_QueryInterface");
		if (NvAPI_QueryInterface == nullptr)
		{
			FreeModule(hNvAPIDLL);
			hNvAPIDLL = nullptr;
			Msg("! Found nvapi64.dll, but DLL missing \"nvapi_QueryInterface\"");
			return;
		}

		auto TryToInitializeFunctionLambda = [this](void** pFuncPtr, u32 FuncId) -> bool
		{
			*pFuncPtr = (*NvAPI_QueryInterface)(FuncId);
			if (*pFuncPtr == nullptr)
			{
				FreeModule(hNvAPIDLL);
				hNvAPIDLL = nullptr;
				Msg("! Found nvapi64.dll, but DLL missing Func ID \"%u\"", FuncId);
				return false;
			}

			return true;
		};

		if (!TryToInitializeFunctionLambda((void**)&NvAPI_Initialize, 0x0150E828))
		{ return; }
		if (!TryToInitializeFunctionLambda((void**)&NvAPI_EnumPhysicalGPUs, 0xE5AC921F))
		{ return; }
		if (!TryToInitializeFunctionLambda((void**)&NvAPI_EnumLogicalGPUs, 0x48B3EA59))
		{ return; }
		if (!TryToInitializeFunctionLambda((void**)&NvAPI_GPU_GetUsages, 0x189A1FDF))
		{ return; }
		if (!TryToInitializeFunctionLambda((void**)&NvAPI_GPU_PhysicalFromLogical, 0x0AEA3FA32))
		{ return; }

		bSupport = true;
		InitDeviceInfo();
	}
	else
	{
		Msg("! Can't load nvapi64.dll");
	}
}

void CNvReader::InitDeviceInfo()
{
	(*NvAPI_Initialize)();

	// gpuUsages[0] must be this value, otherwise NvAPI_GPU_GetUsages won't work
	gpuUsages[0] = (NVAPI_MAX_USAGES_PER_GPU * 4) | 0x10000;

	(*NvAPI_EnumPhysicalGPUs)(gpuHandlesPh, &AdapterID);

	MakeGPUCount();
}

void CNvReader::MakeGPUCount()
{
	NvU32 logicalGPUCount;
	NvAPI_EnumLogicalGPUs(gpuHandlesLg, &logicalGPUCount);

	for (NvU32 i = 0; i < logicalGPUCount; ++i)
	{
		NvAPI_GPU_PhysicalFromLogical(gpuHandlesLg[i], gpuHandlesPh, &AdapterID);
		AdapterFinal = std::max(AdapterFinal, (u64)AdapterID);
	}

	if (AdapterFinal > 1)
	{
		Msg("[MSG] NVidia MGPU: %d-Way SLI detected.", AdapterFinal);
	}
}

u32 CNvReader::GetPercentActive()
{
	(*NvAPI_GPU_GetUsages)(gpuHandlesPh[0], gpuUsages);
	int usage = gpuUsages[3];
	return (u32)usage;
}

u32 CNvReader::GetGPUCount()
{
	return u32(AdapterFinal ? AdapterFinal : 1);
}

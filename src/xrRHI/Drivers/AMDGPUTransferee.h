#pragma once
// AMD (AGS and ADL) detection system
#include "../RHI.h"
#include <amd_adl\adl_sdk.h>
#include <amd_ags.h>

class CAMDReader :
	public IRHIGPU
{
	// ADL 1.x
	using ADL_MAIN_CONTROL_CREATE = int(*)(ADL_MAIN_MALLOC_CALLBACK, int);
	using ADL_ADAPTER_ACTIVE_GET = int(*)(int, int*);
	using ADL_OVERDRIVE5_TEMPERATURE_GET = int(*)(int, int, ADLTemperature*);
	using ADL_ADAPTER_NUMBEROFADAPTERS_GET = int(*)(int*);
	using ADL_ADAPTER_ADAPTERINFO_GET = int(*)(LPAdapterInfo, int);
	using ADL_OVERDRIVE5_CURRENTACTIVITY_GET = int(*)(int, ADLPMActivity*);
	using ADL_MAIN_CONTROL_DESTROY = int(*)();

	// ADL 2.x
	using ADL2_MAIN_CONTROL_CREATE = int(*)(ADL_MAIN_MALLOC_CALLBACK, int, ADL_CONTEXT_HANDLE*);
	using ADL2_MAIN_CONTROL_DESTROY = int(*)(ADL_CONTEXT_HANDLE);
	using ADL2_OVERDRIVE5_CURRENTACTIVITY_GET = int(*)(ADL_CONTEXT_HANDLE, int, ADLPMActivity*);

	// AGS Core
	using AGS_GPU_COUNT_GET = AGSReturnCode(*)(AGSContext*, int*);
	using AGS_DEINIT = AGSReturnCode(*)(AGSContext*);
	using AGS_INITIALIZE = AGSReturnCode(*)(AGSContext**, const AGSConfiguration*, AGSGPUInfo*);

	// AGS DX11 Extensions (5.2–5.3)
	using AGS_DX11EXT = AGSReturnCode(*)(AGSContext*, const AGSDX11DeviceCreationParams*, const AGSDX11ExtensionParams*, AGSDX11ReturnedParams*);
	using AGS_DX11EXTDestroy = AGSReturnCode(*)(AGSContext*, ID3D11Device*, unsigned int*, ID3D11DeviceContext*, unsigned int*);

	// AGS DX12 Extensions
	//using AGS_DX12EXT = AGSReturnCode(*)(AGSContext*, const AGSDX12DeviceCreationParams*, const AGSDX12ExtensionParams*, AGSDX12ReturnedParams*);
	//using AGS_DX12EXTDestroy = AGSReturnCode(*)(AGSContext*, ID3D12Device*, unsigned int*, ID3D12DeviceContext*, unsigned int*);

	using AGSDriverExtensionsDX11_SetDepthBounds_t = AGSReturnCode(*)(AGSContext* context, ID3D11DeviceContext* dxContext, bool enabled, float minDepth, float maxDepth);

private:
	// Memory allocation function
	static void* __stdcall MemoryAllocator(int iSize)
	{
		return xr_malloc(iSize);
	}

private:

	int				AdapterID;
	int				AdapterADLInfo;
	int				AdapterAGSInfo;
	ADLPMActivity	activity;
	HMODULE		    hAMDMain;
	HMODULE		    hAMDAGS;
	AGSContext*		Context;

	ADL_CONTEXT_HANDLE hADLContext;
	AGSDX11ReturnedParams ReturnedParams;

	bool bInitialized = false;

private:
	void InitDeviceInfo	();
	void MakeGPUCount	();

	const char* AGSGetErrorString(AGSReturnCode code);

public:

	ADL_MAIN_CONTROL_CREATE					Main_Control_Create;
	ADL_MAIN_CONTROL_DESTROY				Main_Control_Destroy;

	ADL_ADAPTER_ADAPTERINFO_GET				ADL_Adapter_AdapterInfo_Get;
	ADL_ADAPTER_NUMBEROFADAPTERS_GET		GetAdapter_NumberOfAdapters;
	ADL_ADAPTER_ACTIVE_GET					GetAdapter_Active;
	ADL_OVERDRIVE5_CURRENTACTIVITY_GET		GetOverdrive5_CurrentActivity;
	ADL_OVERDRIVE5_TEMPERATURE_GET			GetTemperatureGPU;

	// ADL 2
	ADL2_MAIN_CONTROL_CREATE ADL2_Main_Control_Create;
	ADL2_MAIN_CONTROL_DESTROY ADL2_Main_Control_Destroy;
	ADL2_OVERDRIVE5_CURRENTACTIVITY_GET ADL2_CurrentActivityGet;

	AGS_INITIALIZE							AGSInitializeProc;
	AGS_DEINIT								AGSDeinit;
	AGS_DX11EXT								GetAGSCrossfireGPUCountExt;
	AGS_GPU_COUNT_GET						GetAGSCrossfireGPUCount;
	AGS_DX11EXTDestroy						AGSCrossfireGPUExtDestroy;
	//AGS_DX12EXT								AGSDX12EXTProc;
	//AGS_DX12EXTDestroy						AGSDX12EXTDestroyProc;
	AGSDriverExtensionsDX11_SetDepthBounds_t AGSDX11_SetDepthBounds;

public:
			CAMDReader		();
			~CAMDReader		();

	bool	IsInitialized() const;
	u32		GetTemperature();

	virtual void	Initialize();
	virtual void	Destroy();

	virtual u32		GetPercentActive();
	virtual u32		GetGPUCount		();

	AGSContext* GetContext() const;

	u32 GetDX11Device(void** pDevice, void** pImmediateContext, void** pSwapChain);
	virtual CAMDReader* GetAMD() override { return this; }
	virtual bool SetDepthBounds(void* RContext, bool, float zMin, float zMax) override;

public:
	static bool bAMDSupportADL;
	bool bGPUCoreDetected = false;
	bool bGPUDriverNotActual = false;
};
//////////////////////////////////////////////////////////////
// Desc   : AMD (AGS & ADL) detection system
// Author : ForserX
//////////////////////////////////////////////////////////////
// Oxygen Engine 2016 - 2020 (c) 
//////////////////////////////////////////////////////////////

#pragma once
#include "../xrEngine/ICore_GPU.h"
#include <amd_adl\adl_sdk.h>
#include <amd_ags\amd_ags.h>

class CAMDReader :
	public ICore_GPU
{
	typedef int(*ADL_MAIN_CONTROL_CREATE)(ADL_MAIN_MALLOC_CALLBACK, int);
	typedef int(*ADL_ADAPTER_ACTIVE_GET) (int, int*);
	typedef int(*ADL_OVERDRIVE5_TEMPERATURE_GET) (int iAdapterIndex, int iThermalControllerIndex, ADLTemperature* lpTemperature);
	typedef int(*ADL_ADAPTER_NUMBEROFADAPTERS_GET) (int*);
	typedef int(*ADL_ADAPTER_ADAPTERINFO_GET) (LPAdapterInfo, int);
	typedef int(*ADL_OVERDRIVE5_CURRENTACTIVITY_GET) (int iAdapterIndex, ADLPMActivity *lpActivity);
	typedef int(*ADL_MAIN_CONTROL_DESTROY)();

	typedef AGSReturnCode(*AGS_GPU_COUNT_GET)(AGSContext* context, int* numGPUs);
	typedef AGSReturnCode(*AGS_DEINIT)(AGSContext* context);
	typedef AGSReturnCode(*AGS_INITIALIZE)(AGSContext** context, const AGSConfiguration* config, AGSGPUInfo* gpuInfo);
	
	// ADL 2
	typedef int(*ADL2_MAIN_CONTROL_CREATE)(ADL_MAIN_MALLOC_CALLBACK, int, ADL_CONTEXT_HANDLE*);
	typedef int(*ADL2_MAIN_CONTROL_DESTROY)(ADL_CONTEXT_HANDLE);
	typedef int(*ADL2_OVERDRIVE5_CURRENTACTIVITY_GET)(ADL_CONTEXT_HANDLE, int, ADLPMActivity*);

	// AGS: 5.2-5.3
	typedef AGSReturnCode(*AGS_DX11EXT)(AGSContext* context, const AGSDX11DeviceCreationParams* creationParams, const AGSDX11ExtensionParams* extensionParams, AGSDX11ReturnedParams* returnedParams);
	typedef AGSReturnCode(*AGS_DX11EXTDestroy)(AGSContext* context, ID3D11Device* device, unsigned int* deviceReferences, ID3D11DeviceContext* immediateContext, unsigned int* immediateContextReferences);
	typedef AGSReturnCode(*AGS_DX12EXT)(AGSContext* context, const AGSDX12DeviceCreationParams* creationParams, const AGSDX12ExtensionParams* extensionParams, AGSDX12ReturnedParams* returnedParams);
	typedef AGSReturnCode(*AGS_DX12EXTDestroy)(AGSContext* context, ID3D12Device* device, unsigned int* deviceReferences);

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
	void	InitDeviceInfo	();
	void	MakeGPUCount	();

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
	AGS_DX12EXT								AGSDX12EXTProc;
	AGS_DX12EXTDestroy						AGSDX12EXTDestroyProc;

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

	virtual void GetDX11Device
	(
		ID3D11Device** pDevice,
		ID3D11DeviceContext** pImmediateContext,
		IDXGISwapChain** pSwapChain,
		D3D_FEATURE_LEVEL& FeatureLevel
	);

public:
	static bool bAMDSupportADL;
	bool bGPUCoreDetected = false;
	bool bGPUDriverNotActual = false;
};
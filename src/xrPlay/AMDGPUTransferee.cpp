#include "../xrEngine/stdafx.h"
#include "AMDGPUTransferee.h"

extern "C"
{
	// https://gpuopen.com/amdpowerxpressrequesthighperformance/
	__declspec(dllexport) int AmdPowerXpressRequestHighPerformance = 1; // PowerXpress or Hybrid Graphics
}

bool CAMDReader::bAMDSupportADL = false;

CAMDReader::CAMDReader() : 
	activity({ 0 }), 
	AdapterID(-1),
	AdapterAGSInfo(0),
	hAMDMain(nullptr),
	hAMDAGS(nullptr),
	hADLContext(nullptr)
{}

CAMDReader::~CAMDReader()
{
	if (bAMDSupportADL)
	{
		if (hAMDMain != nullptr)
		{
			Main_Control_Destroy();

			if (ADL2_Main_Control_Destroy)
				ADL2_Main_Control_Destroy(hADLContext);

			FreeLibrary(hAMDMain);
			hAMDMain = nullptr;
		}

		if (hAMDAGS != nullptr)
		{
			AGSDeinit(Context);
			FreeModule(hAMDAGS);
			hAMDAGS = nullptr;
		}
	}
}

void CAMDReader::Initialize()
{
	if (bInitialized) return;
	hAMDMain = LoadLibraryA("atiadlxx.dll");

	if (hAMDMain != nullptr)
	{
		auto TryInitializeAMDFunctionLambda = [this](void** pFunc, const char* FuncName) -> bool
		{
			*pFunc = GetProcAddress(hAMDMain, FuncName);
			if (*pFunc == nullptr)
			{
				FreeModule(hAMDMain);
				hAMDMain = nullptr;
				Msg("! atiadlxx.dll doesn't have function \"%s\"", FuncName);
				return false;
			}

			return true;
		};

		if (!TryInitializeAMDFunctionLambda((void**)& Main_Control_Create, "ADL_Main_Control_Create"))
		{
			return;
		}
		if (!TryInitializeAMDFunctionLambda((void**)&GetAdapter_Active, "ADL_Adapter_Active_Get"))
		{
			return;
		}
		if (!TryInitializeAMDFunctionLambda((void**)&GetAdapter_NumberOfAdapters, "ADL_Adapter_NumberOfAdapters_Get"))
		{
			return;
		}
		if (!TryInitializeAMDFunctionLambda((void**)&ADL_Adapter_AdapterInfo_Get, "ADL_Adapter_AdapterInfo_Get"))
		{
			return;
		}
		if (!TryInitializeAMDFunctionLambda((void**)&GetOverdrive5_CurrentActivity, "ADL_Overdrive5_CurrentActivity_Get"))
		{
			return;
		}
		if (!TryInitializeAMDFunctionLambda((void**)&Main_Control_Destroy, "ADL_Main_Control_Destroy"))
		{
			return;
		}
		if (!TryInitializeAMDFunctionLambda((void**)&GetTemperatureGPU, "ADL_Overdrive5_Temperature_Get"))
		{
			return;
		}

		ADL2_Main_Control_Create = (ADL2_MAIN_CONTROL_CREATE)GetProcAddress(hAMDMain, "ADL2_Main_Control_Create");;
		ADL2_Main_Control_Destroy = (ADL2_MAIN_CONTROL_DESTROY)GetProcAddress(hAMDMain, "ADL2_Main_Control_Destroy");
		ADL2_CurrentActivityGet = (ADL2_OVERDRIVE5_CURRENTACTIVITY_GET)GetProcAddress(hAMDMain, "ADL2_Overdrive5_CurrentActivity_Get");

		bAMDSupportADL = true;

		Main_Control_Create(MemoryAllocator, 1);

		if (ADL2_Main_Control_Create)
		{
			int ResCode = ADL2_Main_Control_Create(MemoryAllocator, 1, &hADLContext);

			if (ResCode == ADL_ERR_INVALID_ADL_IDX)
				Msg("* AMD ADL Error: Need update GPU driver!");
		}

		InitDeviceInfo();

		activity.iSize = sizeof(ADLPMActivity);
		IsAMD = true;
	}

#ifdef _M_X64
	hAMDAGS = LoadLibraryA("amd_ags_x64.dll");
#else
	hAMDAGS = LoadLibraryA("amd_ags_x86.dll");
#endif

	if (hAMDAGS != nullptr)
	{
		auto TryInitializeAMDAGSFunctionLambda = [this](void** pFunc, const char* FuncName) -> bool
		{
			*pFunc = GetProcAddress(hAMDAGS, FuncName);
			if (*pFunc == nullptr)
			{
				FreeModule(hAMDAGS);
				hAMDAGS = nullptr;
				Msg("! amd_ags_x64.dll doesn't have function \"%s\"", FuncName);
				return false;
			}

			return true;
		};

		if (!TryInitializeAMDAGSFunctionLambda((void**)&AGSDeinit, "agsDeInit"))
		{
			return;
		}

		if (!TryInitializeAMDAGSFunctionLambda((void**)&AGSInitializeProc, "agsInit"))
		{
			return;
		}

		GetAGSCrossfireGPUCount = (AGS_GPU_COUNT_GET)GetProcAddress(hAMDAGS, "agsGetCrossfireGPUCount");
		if (GetAGSCrossfireGPUCount == nullptr)
		{
			if (!TryInitializeAMDAGSFunctionLambda((void**)&GetAGSCrossfireGPUCountExt, "agsDriverExtensionsDX11_CreateDevice"))
			{
				return;
			}
			if (!TryInitializeAMDAGSFunctionLambda((void**)&AGSCrossfireGPUExtDestroy, "agsDriverExtensionsDX11_DestroyDevice"))
			{
				return;
			}
		}

		TryInitializeAMDAGSFunctionLambda((void**) & AGSDX12EXTProc, "agsDriverExtensionsDX12_CreateDevice");
		TryInitializeAMDAGSFunctionLambda((void**) &AGSDX12EXTDestroyProc, "agsDriverExtensionsDX12_DestroyDevice");

		MakeGPUCount();
	}

	bInitialized = true;
}

void CAMDReader::Destroy()
{
	if (hAMDAGS != nullptr)
	{
		u32 DeviceRefs = 0;
		u32 DeviceImmediateRefs = 0;
		AGSCrossfireGPUExtDestroy(Context, ReturnedParams.pDevice, &DeviceRefs, ReturnedParams.pImmediateContext, &DeviceImmediateRefs);
	}
}

bool CAMDReader::IsInitialized() const
{
	return bInitialized;
}

void CAMDReader::InitDeviceInfo()
{
	GetAdapter_NumberOfAdapters(&AdapterADLInfo);

	xr_shared_ptr< AdapterInfo > AdapterInfos;
	AdapterInfos.reset(new AdapterInfo[AdapterADLInfo]);

	// Get the AdapterInfo structure for all adapters in the system
	ADL_Adapter_AdapterInfo_Get(AdapterInfos.get(), sizeof(AdapterInfo) * AdapterADLInfo);

	for (u32 i = 0; i < (u32)AdapterADLInfo; i++)
	{
		int bAdapterActive = 0;
		AdapterInfo& adapterInfo = AdapterInfos.get()[i];
		GetAdapter_Active(adapterInfo.iAdapterIndex, &bAdapterActive);

		if (bAdapterActive)
		{
			// Find normal GPU...
			if (strstr(adapterInfo.strAdapterName, "AMD Radeon(TM) Graphics"))
			{
				bGPUCoreDetected = true;
			}

			AdapterID = adapterInfo.iAdapterIndex;
			break;
		}
	}
}
#include <d3d11.h>

void CAMDReader::MakeGPUCount()
{
	if (hAMDAGS == nullptr)
	{
		return;
	}

	AGSGPUInfo gpuInfo = {};
	AGSConfiguration* config = nullptr;
	//AGSReturnCode status = AGSCrossfireInit(&ags, config, &gpuInfo);
	AGSReturnCode status = AGSInitializeProc(&Context, config, &gpuInfo);

	if (status != AGS_SUCCESS)
	{
		Msg("! AGS: Initialization failed (%d)", status);
		return;
	}

	if (GetAGSCrossfireGPUCount)
	{
		// FX: Old style for Win7 and lazy users
		// But, it's just beautiful 
		status = GetAGSCrossfireGPUCount(Context, &AdapterAGSInfo);
		AdapterAGSInfo = AdapterAGSInfo ? AdapterAGSInfo : AdapterADLInfo / 2;
		Msg("[AGS] Used old ags driver...");
	}
	else
	{
		AGSDX11DeviceCreationParams creationParams;
		RtlZeroMemory(&creationParams, sizeof(AGSDX11DeviceCreationParams));
		creationParams.SDKVersion = 7; // Skip debug output errors. crossfireGPUCount need only
		creationParams.FeatureLevels = 45312; // 11.1

		AGSDX11ExtensionParams extensionParams = {};
		// FX: Enable AFR without requiring a driver profile
		extensionParams.crossfireMode = AGS_CROSSFIRE_MODE_EXPLICIT_AFR;
		extensionParams.uavSlot = 7;

		GetAGSCrossfireGPUCountExt(Context, &creationParams, &extensionParams, &ReturnedParams);
		AdapterAGSInfo = ReturnedParams.crossfireGPUCount ? ReturnedParams.crossfireGPUCount : AdapterADLInfo / 2;

		u32 DeviceRefs = 0;
		u32 DeviceImmediateRefs = 0;
		AGSCrossfireGPUExtDestroy(Context, ReturnedParams.pDevice, &DeviceRefs, ReturnedParams.pImmediateContext, &DeviceImmediateRefs);
	}

	if (status != AGS_SUCCESS) Msg("[AGS] Error! Unable to get CrossFire GPU count (%d)", status);
	else					   Msg("[AGS] CrossFire GPU count: %d", AdapterAGSInfo);
}

u32 CAMDReader::GetPercentActive()
{
	if (bGPUDriverNotActual)
		return 0;

	if (ADL2_CurrentActivityGet)
	{
		int ResCode = ADL2_CurrentActivityGet(hADLContext, AdapterID, &activity);

		if (ResCode == ADL_ERR_INVALID_ADL_IDX)
		{
			Msg("! AMD ADL Error: Need update GPU driver!");
			bGPUDriverNotActual = true;
		}
	}
	else
	{
		GetOverdrive5_CurrentActivity(AdapterID, &activity);
	}

	return activity.iActivityPercent;
}

u32 CAMDReader::GetTemperature()
{
	ADLTemperature adlTemperature = { 0 };
	adlTemperature.iSize = sizeof(ADLTemperature);
	GetTemperatureGPU(AdapterID, 0, &adlTemperature);
	return u32(adlTemperature.iTemperature);
}

u32 CAMDReader::GetGPUCount()
{
	u32 uCount = hAMDAGS ? u32(AdapterAGSInfo) : u32(AdapterADLInfo / 2);
	return uCount ? uCount : 1;
}

AGSContext* CAMDReader::GetContext() const
{
	return Context;
}

void CAMDReader::GetDX11Device(ID3D11Device** pDevice, ID3D11DeviceContext** pImmediateContext, IDXGISwapChain** pSwapChain, D3D_FEATURE_LEVEL& FeatureLevel)
{
	D3D_FEATURE_LEVEL featureLevels[] =
	{
		D3D_FEATURE_LEVEL_11_1,
		D3D_FEATURE_LEVEL_11_0,
		D3D_FEATURE_LEVEL_10_1,
		D3D_FEATURE_LEVEL_10_0
	};

	HWND hwnd = g_AppInfo.GetHWND();

	DXGI_SWAP_CHAIN_DESC sd = {};
	sd.BufferDesc.Width = Device.TargetHeight;
	sd.BufferDesc.Height = Device.TargetHeight;
	sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	sd.BufferCount = 1;
	sd.SampleDesc.Count = 1;
	sd.SampleDesc.Quality = 0;
	sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
	sd.OutputWindow = hwnd;
	sd.Windowed = !psDeviceFlags.is(rsFullscreen);

	sd.BufferDesc.RefreshRate.Numerator = 0;
	sd.BufferDesc.RefreshRate.Denominator = 0;

	//	Additional set up
	sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;

	AGSDX11DeviceCreationParams creationParams
	{
		nullptr,
		D3D_DRIVER_TYPE_HARDWARE,
		nullptr,
		0,
		featureLevels,
		u32(std::size(featureLevels)),
		D3D11_SDK_VERSION,
		&sd
	};

	AGSDX11ExtensionParams extensionParams = {};
	// FX: Enable AFR without requiring a driver profile
	extensionParams.crossfireMode = AGS_CROSSFIRE_MODE_EXPLICIT_AFR;
	extensionParams.uavSlot = 7;

	GetAGSCrossfireGPUCountExt(Context, &creationParams, &extensionParams, &ReturnedParams);

	*pDevice = ReturnedParams.pDevice;
	*pImmediateContext = ReturnedParams.pImmediateContext;
	*pSwapChain = ReturnedParams.pSwapChain;
	FeatureLevel = ReturnedParams.FeatureLevel;
}
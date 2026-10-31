#include "stdafx.h"

#include "DLSSWrapper.h"
#include "../../../xrCore/Platform/OSVersion.h"

DLSSWrapper g_DLSSWrapper;

extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

u32 DLSSWrapper::GetOptimalPresetForScale(float scale)
{
	if (ps_render_scale_preset == 5)
	{
		if (scale >= 0.9f)
		{
			return 0;
		}
		else if (scale >= 0.7f)
		{
			return 1;
		}
		else if (scale >= 0.6f)
		{
			return 2;
		}
		else if (scale >= 0.5f)
		{
			return 3;
		}
		else
		{
			return 4;
		}
	}
	
	return ps_render_scale_preset;
}

// DLSS on D3D11 reads its inputs from CUDA through interop. On some launches it saw a different image
// in the color render target than D3D11 did (black sky, dark scene), and the output stayed black until
// vid_restart reallocated the targets. Feed it a plain texture that is only written by a copy instead
ID3D11Texture2D* DLSSWrapper::GetColorInput(ID3D11Texture2D* Source)
{
	D3D11_TEXTURE2D_DESC SourceDesc;
	Source->GetDesc(&SourceDesc);

	if (ColorInput != nullptr)
	{
		D3D11_TEXTURE2D_DESC Desc;
		ColorInput->GetDesc(&Desc);

		if (Desc.Width != SourceDesc.Width || Desc.Height != SourceDesc.Height || Desc.Format != SourceDesc.Format)
		{
			_RELEASE(ColorInput);
		}
	}

	if (ColorInput == nullptr)
	{
		D3D11_TEXTURE2D_DESC Desc = SourceDesc;
		Desc.Usage = D3D11_USAGE_DEFAULT;
		Desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
		Desc.CPUAccessFlags = 0;
		Desc.MiscFlags = 0;

		if (FAILED(RDevice->CreateTexture2D(&Desc, nullptr, &ColorInput)))
		{
			return Source;
		}
	}

	RContext->CopyResource(ColorInput, Source);
	return ColorInput;
}

void DLSSWrapper::Create()
{
	if (Platform::IsWindows7OrOlder())
	{
		return;
	}

	Destroy();

	if (RFeatureLevel < D3D_FEATURE_LEVEL_11_1)
	{
		return;
	}

#ifdef IXR_X64
	NVSDK_NGX_Result Result;

	if (!DLSSInited)
	{
		Result = NVSDK_NGX_D3D11_Init(1602, L"", RDevice);

		if (Result != NVSDK_NGX_Result_Success)
		{
			return;
		}

		DLSSInited = true;
	}

	Result = NVSDK_NGX_D3D11_GetCapabilityParameters(&NgxParameters);

	if (Result != NVSDK_NGX_Result_Success)
	{
		return;
	}

	uint32_t NeedsUpdatedDriver = 1;
	Result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_NeedsUpdatedDriver, &NeedsUpdatedDriver);

	if (NeedsUpdatedDriver)
	{
		Msg("! PLEASE UPDATE YOUR DRIVER");
	}

	uint32_t DlssAvailable = 0;
	Result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_Available, &DlssAvailable);

	if (!DlssAvailable)
	{
		NVSDK_NGX_D3D11_DestroyParameters(NgxParameters);
		NgxParameters = nullptr;
		return;
	}

	Created = true;
#endif
}

bool DLSSWrapper::GetRenderScale(float& RenderScale)
{
	if (!Created || !NgxParameters)
	{
		Msg("! GetRenderScale DLSSWrapper not valid. Fallback!");
		return false;
	}

	u32 PresetID = GetOptimalPresetForScale(ps_render_scale);

	NVSDK_NGX_PerfQuality_Value PerfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;

	switch (PresetID)
	{
		case 4:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_UltraPerformance;
			break;
		}
		case 3:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxPerf;
			break;
		}
		case 2:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_Balanced;
			break;
		}
		case 1:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxQuality;
			break;
		}
		default:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
			break;
		}
	}

	u32 RenderW = 0, RenderH = 0, MaxW = 0, MinW = 0, MaxH = 0, MinH = 0; float Sharp = 0;
	NVSDK_NGX_Result Result = NGX_DLSS_GET_OPTIMAL_SETTINGS(NgxParameters, Device.TargetWidth, Device.TargetHeight, PerfQualityValue, &RenderW, &RenderH, &MaxW, &MaxH, &MinW, &MinH, &Sharp);

	if (Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_DLSS_GET_OPTIMAL_SETTINGS not valid. Fallback!");
		return false;
	}

	Msg("* DLSS Target - %dx%d, Min - %dx%d, Max - %dx%d, Sharp - %f", RenderW, RenderH, MaxW, MaxH, MinW, MinH, Sharp);
	RenderScale = float(RenderH) / float(Device.TargetHeight);

	return true;
}

void DLSSWrapper::Resize(const ContextParameters& Parameters)
{
	PROF_EVENT("DLSSWrapper::Resize");

	if (!Created)
	{
		return;
	}

#ifdef IXR_X64
	// Устанавливаем пресет для выбранного режима качества
	u32 PresetID = GetOptimalPresetForScale(ps_render_scale);

	NVSDK_NGX_PerfQuality_Value PerfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
	shared_str RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_DLAA;

	switch (PresetID)
	{
		case 4:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_UltraPerformance;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_UltraPerformance;
			break;
		}
		case 3:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxPerf;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Performance;
			break;
		}
		case 2:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_Balanced;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Balanced;
			break;
		}
		case 1:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxQuality;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Quality;
			break;
		}
		default:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_DLAA;
			break;
		}
	}

	NgxParameters->Set(*RenderPreset, static_cast<int>(NVSDK_NGX_DLSS_Hint_Render_Preset_K));

	Msg("* Resize DLSSWrapper Render Preset [%s]", *RenderPreset);

	NVSDK_NGX_DLSS_Create_Params DLSSCreateParams = {};

	DLSSCreateParams.Feature.InWidth = Parameters.renderSize.x;
	DLSSCreateParams.Feature.InHeight = Parameters.renderSize.y;

	DLSSCreateParams.Feature.InTargetWidth = Parameters.displaySize.x;
	DLSSCreateParams.Feature.InTargetHeight = Parameters.displaySize.y;

	DLSSCreateParams.Feature.InPerfQualityValue = PerfQualityValue;
	DLSSCreateParams.InFeatureCreateFlags = 0;

	DLSSCreateParams.InFeatureCreateFlags |= NVSDK_NGX_DLSS_Feature_Flags_IsHDR;
	DLSSCreateParams.InFeatureCreateFlags |= NVSDK_NGX_DLSS_Feature_Flags_MVLowRes;
	DLSSCreateParams.InFeatureCreateFlags |= NVSDK_NGX_DLSS_Feature_Flags_AutoExposure;

	if (Handle != nullptr)
	{
		NVSDK_NGX_D3D11_ReleaseFeature(Handle);
		Handle = nullptr;
	}

	NVSDK_NGX_Result Result = NGX_D3D11_CREATE_DLSS_EXT(RContext, &Handle, NgxParameters, &DLSSCreateParams);

	if (Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_D3D11_CREATE_DLSS_EXT not valid. Need use FSR");
		Created = false;
		return;
	}

	HistoryValid = false;
#endif
}

void DLSSWrapper::Destroy() 
{
#ifdef IXR_X64
	if (Handle != nullptr)
	{
		NVSDK_NGX_D3D11_ReleaseFeature(Handle);
		Handle = nullptr;
	}

	if (NgxParameters != nullptr)
	{
		NVSDK_NGX_D3D11_DestroyParameters(NgxParameters);
		NgxParameters = nullptr;
	}

	_RELEASE(ColorInput);

	if (DLSSInited)
	{
		NVSDK_NGX_D3D11_Shutdown1(nullptr);
		DLSSInited = false;
	}

	Created = false;	
#endif
}

bool DLSSWrapper::Draw(const DrawParameters& params)
{
	if(!Created)
	{
		Msg("! DLSSWrapper not created. Need use FSR");
		return false;
	}

#ifdef IXR_X64
	NVSDK_NGX_D3D11_DLSS_Eval_Params DLSSEvalParams = {};

	DLSSEvalParams.Feature.pInColor = GetColorInput(params.unresolvedColorResource);
	DLSSEvalParams.Feature.pInOutput = params.resolvedColorResource;
	DLSSEvalParams.Feature.InSharpness = params.sharpness;

	DLSSEvalParams.pInDepth = params.depthbufferResource;
	DLSSEvalParams.pInMotionVectors = params.motionvectorResource;

	DLSSEvalParams.InRenderSubrectDimensions.Width = params.renderWidth;
	DLSSEvalParams.InRenderSubrectDimensions.Height = params.renderHeight;

	DLSSEvalParams.InJitterOffsetX = params.cameraJitterX;
	DLSSEvalParams.InJitterOffsetY = params.cameraJitterY;

	// The history is stale for a new feature or after frames without evaluation (loading, main menu, reset).
	// Without a reset, a single invalid frame stays in the history and the auto exposure forever
	const bool HistoryStale = !HistoryValid || LastDrawFrame + 1 != Device.dwFrame;
	DLSSEvalParams.InReset = params.cameraReset || HistoryStale;

	DLSSEvalParams.InMVScaleX = -(float)params.renderWidth * 0.5f;
	DLSSEvalParams.InMVScaleY = (float)params.renderHeight * 0.5f;

	DLSSEvalParams.pInTransparencyMask = params.transparencyAndCompositionResource;
	DLSSEvalParams.InFrameTimeDeltaInMsec = params.frameTimeDelta;

	NVSDK_NGX_Result Result = NGX_D3D11_EVALUATE_DLSS_EXT(RContext, Handle, NgxParameters, &DLSSEvalParams);

	if(Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_D3D11_EVALUATE_DLSS_EXT not valid. Need use FSR");
		HistoryValid = false;
		return false;
	}

	HistoryValid = true;
	LastDrawFrame = Device.dwFrame;
#endif

	return true;
}

DLSSWrapper::~DLSSWrapper()
{
	Destroy();
}

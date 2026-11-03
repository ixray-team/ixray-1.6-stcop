#include "stdafx.h"

#include "DLSSWrapper.h"

DLSSWrapper g_DLSSWrapper;

extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;
extern ENGINE_API u32 ps_r_dlss_model;

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

void DLSSWrapper::Create()
{
	Destroy();

	if (RFeatureLevel < D3D_FEATURE_LEVEL_11_1)
	{
		return;
	}

#ifdef IXR_X64
	NVSDK_NGX_Result Result;

	if (!DLSSInited)
	{
		string_path LogsPath = {};
		FS.update_path(LogsPath, "$logs$", "");

		wchar_t LogsPathW[_MAX_PATH] = {};
		MultiByteToWideChar(CP_ACP, 0, LogsPath, -1, LogsPathW, _MAX_PATH);

		Result = NVSDK_NGX_D3D11_Init_with_ProjectID(
			"5cd873f3-9331-4d2f-8756-0d8dbd823fbf", NVSDK_NGX_ENGINE_TYPE_CUSTOM, "1.6",
			LogsPathW, RDevice);

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

	uint32_t NeedsUpdatedDriver = 0;
	Result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_NeedsUpdatedDriver, &NeedsUpdatedDriver);

	if (NVSDK_NGX_SUCCEED(Result) && NeedsUpdatedDriver)
	{
		uint32_t MinDriverMajor = 0, MinDriverMinor = 0;
		NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_MinDriverVersionMajor, &MinDriverMajor);
		NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_MinDriverVersionMinor, &MinDriverMinor);
		Msg("! DLSS requires a newer NVIDIA driver (minimum %u.%u)", MinDriverMajor, MinDriverMinor);
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

	Msg("* DLSS Target - %ux%u, Min - %ux%u, Max - %ux%u", RenderW, RenderH, MinW, MinH, MaxW, MaxH);
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
	if (Handle != nullptr)
	{
		NVSDK_NGX_D3D11_ReleaseFeature(Handle);
		Handle = nullptr;
	}

	// Устанавливаем пресет для выбранного режима качества
	u32 PresetID = GetOptimalPresetForScale(ps_render_scale);

	NVSDK_NGX_PerfQuality_Value PerfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
	shared_str RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_DLAA;

	NVSDK_NGX_DLSS_Hint_Render_Preset PresetValue = NVSDK_NGX_DLSS_Hint_Render_Preset_K;

	switch (PresetID)
	{
		case 4:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_UltraPerformance;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_UltraPerformance;
			PresetValue = NVSDK_NGX_DLSS_Hint_Render_Preset_L;
			break;
		}
		case 3:
		{
			PerfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxPerf;
			RenderPreset = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Performance;
			PresetValue = NVSDK_NGX_DLSS_Hint_Render_Preset_M;
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

	switch (ps_r_dlss_model)
	{
		case 1:
		{
			PresetValue = NVSDK_NGX_DLSS_Hint_Render_Preset_E;
			break;
		}
		case 2:
		{
			PresetValue = NVSDK_NGX_DLSS_Hint_Render_Preset_K;
			break;
		}
		case 3:
		{
			PresetValue = PresetID == 4 ? NVSDK_NGX_DLSS_Hint_Render_Preset_L : NVSDK_NGX_DLSS_Hint_Render_Preset_M;
			break;
		}
	}

	NgxParameters->Set(*RenderPreset, static_cast<unsigned int>(PresetValue));

	Msg("* Resize DLSSWrapper Render Preset [%s] = %c", *RenderPreset, 'A' + int(PresetValue) - 1);

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

	NVSDK_NGX_Result Result = NGX_D3D11_CREATE_DLSS_EXT(RContext, &Handle, NgxParameters, &DLSSCreateParams);

	if (Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_D3D11_CREATE_DLSS_EXT not valid. Need use FSR");
		Created = false;
		return;
	}

	HistoryInvalid = true;
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

	DLSSEvalParams.Feature.pInColor = params.unresolvedColorResource;
	DLSSEvalParams.Feature.pInOutput = params.resolvedColorResource;

	DLSSEvalParams.pInDepth = params.depthbufferResource;
	DLSSEvalParams.pInMotionVectors = params.motionvectorResource;

	DLSSEvalParams.InRenderSubrectDimensions.Width = params.renderWidth;
	DLSSEvalParams.InRenderSubrectDimensions.Height = params.renderHeight;

	DLSSEvalParams.InJitterOffsetX = params.cameraJitterX;
	DLSSEvalParams.InJitterOffsetY = params.cameraJitterY;

	DLSSEvalParams.InReset = params.cameraReset || HistoryInvalid;
	HistoryInvalid = false;

	DLSSEvalParams.InMVScaleX = -(float)params.renderWidth * 0.5f;
	DLSSEvalParams.InMVScaleY = (float)params.renderHeight * 0.5f;

	DLSSEvalParams.pInTransparencyMask = params.transparencyAndCompositionResource;
	DLSSEvalParams.pInBiasCurrentColorMask = params.reactiveMapResource;
	DLSSEvalParams.InFrameTimeDeltaInMsec = params.frameTimeDelta;
	
	NVSDK_NGX_Result Result = NGX_D3D11_EVALUATE_DLSS_EXT(RContext, Handle, NgxParameters, &DLSSEvalParams);

	if(Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_D3D11_EVALUATE_DLSS_EXT not valid. Need use FSR");
		return false;
	}
#endif

	return true;
}

DLSSWrapper::~DLSSWrapper()
{
	Destroy();
}

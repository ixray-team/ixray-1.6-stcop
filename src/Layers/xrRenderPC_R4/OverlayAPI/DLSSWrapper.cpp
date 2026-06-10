#include "stdafx.h"

#include "DLSSWrapper.h"

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

void DLSSWrapper::Create()
{
	Destroy();

	if (RFeatureLevel < D3D_FEATURE_LEVEL_11_1)
	{
		return;
	}

#ifdef IXR_X64
	NVSDK_NGX_Result result;

	if (!DLSSInited)
	{
		result = NVSDK_NGX_D3D11_Init(1602, L"", RDevice);

		if (result != NVSDK_NGX_Result_Success)
		{
			return;
		}

		DLSSInited = true;
	}

	result = NVSDK_NGX_D3D11_GetCapabilityParameters(&NgxParameters);

	if (result != NVSDK_NGX_Result_Success)
	{
		return;
	}

	uint32_t needsUpdatedDriver = 1;
	result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_NeedsUpdatedDriver, &needsUpdatedDriver);

	if (needsUpdatedDriver)
	{
		Msg("! PLEASE UPDATE YOUR DRIVER");
	}

	uint32_t dlssAvailable = 0;
	result = NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_Available, &dlssAvailable);

	if (!dlssAvailable)
	{
		NVSDK_NGX_D3D11_DestroyParameters(NgxParameters);
		NgxParameters = nullptr;
		return;
	}

	m_created = true;
#endif
}

bool DLSSWrapper::GetRenderScale(float& RenderScale)
{
	if (!m_created || !NgxParameters)
	{
		Msg("! GetRenderScale DLSSWrapper not valid. Fallback!");
		return false;
	}

	u32 PresetID = GetOptimalPresetForScale(ps_render_scale);

	NVSDK_NGX_PerfQuality_Value perfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;

	switch (PresetID)
	{
		case 4:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_UltraPerformance;
			break;
		}
		case 3:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxPerf;
			break;
		}
		case 2:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_Balanced;
			break;
		}
		case 1:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxQuality;
			break;
		}
		default:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
			break;
		}
	}


	u32 RenderW = 0, RenderH = 0, MaxW = 0, MinW = 0, MaxH = 0, MinH = 0; float sharp = 0;
	NVSDK_NGX_Result result = NGX_DLSS_GET_OPTIMAL_SETTINGS(NgxParameters, Device.TargetWidth, Device.TargetHeight, perfQualityValue, &RenderW, &RenderH, &MaxW, &MaxH, &MinW, &MinH, &sharp);

	if (result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_DLSS_GET_OPTIMAL_SETTINGS not valid. Fallback!");
		return false;
	}

	Msg("* DLSS Target - %dx%d, Min - %dx%d, Max - %dx%d, Sharp - %f", RenderW, RenderH, MaxW, MaxH, MinW, MinH, sharp);
	RenderScale = float(RenderH) / float(Device.TargetHeight);

	return true;
}

void DLSSWrapper::Resize(const ContextParameters& Parameters)
{
	PROF_EVENT("DLSSWrapper::Create");

	if (!m_created)
	{
		return;
	}

#ifdef IXR_X64
	// Устанавливаем пресет для выбранного режима качества
	u32 PresetID = GetOptimalPresetForScale(ps_render_scale);

	NVSDK_NGX_PerfQuality_Value perfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
	shared_str presetParameter = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_DLAA;

	switch (PresetID)
	{
		case 4:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_UltraPerformance;
			presetParameter = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_UltraPerformance;
			break;
		}
		case 3:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxPerf;
			presetParameter = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Performance;
			break;
		}
		case 2:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_Balanced;
			presetParameter = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Balanced;
			break;
		}
		case 1:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_MaxQuality;
			presetParameter = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_Quality;
			break;
		}
		default:
		{
			perfQualityValue = NVSDK_NGX_PerfQuality_Value_DLAA;
			presetParameter = NVSDK_NGX_Parameter_DLSS_Hint_Render_Preset_DLAA;
			break;
		}
	}

	NgxParameters->Set(presetParameter.c_str(), static_cast<int>(NVSDK_NGX_DLSS_Hint_Render_Preset_F));

	int32_t flags = 0; // NVSDK_NGX_DLSS_Feature_Flags_DoSharpening
	flags |= NVSDK_NGX_DLSS_Feature_Flags_MVLowRes;
	flags |= NVSDK_NGX_DLSS_Feature_Flags_IsHDR;

	NVSDK_NGX_DLSS_Create_Params dlssCreateParams = {};

	dlssCreateParams.Feature.InWidth = Parameters.renderSize.x;
	dlssCreateParams.Feature.InHeight = Parameters.renderSize.y;

	dlssCreateParams.Feature.InTargetWidth = Parameters.displaySize.x;
	dlssCreateParams.Feature.InTargetHeight = Parameters.displaySize.y;

	dlssCreateParams.Feature.InPerfQualityValue = perfQualityValue;
	dlssCreateParams.InFeatureCreateFlags = flags;

	NVSDK_NGX_Result result = NGX_D3D11_CREATE_DLSS_EXT(RContext, &Handle, NgxParameters, &dlssCreateParams);

	if (result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_D3D11_CREATE_DLSS_EXT not valid. Need use FSR");
		m_created = false;
		return;
	}
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

	m_created = false;	
#endif
}

bool DLSSWrapper::Draw(const DrawParameters& params)
{
	if(!m_created)
	{
		Msg("! DLSSWrapper not created. Need use FSR");
		return false;
	}

#ifdef IXR_X64
	ID3D11Resource* resourceInput = params.unresolvedColorResource;
	ID3D11Resource* resourceMv = params.motionvectorResource;
	ID3D11Resource* resourceDepth = params.depthbufferResource;
	ID3D11Resource* resourceOutput = params.resolvedColorResource;

	NVSDK_NGX_D3D11_DLSS_Eval_Params dlssEvalParams = {};

	dlssEvalParams.Feature.pInColor = resourceInput;
	dlssEvalParams.Feature.pInOutput = resourceOutput;
	dlssEvalParams.Feature.InSharpness = params.sharpness;

	dlssEvalParams.pInDepth = resourceDepth;
	dlssEvalParams.pInMotionVectors = resourceMv;

	dlssEvalParams.InRenderSubrectDimensions.Width = params.renderWidth;
	dlssEvalParams.InRenderSubrectDimensions.Height = params.renderHeight;

	dlssEvalParams.InJitterOffsetX = params.cameraJitterX;
	dlssEvalParams.InJitterOffsetY = params.cameraJitterY;

	dlssEvalParams.InReset = params.cameraReset;

	// adjust the x direction in motion vector to fit FSR2's requirement
	dlssEvalParams.InMVScaleX = -(float)params.renderWidth * 0.5f;
	dlssEvalParams.InMVScaleY = (float)params.renderHeight * 0.5f;

	dlssEvalParams.pInTransparencyMask = params.transparencyAndCompositionResource;
	
	NVSDK_NGX_Result result = NGX_D3D11_EVALUATE_DLSS_EXT(RContext, Handle, NgxParameters, &dlssEvalParams);
	if(result != NVSDK_NGX_Result_Success)
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

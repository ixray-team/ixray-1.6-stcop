#include "../RHI.h"
#include <d3d11.h>
#ifdef IXR_WINDOWS
#include "../D3D12/Device.h"
#endif
#include <ngx/nvsdk_ngx.h>
#include <ngx/nvsdk_ngx_helpers.h>

#include "../DLSSWrapper.h"

struct DLSS_State_data {

    NVSDK_NGX_Parameter* NgxParameters = nullptr;
    NVSDK_NGX_Handle* Handle = nullptr;

    Ivector2 DisplaySize{};

    bool IsD3D12 = false;
    bool DLSSInited = false;
    bool Created = false;
};

static DLSS_State_data DLSS_State;

template<typename T>
static T* RHI_Surface(IRHISurface* surface)
{
	return surface ? static_cast<T*>(surface->GetRawTexture()) : nullptr;
}

template<typename T, typename Eval>
static void FillDLSSParameters(Eval& DLSSEvalParams, const DLSSWrapper::DrawParameters& params)
{

	DLSSEvalParams.Feature.pInColor = RHI_Surface<T>(params.unresolvedColorResource);
	DLSSEvalParams.Feature.pInOutput = RHI_Surface<T>(params.resolvedColorResource);
	DLSSEvalParams.Feature.InSharpness = params.sharpness;

	DLSSEvalParams.pInDepth = RHI_Surface<T>(params.depthbufferResource);
	DLSSEvalParams.pInMotionVectors = RHI_Surface<T>(params.motionvectorResource);

	DLSSEvalParams.InRenderSubrectDimensions.Width = params.renderWidth;
	DLSSEvalParams.InRenderSubrectDimensions.Height = params.renderHeight;

	DLSSEvalParams.InJitterOffsetX = params.cameraJitterX;
	DLSSEvalParams.InJitterOffsetY = params.cameraJitterY;

	DLSSEvalParams.InReset = params.cameraReset;

	DLSSEvalParams.InMVScaleX = -(float)params.renderWidth * 0.5f;
	DLSSEvalParams.InMVScaleY = (float)params.renderHeight * 0.5f;

	DLSSEvalParams.pInTransparencyMask = RHI_Surface<T>(params.transparencyAndCompositionResource);
	DLSSEvalParams.InFrameTimeDeltaInMsec = params.frameTimeDelta;

}

DLSSWrapper g_DLSSWrapper;

u32 DLSSWrapper::GetOptimalPresetForScale(float scale, u32 preset)
{
	if (preset == 5)
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

	return preset;
}

void DLSSWrapper::Create()
{
	Destroy();
    DLSS_State.IsD3D12 = GRHI->APILevel == ERHI_API_LAYER::D3D12;

	if (GRHI->DevicePtr->FeatureLevel < D3D_FEATURE_LEVEL_11_1)
	{
		return;
	}

#ifdef IXR_X64
	NVSDK_NGX_Result Result;

	if (!DLSS_State.DLSSInited)
	{
		#ifdef IXR_WINDOWS
        Result = DLSS_State.IsD3D12 ?
            NVSDK_NGX_D3D12_Init(1602, L"", static_cast<ID3D12Device*>(GRHI->DevicePtr->RawDevice)) :
            NVSDK_NGX_D3D11_Init(1602, L"", static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice));
#else
        Result = NVSDK_NGX_D3D11_Init(1602, L"", static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice));
#endif

		if (Result != NVSDK_NGX_Result_Success)
		{
			return;
		}

		DLSS_State.DLSSInited = true;
	}

	#ifdef IXR_WINDOWS
    Result = DLSS_State.IsD3D12 ? NVSDK_NGX_D3D12_GetCapabilityParameters(&DLSS_State.NgxParameters) :
        NVSDK_NGX_D3D11_GetCapabilityParameters(&DLSS_State.NgxParameters);
#else
    Result = NVSDK_NGX_D3D11_GetCapabilityParameters(&DLSS_State.NgxParameters);
#endif

	if (Result != NVSDK_NGX_Result_Success)
	{
		return;
	}

	uint32_t NeedsUpdatedDriver = 1;
	Result = DLSS_State.NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_NeedsUpdatedDriver, &NeedsUpdatedDriver);

	if (NeedsUpdatedDriver)
	{
		Msg("! PLEASE UPDATE YOUR DRIVER");
	}

	uint32_t DlssAvailable = 0;
	Result = DLSS_State.NgxParameters->Get(NVSDK_NGX_Parameter_SuperSampling_Available, &DlssAvailable);

	if (!DlssAvailable)
	{
		#ifdef IXR_WINDOWS
        if (DLSS_State.IsD3D12)
        {
            NVSDK_NGX_D3D12_DestroyParameters(DLSS_State.NgxParameters);
        }
        else
#endif
        {
            NVSDK_NGX_D3D11_DestroyParameters(DLSS_State.NgxParameters);
        }
		DLSS_State.NgxParameters = nullptr;
		return;
	}

	DLSS_State.Created = true;
#endif
}

bool DLSSWrapper::GetRenderScale(float& RenderScale, u32 preset, float scale, u32 width, u32 height)
{
	if (!DLSS_State.Created || !DLSS_State.NgxParameters)
	{
		Msg("! GetRenderScale DLSSWrapper not valid. Fallback!");
		return false;
	}

	u32 PresetID = GetOptimalPresetForScale(scale, preset);

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
	NVSDK_NGX_Result Result = NGX_DLSS_GET_OPTIMAL_SETTINGS(DLSS_State.NgxParameters, width, height, PerfQualityValue, &RenderW, &RenderH, &MaxW, &MaxH, &MinW, &MinH, &Sharp);

	if (Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_DLSS_GET_OPTIMAL_SETTINGS not valid. Fallback!");
		return false;
	}

	Msg("* DLSS Target - %dx%d, Min - %dx%d, Max - %dx%d, Sharp - %f", RenderW, RenderH, MaxW, MaxH, MinW, MinH, Sharp);
	RenderScale = float(RenderH) / float(height);

	return true;
}

void DLSSWrapper::Resize(const ContextParameters& Parameters)
{
	PROF_EVENT("DLSSWrapper::Resize");

	if (!DLSS_State.Created)
	{
		return;
	}

#ifdef IXR_X64
	// Устанавливаем пресет для выбранного режима качества
	u32 PresetID = GetOptimalPresetForScale(Parameters.scale, Parameters.preset);

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

	DLSS_State.NgxParameters->Set(*RenderPreset, static_cast<int>(NVSDK_NGX_DLSS_Hint_Render_Preset_K));

	Msg("* Resize DLSSWrapper Render Preset [%s]", *RenderPreset);

	DLSS_State.DisplaySize = Parameters.displaySize;
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

	NVSDK_NGX_Result Result;
#ifdef IXR_WINDOWS
    if (DLSS_State.IsD3D12)
    {
        auto& device = *static_cast<InternalDevice12*>(GRHI->DevicePtr);
        xrCriticalSectionGuard guard(device.ContextMutex());
        if (DLSS_State.Handle)
        {
            device.Flush();
            NVSDK_NGX_D3D12_ReleaseFeature(DLSS_State.Handle);
            DLSS_State.Handle = nullptr;
        }
        Result = NGX_D3D12_CREATE_DLSS_EXT(device.Commands(), 1, 1, &DLSS_State.Handle,
            DLSS_State.NgxParameters, &DLSSCreateParams);
        device.InvalidateBindings();
    }
    else
#endif
    {
        if (DLSS_State.Handle)
        {
            NVSDK_NGX_D3D11_ReleaseFeature(DLSS_State.Handle);
            DLSS_State.Handle = nullptr;
        }
        Result = NGX_D3D11_CREATE_DLSS_EXT(static_cast<ID3D11DeviceContext*>(GRHI->GetContext()),
            &DLSS_State.Handle, DLSS_State.NgxParameters, &DLSSCreateParams);
    }

	if (Result != NVSDK_NGX_Result_Success)
	{
		Msg("! NGX_D3D11_CREATE_DLSS_EXT not valid. Need use FSR");
		DLSS_State.Created = false;
		return;
	}
#endif
}

void DLSSWrapper::Destroy()
{
#ifdef IXR_X64
#ifdef IXR_WINDOWS
    if (DLSS_State.IsD3D12 && DLSS_State.DLSSInited)
    {
        static_cast<InternalDevice12*>(GRHI->DevicePtr)->Flush();
    }
#endif
	if (DLSS_State.Handle != nullptr)
	{
		#ifdef IXR_WINDOWS
        if (DLSS_State.IsD3D12)
        {
            NVSDK_NGX_D3D12_ReleaseFeature(DLSS_State.Handle);
        }
        else
#endif
        {
            NVSDK_NGX_D3D11_ReleaseFeature(DLSS_State.Handle);
        }
        DLSS_State.Handle = nullptr;
	}

	if (DLSS_State.NgxParameters != nullptr)
	{
		#ifdef IXR_WINDOWS
        if (DLSS_State.IsD3D12)
        {
            NVSDK_NGX_D3D12_DestroyParameters(DLSS_State.NgxParameters);
        }
        else
#endif
        {
            NVSDK_NGX_D3D11_DestroyParameters(DLSS_State.NgxParameters);
        }
		DLSS_State.NgxParameters = nullptr;
	}

	if (DLSS_State.DLSSInited)
	{
		#ifdef IXR_WINDOWS
        if (DLSS_State.IsD3D12)
        {
            NVSDK_NGX_D3D12_Shutdown1(static_cast<ID3D12Device*>(GRHI->DevicePtr->RawDevice));
        }
        else
#endif
        {
            NVSDK_NGX_D3D11_Shutdown1(nullptr);
        }
		DLSS_State.DLSSInited = false;
	}

	DLSS_State.Created = false;
#endif
}

bool DLSSWrapper::Draw(const DrawParameters& params)
{
	if(!DLSS_State.Created)
	{
		Msg("! DLSSWrapper not created. Need use FSR");
		return false;
	}

#ifdef IXR_X64
    NVSDK_NGX_Result Result;
#ifdef IXR_WINDOWS
    if (DLSS_State.IsD3D12)
    {
        auto& device = *static_cast<InternalDevice12*>(GRHI->DevicePtr);
        xrCriticalSectionGuard guard(device.ContextMutex());
        NVSDK_NGX_D3D12_DLSS_Eval_Params evaluation = {};
        FillDLSSParameters<ID3D12Resource>(evaluation, params);
        device.PrepareUpscale(params.unresolvedColorResource, false);
        device.PrepareUpscale(params.depthbufferResource, false);
        device.PrepareUpscale(params.motionvectorResource, false);
        device.PrepareUpscale(params.transparencyAndCompositionResource, false);
        device.PrepareUpscale(params.resolvedColorResource, true);
        Result = NGX_D3D12_EVALUATE_DLSS_EXT(device.Commands(), DLSS_State.Handle, DLSS_State.NgxParameters, &evaluation);
        device.UAVBarrier(evaluation.Feature.pInOutput);
        device.InvalidateBindings();
    }
    else
#endif
    {
        NVSDK_NGX_D3D11_DLSS_Eval_Params evaluation = {};
        FillDLSSParameters<ID3D11Texture2D>(evaluation, params);
        Result = NGX_D3D11_EVALUATE_DLSS_EXT(static_cast<ID3D11DeviceContext*>(GRHI->GetContext()),
            DLSS_State.Handle, DLSS_State.NgxParameters, &evaluation);
    }

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

const Ivector2& DLSSWrapper::GetDisplaySize() const
{
	return DLSS_State.DisplaySize;
}

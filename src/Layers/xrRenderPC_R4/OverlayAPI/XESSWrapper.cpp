#include "stdafx.h"
#include "XESSWrapper.h"

XeSSWrapper g_XESSWrapper;

extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

static void XeSSLog(const char* Message, xess_logging_level_t Level)
{
	Msg("%s [XeSS] %s", Level >= XESS_LOGGING_LEVEL_ERROR ? "!" : "~", Message);
}

XeSSWrapper::~XeSSWrapper()
{
	Destroy();
}

u32 XeSSWrapper::GetOptimalPresetForScale(float Scale)
{
	if (ps_render_scale_preset == 5)
	{
		if (Scale >= 0.9f)
		{
			return 0;
		}
		else if (Scale >= 0.64f)
		{
			return 1;
		}
		else if (Scale >= 0.55f)
		{
			return 2;
		}
		else if (Scale >= 0.45f)
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

// Engine presets: 0 native, 1 x1.5, 2 x1.724, 3 x2.0, 4 x3.0 (see CRenderDevice::Reset)
xess_quality_settings_t XeSSWrapper::GetQualityForPreset(u32 PresetID)
{
	switch (PresetID)
	{
		case 4: return XESS_QUALITY_SETTING_ULTRA_PERFORMANCE;
		case 3: return XESS_QUALITY_SETTING_BALANCED;
		case 2: return XESS_QUALITY_SETTING_QUALITY;
		case 1: return XESS_QUALITY_SETTING_ULTRA_QUALITY;
		default: return XESS_QUALITY_SETTING_AA;
	}
}

// Default XeSS upscale ratios (XeSS 1.3+ scale factors)
float XeSSWrapper::GetUpscaleRatioForPreset(u32 PresetID)
{
	switch (PresetID)
	{
		case 4: return 3.0f;
		case 3: return 2.0f;
		case 2: return 1.7f;
		case 1: return 1.5f;
		default: return 1.0f;
	}
}

bool XeSSWrapper::GetRenderScale(float& RenderScale)
{
	if (Unsupported)
	{
		Msg("! GetRenderScale XeSSWrapper not supported on this GPU. Fallback!");
		return false;
	}

	const u32 PresetID = GetOptimalPresetForScale(ps_render_scale);
	RenderScale = 1.0f / GetUpscaleRatioForPreset(PresetID);

	Msg("* XeSS Target - %dx%d", iFloor(Device.TargetWidth * RenderScale), iFloor(Device.TargetHeight * RenderScale));
	return true;
}

bool XeSSWrapper::Create(const ContextParameters& Parameters)
{
	Destroy();

	if (Unsupported || !Parameters.D3DDevice || Parameters.DisplaySize.x <= 0 || Parameters.RenderSize.x <= 0)
	{
		return false;
	}

	xess_result_t Result = xessD3D11CreateContext(Parameters.D3DDevice, &Context);
	if (Result != XESS_RESULT_SUCCESS)
	{
		Context = nullptr;

		if (Result == XESS_RESULT_ERROR_UNSUPPORTED_DEVICE || Result == XESS_RESULT_ERROR_UNSUPPORTED_DRIVER || Result == XESS_RESULT_ERROR_CANT_LOAD_LIBRARY)
		{
			Unsupported = true;
		}

		Msg("! [XeSS] Failed to create D3D11 context: %s", ResultToString(Result));
		return false;
	}

	xessSetLoggingCallback(Context, XESS_LOGGING_LEVEL_WARNING, XeSSLog);

	if (xessIsOptimalDriver(Context) == XESS_RESULT_WARNING_OLD_DRIVER)
	{
		Msg("~ [XeSS] Graphics driver is older than recommended, please update it");
	}

	const u32 PresetID = GetOptimalPresetForScale(ps_render_scale);

	xess_d3d11_init_params_t InitParams = {};
	InitParams.outputResolution = { (u32)Parameters.DisplaySize.x, (u32)Parameters.DisplaySize.y };
	InitParams.qualitySetting = GetQualityForPreset(PresetID);
	// Scene color is HDR, motion vectors are in render resolution, depth is not inverted
	InitParams.initFlags = XESS_INIT_FLAG_ENABLE_AUTOEXPOSURE;

	Result = xessD3D11Init(Context, &InitParams);
	if (Result != XESS_RESULT_SUCCESS)
	{
		Msg("! [XeSS] Failed to initialize: %s", ResultToString(Result));
		Destroy();
		return false;
	}

	xess_2d_t OptimalInput = { 0, 0 };
	xessGetOptimalInputResolution(Context, &InitParams.outputResolution, InitParams.qualitySetting, &OptimalInput, &MinInput, &MaxInput);

	xess_version_t Version = {};
	xessGetVersion(&Version);

	RenderSize = Parameters.RenderSize;
	Msg("* [XeSS] %u.%u.%u initialized: render %dx%d, display %dx%d, optimal input %ux%u", Version.major, Version.minor, Version.patch,
		RenderSize.x, RenderSize.y, Parameters.DisplaySize.x, Parameters.DisplaySize.y, OptimalInput.x, OptimalInput.y);

	HistoryValid = false;
	return true;
}

void XeSSWrapper::Destroy()
{
	if (Context)
	{
		xessDestroyContext(Context);
		Context = nullptr;
	}

	HistoryValid = false;
}

bool XeSSWrapper::Draw(const DrawParameters& Parameters)
{
	if (!Context)
	{
		return false;
	}

	// Engine velocity is stored in NDC-like units, convert to render-resolution pixels (same as FSR3)
	xessSetVelocityScale(Context, -float(Parameters.RenderWidth) * 0.5f * ps_r_xess_mv_scale.x, float(Parameters.RenderHeight) * 0.5f * ps_r_xess_mv_scale.y);
	xessSetJitterScale(Context, ps_r_xess_jitter_scale.x, ps_r_xess_jitter_scale.y);

	// Drop history after pauses (menu, loading) to avoid ghosting from stale frames
	const bool FrameGap = LastDrawFrame + 1 != Device.dwFrame;
	LastDrawFrame = Device.dwFrame;

	xess_d3d11_execute_params_t ExecParams = {};
	ExecParams.pColorTexture = Parameters.ColorResource;
	ExecParams.pVelocityTexture = Parameters.VelocityResource;
	ExecParams.pDepthTexture = Parameters.DepthResource;
	ExecParams.pOutputTexture = Parameters.OutputResource;

	ExecParams.jitterOffsetX = Parameters.JitterX;
	ExecParams.jitterOffsetY = Parameters.JitterY;
	ExecParams.exposureScale = 1.0f;
	ExecParams.resetHistory = (Parameters.CameraReset || !HistoryValid || FrameGap) ? 1 : 0;

	ExecParams.inputWidth = std::clamp(Parameters.RenderWidth, MinInput.x, MaxInput.x);
	ExecParams.inputHeight = std::clamp(Parameters.RenderHeight, MinInput.y, MaxInput.y);

	const xess_result_t Result = xessD3D11Execute(Context, &ExecParams);
	if (Result != XESS_RESULT_SUCCESS)
	{
		Msg("! [XeSS] Execute failed: %s", ResultToString(Result));
		Destroy();
		return false;
	}

	HistoryValid = true;
	return true;
}

const char* XeSSWrapper::ResultToString(xess_result_t Result)
{
	switch (Result)
	{
		case XESS_RESULT_WARNING_NONEXISTING_FOLDER: return "XESS_RESULT_WARNING_NONEXISTING_FOLDER";
		case XESS_RESULT_WARNING_OLD_DRIVER: return "XESS_RESULT_WARNING_OLD_DRIVER";
		case XESS_RESULT_SUCCESS: return "XESS_RESULT_SUCCESS";
		case XESS_RESULT_ERROR_UNSUPPORTED_DEVICE: return "XESS_RESULT_ERROR_UNSUPPORTED_DEVICE";
		case XESS_RESULT_ERROR_UNSUPPORTED_DRIVER: return "XESS_RESULT_ERROR_UNSUPPORTED_DRIVER";
		case XESS_RESULT_ERROR_UNINITIALIZED: return "XESS_RESULT_ERROR_UNINITIALIZED";
		case XESS_RESULT_ERROR_INVALID_ARGUMENT: return "XESS_RESULT_ERROR_INVALID_ARGUMENT";
		case XESS_RESULT_ERROR_DEVICE_OUT_OF_MEMORY: return "XESS_RESULT_ERROR_DEVICE_OUT_OF_MEMORY";
		case XESS_RESULT_ERROR_DEVICE: return "XESS_RESULT_ERROR_DEVICE";
		case XESS_RESULT_ERROR_NOT_IMPLEMENTED: return "XESS_RESULT_ERROR_NOT_IMPLEMENTED";
		case XESS_RESULT_ERROR_INVALID_CONTEXT: return "XESS_RESULT_ERROR_INVALID_CONTEXT";
		case XESS_RESULT_ERROR_OPERATION_IN_PROGRESS: return "XESS_RESULT_ERROR_OPERATION_IN_PROGRESS";
		case XESS_RESULT_ERROR_UNSUPPORTED: return "XESS_RESULT_ERROR_UNSUPPORTED";
		case XESS_RESULT_ERROR_CANT_LOAD_LIBRARY: return "XESS_RESULT_ERROR_CANT_LOAD_LIBRARY";
		case XESS_RESULT_ERROR_WRONG_CALL_ORDER: return "XESS_RESULT_ERROR_WRONG_CALL_ORDER";
		case XESS_RESULT_ERROR_UNKNOWN: return "XESS_RESULT_ERROR_UNKNOWN";
		default: return "Unknown error code";
	}
}

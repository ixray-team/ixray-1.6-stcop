#include "stdafx.h"

#include "FSR3Wrapper.h"

Fsr3Wrapper g_Fsr3Wrapper;

static void fsr3_message(FfxMsgType type, const wchar_t* message)
{
	string512 text { };

	const int written = WideCharToMultiByte(CP_ACP, 0, message, -1, text, sizeof(text) - 1, nullptr, nullptr);
	text[written > 0 ? written : 0] = 0;

	Msg("%s [FSR3] %s", type == FFX_MESSAGE_TYPE_ERROR ? "!" : "~", text);
}

extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

u32 Fsr3Wrapper::GetOptimalPresetForScale(float scale)
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

bool Fsr3Wrapper::GetRenderScale(float& RenderScale)
{
	if (!Created)
	{
		Msg("! GetRenderScale Fsr3Wrapper not valid. Fallback!");
		return false;
	}

	u32 PresetID = GetOptimalPresetForScale(ps_render_scale);
	FfxFsr3UpscalerQualityMode PerfQualityValue = FFX_FSR3UPSCALER_QUALITY_MODE_NATIVEAA;

	switch (PresetID)
	{
		case 4:
		{
			PerfQualityValue = FFX_FSR3UPSCALER_QUALITY_MODE_ULTRA_PERFORMANCE;
			break;
		}
		case 3:
		{
			PerfQualityValue = FFX_FSR3UPSCALER_QUALITY_MODE_PERFORMANCE;
			break;
		}
		case 2:
		{
			PerfQualityValue = FFX_FSR3UPSCALER_QUALITY_MODE_BALANCED;
			break;
		}
		case 1:
		{
			PerfQualityValue = FFX_FSR3UPSCALER_QUALITY_MODE_QUALITY;
			break;
		}
		default:
		{
			PerfQualityValue = FFX_FSR3UPSCALER_QUALITY_MODE_NATIVEAA;
			break;
		}
	}

	u32 RenderW = 0, RenderH = 0;
	FfxErrorCode Result = ffxFsr3UpscalerGetRenderResolutionFromQualityMode(&RenderW, &RenderH, Device.TargetWidth, Device.TargetHeight, PerfQualityValue);

	if (Result != FFX_OK)
	{
		Msg("! ffxFsr3UpscalerGetRenderResolutionFromQualityMode not valid. Fallback!");
		return false;
	}

	Msg("* FSR Target - %dx%d", RenderW, RenderH);
	RenderScale = float(RenderH) / float(Device.TargetHeight);

	return true;
}

bool Fsr3Wrapper::Create(ContextParameters params)
{
	Destroy();

	if (RFeatureLevel < D3D_FEATURE_LEVEL_11_0 || !params.device || !params.maxRenderSize.width || !params.displaySize.width)
	{
		return false;
	}

	ContextParams = params;

	const size_t scratchSize = ffxGetScratchMemorySizeDX11(1);
	ScratchBuffer.resize(scratchSize);

	FfxErrorCode errorCode = ffxGetInterfaceDX11(&ContextDesc.backendInterface, ffxGetDeviceDX11(params.device), ScratchBuffer.data(), ScratchBuffer.size(), 1);

	if (errorCode != FFX_OK)
	{
		Msg("! [FSR3] cannot create the DX11 interface (%d)", errorCode);
		return false;
	}

	auto MakeTexLambda = [&](DXGI_FORMAT fmt, bool renderTarget, ID3D11Texture2D** out) -> bool
	{
		D3D11_TEXTURE2D_DESC TexDesc{};
		TexDesc.Width = params.maxRenderSize.width;
		TexDesc.Height = params.maxRenderSize.height;
		TexDesc.MipLevels = 1;
		TexDesc.ArraySize = 1;
		TexDesc.Format = fmt;
		TexDesc.SampleDesc.Count = 1;
		TexDesc.Usage = D3D11_USAGE_DEFAULT;
		TexDesc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_UNORDERED_ACCESS;

		if (renderTarget)
		{
			TexDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;
		}

		return SUCCEEDED(params.device->CreateTexture2D(&TexDesc, nullptr, out));
	};

	if (!MakeTexLambda(DXGI_FORMAT_R32_FLOAT, true, &DilatedDepth) || !MakeTexLambda(DXGI_FORMAT_R16G16_FLOAT, true, &DilatedMotion) || !MakeTexLambda(DXGI_FORMAT_R32_UINT, false, &ReconstructedPrevDepth))
	{
		Msg("! [FSR3] cannot create the shared buffers");
		Destroy();

		return false;
	}

	ContextDesc.maxRenderSize = params.maxRenderSize;
	ContextDesc.maxUpscaleSize = params.displaySize;
	ContextDesc.fpMessage = fsr3_message;

	ContextDesc.flags = 0;

#ifdef DEBUG
	ContextDesc.flags |= FFX_FSR3UPSCALER_ENABLE_DEBUG_CHECKING;
#endif

	ContextDesc.flags |= FFX_FSR3UPSCALER_ENABLE_HIGH_DYNAMIC_RANGE;
	ContextDesc.flags |= FFX_FSR3UPSCALER_ENABLE_AUTO_EXPOSURE;

	errorCode = ffxFsr3UpscalerContextCreate(&Context, &ContextDesc);

	if (errorCode != FFX_OK)
	{
		Msg("! [FSR3] context creation failed (%d)", errorCode);
		Destroy();

		return false;
	}

	Created = true;
	return true;
}

void Fsr3Wrapper::Destroy()
{
	if (Created)
	{
		ffxFsr3UpscalerContextDestroy(&Context);
		Created = false;
	}

	_RELEASE(DilatedDepth);
	_RELEASE(DilatedMotion);
	_RELEASE(ReconstructedPrevDepth);

	ScratchBuffer.clear();
}

bool Fsr3Wrapper::Draw(const DrawParameters& params)
{
	if (!Created)
	{
		Msg("! Fsr3Wrapper not created. Need use linear filter");
		return false;
	}

	FfxFsr3UpscalerDispatchDescription FsrDesc{};
	FsrDesc.commandList = ffxGetCommandListDX11(params.deviceContext);

	FsrDesc.color = ffxGetResourceDX11(params.unresolvedColorResource, GetFfxResourceDescriptionDX11(params.unresolvedColorResource), nullptr);
	FsrDesc.depth = ffxGetResourceDX11(params.depthbufferResource, GetFfxResourceDescriptionDX11(params.depthbufferResource), nullptr);
	FsrDesc.motionVectors = ffxGetResourceDX11(params.motionvectorResource, GetFfxResourceDescriptionDX11(params.motionvectorResource), nullptr);
	FsrDesc.exposure = ffxGetResourceDX11(nullptr, FfxResourceDescription{}, nullptr);

	FsrDesc.reactive = params.reactiveMapResource
		? ffxGetResourceDX11(params.reactiveMapResource, GetFfxResourceDescriptionDX11(params.reactiveMapResource), nullptr)
		: ffxGetResourceDX11(nullptr, FfxResourceDescription{}, nullptr);

	FsrDesc.transparencyAndComposition = params.transparencyAndCompositionResource
		? ffxGetResourceDX11(params.transparencyAndCompositionResource, GetFfxResourceDescriptionDX11(params.transparencyAndCompositionResource), nullptr)
		: ffxGetResourceDX11(nullptr, FfxResourceDescription{}, nullptr);

	FsrDesc.dilatedDepth = ffxGetResourceDX11(DilatedDepth, GetFfxResourceDescriptionDX11(DilatedDepth),nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);
	FsrDesc.dilatedMotionVectors = ffxGetResourceDX11(DilatedMotion, GetFfxResourceDescriptionDX11(DilatedMotion), nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);
	FsrDesc.reconstructedPrevNearestDepth = ffxGetResourceDX11(ReconstructedPrevDepth, GetFfxResourceDescriptionDX11(ReconstructedPrevDepth), nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);

	FsrDesc.output = ffxGetResourceDX11(params.resolvedColorResource, GetFfxResourceDescriptionDX11(params.resolvedColorResource), nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);

	FsrDesc.jitterOffset.x = params.cameraJitterX;
	FsrDesc.jitterOffset.y = params.cameraJitterY;

	FsrDesc.motionVectorScale.x = -float(params.renderWidth) * 0.5f;
	FsrDesc.motionVectorScale.y = float(params.renderHeight) * 0.5f;

	FsrDesc.renderSize = { params.renderWidth, params.renderHeight };
	FsrDesc.upscaleSize = { params.displayWidth, params.displayHeight };

	FsrDesc.enableSharpening = params.enableSharpening;
	FsrDesc.sharpness = params.sharpness;
	FsrDesc.frameTimeDelta = params.frameTimeDelta;
	FsrDesc.preExposure = 1.0f;
	FsrDesc.reset = params.cameraReset;

	FsrDesc.cameraNear = params.nearPlane;
	FsrDesc.cameraFar = params.farPlane;
	FsrDesc.cameraFovAngleVertical = params.fovH;
	FsrDesc.viewSpaceToMetersFactor = 1.0f;

	const FfxErrorCode ErrorCode = ffxFsr3UpscalerContextDispatch(&Context, &FsrDesc);
	if (ErrorCode != FFX_OK)
	{
		Msg("! [FSR3] dispatch failed (%d)", ErrorCode);
		return false;
	}
	return true;
}

Fsr3Wrapper::~Fsr3Wrapper()
{
	Destroy();
}

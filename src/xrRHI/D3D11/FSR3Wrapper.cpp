#include "../RHI.h"
#include <d3d11.h>
#include <FidelityFX/host/ffx_fsr3upscaler.h>
#include <FidelityFX/host/backends/dx11/ffx_dx11.h>

#include "../FSR3Wrapper.h"

struct Fsr_State_data {

	bool Created = false;

	FfxFsr3UpscalerContext Context = {};
	FfxFsr3UpscalerContextDescription ContextDesc = {};
	Fsr3Wrapper::ContextParameters ContextParams;

	// FSR3 shared resources (see ffxFsr3UpscalerGetSharedResourceDescriptions in the component
	// source - getting the formats wrong is not caught at creation, only later as corrupt output).
	ID3D11Texture2D* DilatedDepth = nullptr;
	ID3D11Texture2D* DilatedMotion = nullptr;
	ID3D11Texture2D* ReconstructedPrevDepth = nullptr;

	xr_vector<char> ScratchBuffer;
};

static Fsr_State_data Fsr_State;

static ID3D11Resource* RHI_Surface(IRHISurface* surface)
{
	return surface ? static_cast<ID3D11Resource*>(surface->GetRawTexture()) : nullptr;
}

Fsr3Wrapper g_Fsr3Wrapper;

s32 Fsr3Wrapper::GetJitterPhaseCount(u32 render_width, u32 display_width) const
{
	return ffxFsr3UpscalerGetJitterPhaseCount(static_cast<int32_t>(render_width), static_cast<int32_t>(display_width));
}

void Fsr3Wrapper::GetJitterOffset(float& out_x, float& out_y, u32 frame, s32 phase_count) const
{
	ffxFsr3UpscalerGetJitterOffset(&out_x, &out_y, static_cast<int32_t>(frame), phase_count);
}

static void fsr3_message(FfxMsgType type, const wchar_t* message)
{
	string512 text { };

	const int written = WideCharToMultiByte(CP_ACP, 0, message, -1, text, sizeof(text) - 1, nullptr, nullptr);
	text[written > 0 ? written : 0] = 0;

	Msg("%s [FSR3] %s", type == FFX_MESSAGE_TYPE_ERROR ? "!" : "~", text);
}

u32 Fsr3Wrapper::GetOptimalPresetForScale(float scale, u32 preset)
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

bool Fsr3Wrapper::GetRenderScale(float& RenderScale, u32 preset, float scale, u32 width, u32 height)
{
	if (!Fsr_State.Created)
	{
		Msg("! GetRenderScale Fsr3Wrapper not valid. Fallback!");
		return false;
	}

	u32 PresetID = GetOptimalPresetForScale(scale, preset);
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
	FfxErrorCode Result = ffxFsr3UpscalerGetRenderResolutionFromQualityMode(&RenderW, &RenderH, width, height, PerfQualityValue);

	if (Result != FFX_OK)
	{
		Msg("! ffxFsr3UpscalerGetRenderResolutionFromQualityMode not valid. Fallback!");
		return false;
	}

	Msg("* FSR Target - %dx%d", RenderW, RenderH);
	RenderScale = float(RenderH) / float(height);

	return true;
}

bool Fsr3Wrapper::Create(ContextParameters params)
{
	Destroy();

	if (GRHI->APILevel != ERHI_API_LAYER::D3D11 || GRHI->DevicePtr->FeatureLevel < D3D_FEATURE_LEVEL_11_0 || !static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice) || !params.maxRenderSize.width || !params.displaySize.width)
	{
		return false;
	}

	Fsr_State.ContextParams = params;

	const size_t scratchSize = ffxGetScratchMemorySizeDX11(1);
	Fsr_State.ScratchBuffer.resize(scratchSize);

	FfxErrorCode errorCode = ffxGetInterfaceDX11(&Fsr_State.ContextDesc.backendInterface, ffxGetDeviceDX11(static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice)), Fsr_State.ScratchBuffer.data(), Fsr_State.ScratchBuffer.size(), 1);

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

		return SUCCEEDED(static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice)->CreateTexture2D(&TexDesc, nullptr, out));
	};

	if (!MakeTexLambda(DXGI_FORMAT_R32_FLOAT, true, &Fsr_State.DilatedDepth) || !MakeTexLambda(DXGI_FORMAT_R16G16_FLOAT, true, &Fsr_State.DilatedMotion) || !MakeTexLambda(DXGI_FORMAT_R32_UINT, false, &Fsr_State.ReconstructedPrevDepth))
	{
		Msg("! [FSR3] cannot create the shared buffers");
		Destroy();

		return false;
	}

	Fsr_State.ContextDesc.maxRenderSize = { params.maxRenderSize.width, params.maxRenderSize.height };
	Fsr_State.ContextDesc.maxUpscaleSize = { params.displaySize.width, params.displaySize.height };
	Fsr_State.ContextDesc.fpMessage = fsr3_message;

	Fsr_State.ContextDesc.flags = 0;

#ifdef DEBUG
	Fsr_State.ContextDesc.flags |= FFX_FSR3UPSCALER_ENABLE_DEBUG_CHECKING;
#endif

	Fsr_State.ContextDesc.flags |= FFX_FSR3UPSCALER_ENABLE_HIGH_DYNAMIC_RANGE;
	Fsr_State.ContextDesc.flags |= FFX_FSR3UPSCALER_ENABLE_AUTO_EXPOSURE;

	errorCode = ffxFsr3UpscalerContextCreate(&Fsr_State.Context, &Fsr_State.ContextDesc);

	if (errorCode != FFX_OK)
	{
		Msg("! [FSR3] context creation failed (%d)", errorCode);
		Destroy();

		return false;
	}

	Fsr_State.Created = true;
	return true;
}

void Fsr3Wrapper::Destroy()
{
	if (Fsr_State.Created)
	{
		ffxFsr3UpscalerContextDestroy(&Fsr_State.Context);
		Fsr_State.Created = false;
	}

	if (Fsr_State.DilatedDepth)
	{
		Fsr_State.DilatedDepth->Release();
		Fsr_State.DilatedDepth = nullptr;
	}
	if (Fsr_State.DilatedMotion)
	{
		Fsr_State.DilatedMotion->Release();
		Fsr_State.DilatedMotion = nullptr;
	}
	if (Fsr_State.ReconstructedPrevDepth)
	{
		Fsr_State.ReconstructedPrevDepth->Release();
		Fsr_State.ReconstructedPrevDepth = nullptr;
	}

	Fsr_State.ScratchBuffer.clear();
}

bool Fsr3Wrapper::Draw(const DrawParameters& params)
{
	if (!Fsr_State.Created)
	{
		Msg("! Fsr3Wrapper not created. Need use linear filter");
		return false;
	}

	FfxFsr3UpscalerDispatchDescription FsrDesc{};
	FsrDesc.commandList = ffxGetCommandListDX11(static_cast<ID3D11DeviceContext*>(GRHI->GetContext()));

	FsrDesc.color = ffxGetResourceDX11(RHI_Surface(params.unresolvedColorResource), GetFfxResourceDescriptionDX11(RHI_Surface(params.unresolvedColorResource)), nullptr);
	FsrDesc.depth = ffxGetResourceDX11(RHI_Surface(params.depthbufferResource), GetFfxResourceDescriptionDX11(RHI_Surface(params.depthbufferResource)), nullptr);
	FsrDesc.motionVectors = ffxGetResourceDX11(RHI_Surface(params.motionvectorResource), GetFfxResourceDescriptionDX11(RHI_Surface(params.motionvectorResource)), nullptr);
	FsrDesc.exposure = ffxGetResourceDX11(nullptr, FfxResourceDescription{}, nullptr);

	FsrDesc.reactive = RHI_Surface(params.reactiveMapResource)
		? ffxGetResourceDX11(RHI_Surface(params.reactiveMapResource), GetFfxResourceDescriptionDX11(RHI_Surface(params.reactiveMapResource)), nullptr)
		: ffxGetResourceDX11(nullptr, FfxResourceDescription{}, nullptr);

	FsrDesc.transparencyAndComposition = RHI_Surface(params.transparencyAndCompositionResource)
		? ffxGetResourceDX11(RHI_Surface(params.transparencyAndCompositionResource), GetFfxResourceDescriptionDX11(RHI_Surface(params.transparencyAndCompositionResource)), nullptr)
		: ffxGetResourceDX11(nullptr, FfxResourceDescription{}, nullptr);

	FsrDesc.dilatedDepth = ffxGetResourceDX11(Fsr_State.DilatedDepth, GetFfxResourceDescriptionDX11(Fsr_State.DilatedDepth),nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);
	FsrDesc.dilatedMotionVectors = ffxGetResourceDX11(Fsr_State.DilatedMotion, GetFfxResourceDescriptionDX11(Fsr_State.DilatedMotion), nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);
	FsrDesc.reconstructedPrevNearestDepth = ffxGetResourceDX11(Fsr_State.ReconstructedPrevDepth, GetFfxResourceDescriptionDX11(Fsr_State.ReconstructedPrevDepth), nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);

	FsrDesc.output = ffxGetResourceDX11(RHI_Surface(params.resolvedColorResource), GetFfxResourceDescriptionDX11(RHI_Surface(params.resolvedColorResource)), nullptr, FFX_RESOURCE_STATE_UNORDERED_ACCESS);

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

	const FfxErrorCode ErrorCode = ffxFsr3UpscalerContextDispatch(&Fsr_State.Context, &FsrDesc);
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

bool Fsr3Wrapper::IsCreated() const
{
	return Fsr_State.Created;
}

RHIExtent2D Fsr3Wrapper::GetDisplaySize() const
{
	return { Fsr_State.ContextDesc.maxUpscaleSize.width, Fsr_State.ContextDesc.maxUpscaleSize.height };
}

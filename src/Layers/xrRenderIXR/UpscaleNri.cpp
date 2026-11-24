/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "UpscaleNri.h"
#include "NriPass.h"
#include "GpuTextures.h"

extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>
#include <Extensions/NRIUpscaler.h>

namespace
{
	constexpr u32 TextureCount = 5;
	constexpr u32 KeptFrames = 4;

	nri::CoreInterface Api;
	nri::UpscalerInterface UpscalerApi;
	nri::Upscaler* Handle = nullptr;
	nri::UpscalerProps Props = {};
	u32 Key[5] = {};
	bool IsLoaded = false;
	bool HasApi = false;

	// What a dispatch has wrapped stays until the command list that names it has run
	struct Kept
	{
		nri::Texture* Textures[TextureCount];
		nri::Descriptor* Views[TextureCount];
	};

	Kept Frames[KeptFrames] = {};
	u32 FrameIndex = 0;
	bool IsHistoryLost = false;

	void Release(Kept& Item)
	{
		for (u32 Index = 0; Index < TextureCount; ++Index)
		{
			if (Item.Views[Index])
			{
				Api.DestroyDescriptor(Item.Views[Index]);
			}

			if (Item.Textures[Index])
			{
				Api.DestroyTexture(Item.Textures[Index]);
			}
		}

		Item = {};
	}

	// r_scale: native, quality, balanced, performance, ultra performance; custom takes the nearest
	u32 QualityPreset()
	{
		if (ps_render_scale_preset < 5)
		{
			return ps_render_scale_preset;
		}

		static const float Least[] = { 0.9f, 0.7f, 0.6f, 0.5f };
		u32 Preset = 0;

		while (Preset < 4 && ps_render_scale < Least[Preset])
		{
			++Preset;
		}

		return Preset;
	}

	// By the scale mode: 2 - DLSS, 3 - FSR, 4 - XeSS
	const nri::UpscalerType Types[] = { nri::UpscalerType::DLSR, nri::UpscalerType::FSR, nri::UpscalerType::XESS };

	bool LoadApi(nri::Device& Gpu)
	{
		if (!IsLoaded)
		{
			IsLoaded = true;
			HasApi = nri::nriGetInterface(Gpu, NRI_INTERFACE(nri::CoreInterface), &Api) == nri::Result::SUCCESS &&
				nri::nriGetInterface(Gpu, NRI_INTERFACE(nri::UpscalerInterface), &UpscalerApi) == nri::Result::SUCCESS;
		}

		return HasApi;
	}

	// Wanted: scale mode, quality preset, r_dlss_preset, output width and height
	bool MakeUpscaler(const u32 (&Wanted)[5])
	{
		UpscaleNri::Destroy();
		nri::Device& Gpu = *GRHI->Nri->Device;
		static const char* const Names[] = { "DLSS", "FSR", "XeSS" };
		static const nri::UpscalerMode Modes[] = { nri::UpscalerMode::NATIVE, nri::UpscalerMode::QUALITY, nri::UpscalerMode::BALANCED, nri::UpscalerMode::PERFORMANCE, nri::UpscalerMode::ULTRA_PERFORMANCE };
		const u32 Kind = Wanted[0] - 2;
		const u32 Preset = Wanted[1];
		const u32 Model = Wanted[2];

		if (!LoadApi(Gpu) || !UpscalerApi.IsUpscalerSupported(Gpu, Types[Kind]))
		{
			Msg("! %s is not supported on this device", Names[Kind]);
			return false;
		}

		nri::UpscalerDesc Desc = {};
		Desc.upscaleResolution = { nri::Dim_t(Wanted[3]), nri::Dim_t(Wanted[4]) };
		Desc.type = Types[Kind];
		Desc.mode = Modes[Preset];
		Desc.flags = nri::UpscalerBits::HDR;

		// XeSS takes a mask of another meaning
		if (Kind != 2)
		{
			Desc.flags |= nri::UpscalerBits::USE_REACTIVE;
		}

		if (Kind == 0)
		{
			// r_dlss_preset: E (CNN), K (transformer), L and M (second transformer)
			static const u8 Presets[] = { 5, 11, 12, 13 };
			Desc.preset = Presets[std::min(Model, 3u)];
		}

		if (UpscalerApi.CreateUpscaler(Gpu, Desc, Handle) != nri::Result::SUCCESS)
		{
			Handle = nullptr;
			Msg("! %s could not be created", Names[Kind]);
			return false;
		}

		UpscalerApi.GetUpscalerProps(*Handle, Props);
		std::memcpy(Key, Wanted, sizeof(Key));
		IsHistoryLost = true;
		Msg("* %s through NRI: %ux%u to %ux%u", Names[Kind], u32(Props.renderResolution.w), u32(Props.renderResolution.h), Wanted[3], Wanted[4]);
		return true;
	}
} // namespace

bool UpscaleNri::IsWanted()
{
	return GRHI->APILevel == ERHI_API_LAYER::D3D12 && ps_r_scale_mode >= 2 && ps_r_scale_mode <= 4;
}

bool UpscaleNri::IsSupported(u32 Mode)
{
	if (GRHI->APILevel != ERHI_API_LAYER::D3D12)
	{
		return Mode == 3;
	}

	return GRHI->Nri && GRHI->Nri->Device && LoadApi(*GRHI->Nri->Device) && UpscalerApi.IsUpscalerSupported(*GRHI->Nri->Device, Types[Mode - 2]);
}

bool UpscaleNri::Sync(u32 Width, u32 Height)
{
	if (!IsWanted())
	{
		Destroy();
		return false;
	}

	const u32 Wanted[5] = { ps_r_scale_mode, QualityPreset(), VideoSettings::Effective(EVideoSetting::DlssPreset), Width, Height };
	return (Handle && std::memcmp(Key, Wanted, sizeof(Key)) == 0) || MakeUpscaler(Wanted);
}

bool UpscaleNri::IsReady()
{
	return Handle && Key[0] == ps_r_scale_mode;
}

bool UpscaleNri::IsSized(u32 Width, u32 Height)
{
	return Handle && Key[3] == Width && Key[4] == Height;
}

bool UpscaleNri::Resize(u32 Width, u32 Height)
{
	if (!Handle || IsSized(Width, Height))
	{
		return Handle != nullptr;
	}

	const u32 Wanted[5] = { Key[0], Key[1], Key[2], Width, Height };
	return MakeUpscaler(Wanted);
}

bool UpscaleNri::RenderSize(u32& Width, u32& Height)
{
	if (!Handle)
	{
		return false;
	}

	Width = Props.renderResolution.w;
	Height = Props.renderResolution.h;
	return true;
}

bool UpscaleNri::Draw(const Frame& Frame)
{
	if (!Handle)
	{
		return false;
	}

	IRHISurface* Reads[4] = { Frame.Colour, Frame.Motion, Frame.Depth, Frame.Reactive };
	IRHIRenderTargetView* Targets[1] = { Frame.Output };
	nri::CommandBuffer& Commands = *BeginNriCommands(Reads, 4, Targets, 1, nullptr);

	Kept& Item = Frames[FrameIndex++ % KeptFrames];
	Release(Item);
	IRHISurface* Surfaces[TextureCount] = { Frame.Output->GetSurface(), Frame.Colour, Frame.Motion, Frame.Depth, Frame.Reactive };

	for (u32 Index = 0; Index < TextureCount; ++Index)
	{
		Item.Textures[Index] = WrapNriSurface(Surfaces[Index]);
		nri::TextureViewDesc View = {};
		View.texture = Item.Textures[Index];
		View.type = Index == 0 ? nri::TextureView::STORAGE_TEXTURE : nri::TextureView::TEXTURE;
		View.mipNum = 1;
		View.layerNum = 1;
		Api.CreateTextureView(View, Item.Views[Index]);
	}

	// The old backend hands the output over as drawn into, and takes it back so
	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = Item.Textures[0];
	Barrier.before = { nri::AccessBits::COLOR_ATTACHMENT, nri::Layout::COLOR_ATTACHMENT, nri::StageBits::COLOR_ATTACHMENT };
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;
	Api.CmdBarrier(Commands, Barriers);

	nri::DispatchUpscaleDesc Desc = {};
	Desc.output = { Item.Textures[0], Item.Views[0] };
	Desc.input = { Item.Textures[1], Item.Views[1] };
	Desc.guides.upscaler.mv = { Item.Textures[2], Item.Views[2] };
	Desc.guides.upscaler.depth = { Item.Textures[3], Item.Views[3] };

	if (Key[0] != 4)
	{
		Desc.guides.upscaler.reactive = { Item.Textures[4], Item.Views[4] };
	}

	if (Key[0] == 3)
	{
		Desc.settings.fsr.zNear = Frame.Near;
		Desc.settings.fsr.zFar = Frame.Far;
		Desc.settings.fsr.verticalFov = Frame.Fov;
		Desc.settings.fsr.frameTime = Frame.Milliseconds;
		Desc.settings.fsr.viewSpaceToMetersFactor = 1.0f;
	}

	// A view smaller than the targets renders at their render size's share of it, rounded up to even pixels: a pixel
	// or two more than the upscaler made for that view takes
	Desc.currentResolution = { nri::Dim_t(std::min<u32>(Frame.Width, Props.renderResolution.w)), nri::Dim_t(std::min<u32>(Frame.Height, Props.renderResolution.h)) };
	Desc.cameraJitter = { Frame.JitterX, Frame.JitterY };
	Desc.mvScale = { 1.0f, 1.0f };
	Desc.flags = Frame.IsReset || IsHistoryLost ? nri::DispatchUpscaleBits::RESET_HISTORY : nri::DispatchUpscaleBits::NONE;
	IsHistoryLost = false;
	UpscalerApi.CmdDispatchUpscale(Commands, *Handle, Desc);

	std::swap(Barrier.before, Barrier.after);
	Api.CmdBarrier(Commands, Barriers);
	EndNriCommands(&Commands);
	return true;
}

void UpscaleNri::ReleaseFrames()
{
	if (!Handle)
	{
		return;
	}

	// The frames still on their way to the screen name the upscaler and what it was given
	GRHI->DevicePtr->Flush();

	for (Kept& Item : Frames)
	{
		Release(Item);
	}

	IsHistoryLost = true;
}

void UpscaleNri::Destroy()
{
	if (!Handle)
	{
		return;
	}

	ReleaseFrames();
	UpscalerApi.DestroyUpscaler(Handle);
	Handle = nullptr;
}

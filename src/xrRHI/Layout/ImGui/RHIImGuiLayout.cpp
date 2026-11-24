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
#include "../../RHI.h"
#include <imgui.h>

#ifdef IXR_WINDOWS
#	include "../../D3D11/Device.h"

#	include "imgui_impl_dx11.h"
#	include "imgui_impl_dx12.h"
#	include "../../D3D12/Device.h"

	ImGui_ImplDX11_Data* ImGui_ImplDX11_GetBackendData();

#	define DX11Device ((ID3D11Device*)GRHI->DevicePtr->RawDevice)
#	define DX11Context ((ID3D11DeviceContext*)GRHI->GetContext())

#endif

namespace
{
struct EngineImage
{
	IRHISurface* Surface;
	IRHIShaderResourceView* View;
	u32 Frame;
};

// Frames an engine image or texture is kept after its last use: the GPU may still read it
constexpr u32 ImageFrames = 4;
bool IsEngineTextures = false;
u32 ImageFrame = 0;
xr_vector<EngineImage> Images;

// A depth surface is read through its typed format
ERHI_FORMAT ReadFormat(ERHI_FORMAT Format)
{
	switch (Format)
	{
	case ERHI_FORMAT::R32G8X24_TYPELESS: return ERHI_FORMAT::R32_FLOAT_X8X24_TYPELESS;
	case ERHI_FORMAT::R32_TYPELESS: return ERHI_FORMAT::R32_FLOAT;
	case ERHI_FORMAT::R24G8_TYPELESS: return ERHI_FORMAT::R24_UNORM_X8_TYPELESS;
	case ERHI_FORMAT::R16_TYPELESS: return ERHI_FORMAT::R16_UNORM;
	default: return Format;
	}
}

void ReleaseImages(u32 Age)
{
	const auto Kept = std::remove_if(Images.begin(), Images.end(), [Age](const EngineImage& Item)
	{
		if (ImageFrame - Item.Frame < Age)
		{
			return false;
		}

		if (Item.View)
		{
			Item.View->Release();
		}

		Item.Surface->Release();
		return true;
	});

	Images.erase(Kept, Images.end());
}

void DestroyTexture(ImTextureData& Texture)
{
	if (Texture.BackendUserData)
	{
		static_cast<IRHISurface*>(Texture.BackendUserData)->Release();
	}

	Texture.SetTexID(ImTextureID_Invalid);
	Texture.SetStatus(ImTextureStatus_Destroyed);
	Texture.BackendUserData = nullptr;
}

RHISubResource TexturePixels(ImTextureData& Texture, u32 X, u32 Y, u32 Width, u32 Height)
{
	RHISubResource Pixels;
	Pixels.Data = Texture.GetPixelsAt(int(X), int(Y));
	Pixels.Width = Width;
	Pixels.Height = Height;
	Pixels.RowPitch = Pixels.DataSize = u32(Texture.GetPitch());
	Pixels.TextureFormat = ERHI_FORMAT::R8G8B8A8_UNORM;
	return Pixels;
}

void CreateTexture(ImTextureData& Texture)
{
	R_ASSERT2(Texture.Format == ImTextureFormat_RGBA32, "ImGui texture: only RGBA32 is made");
	RHITextureDesc Desc(u32(Texture.Width), u32(Texture.Height), ERHI_FORMAT::R8G8B8A8_UNORM);
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
	RHISubResource Pixels = TexturePixels(Texture, 0, 0, Desc.Width, Desc.Height);
	IRHISurface* Surface = GRHI->CreateTexture2D(Desc, Pixels);
	R_ASSERT2(Surface, "ImGui texture: not created");
	Texture.BackendUserData = Surface;
	Texture.SetTexID(RHIUtils::ImGui::Image(Surface));
	Texture.SetStatus(ImTextureStatus_OK);
}

void UpdateTexture(ImTextureData& Texture)
{
	IRHISurface* Surface = static_cast<IRHISurface*>(Texture.BackendUserData);

	for (const ImTextureRect& Rect : Texture.Updates)
	{
		const RHISubResource Pixels = TexturePixels(Texture, Rect.x, Rect.y, Rect.w, Rect.h);
		const RHIBox Box = { Rect.x, Rect.y, 0, u32(Rect.x + Rect.w), u32(Rect.y + Rect.h), 1 };
		Surface->UpdateData(0, 0, &Pixels, Box);
	}

	Texture.SetStatus(ImTextureStatus_OK);
}
} // namespace

#ifdef IXR_WINDOWS
// What the old backend binds for an ImTextureID: the view of an engine image, or a view of its own
static void* LegacyTexture(void* Id)
{
	IRHISurface* Surface = RHIUtils::ImGui::ImageSurface(Id);

	if (!Surface)
	{
		return Id;
	}

	IRHIShaderResourceView* View = RHIUtils::ImGui::ImageView(Surface);
	return View ? View->GetRawSRV() : nullptr;
}

static InternalDevice12& ImGuiDevice12()
{
    return *static_cast<InternalDevice12*>(GRHI->DevicePtr);
}

static void PrepareImGuiImages(ImDrawData* data)
{
    if (!data)
    {
        return;
    }
    // The engine has made and updated them for the main viewport
    if (IsEngineTextures)
    {
        data->Textures = nullptr;
    }
    if (data->Textures)
    {
        for (auto texture : *data->Textures)
        {
            if (texture->Status != ImTextureStatus_OK)
            {
                ImGui_ImplDX12_UpdateTexture(texture);
            }
        }
    }
    for (auto list : data->CmdLists)
    {
        for (const auto& command : list->CmdBuffer)
        {
            ImGuiDevice12().PrepareImage(u64(LegacyTexture(command.GetTexID())));
        }
    }
}

static void (*RenderImGuiWindow)(ImGuiViewport*, void*) = nullptr;

static void DrawImGuiWindow(ImGuiViewport* viewport, void* argument)
{
    auto& device = ImGuiDevice12();
    xrCriticalSectionGuard guard(device.ContextMutex());
    PrepareImGuiImages(viewport->DrawData);
    // Submitted, not waited for: the window's list runs after it on the same queue, before the frame's signal. The
    // Game window's OS window is drawn every frame, and a wait here would hold the frame until the GPU is idle
    device.SubmitExternalCommands(nullptr, 0);
    RenderImGuiWindow(viewport, argument);
}

static void InitImGui12()
{
    auto& device = ImGuiDevice12();
    xrCriticalSectionGuard guard(device.ContextMutex());
    ImGui_ImplDX12_InitInfo desc = {};
    desc.Device = device.GetDevice();
    desc.CommandQueue = device.GetQueue();
    desc.NumFramesInFlight = 3;
    desc.RTVFormat = DXGI_FORMAT_R8G8B8A8_UNORM;
    desc.SrvDescriptorHeap = device.GetResourceHeap();
    desc.UserData = &device;
    desc.ResolveTextureFn = [](ImGui_ImplDX12_InitInfo* desc, ImTextureID texture)
    {
        void* native = LegacyTexture(texture);
        return native ? ImTextureID(static_cast<InternalDevice12*>(desc->UserData)->ResolveImageHandle(u64(native))) : ImTextureID_Invalid;
    };
    desc.SrvDescriptorAllocFn = [](ImGui_ImplDX12_InitInfo* desc, D3D12_CPU_DESCRIPTOR_HANDLE* out_cpu,
        D3D12_GPU_DESCRIPTOR_HANDLE* out_gpu)
    {
        auto descriptor = static_cast<InternalDevice12*>(desc->UserData)->AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
        *out_cpu = static_cast<InternalDevice12*>(desc->UserData)->VisibleCPU(descriptor);
        *out_gpu = descriptor.Gpu;
    };
    desc.SrvDescriptorFreeFn = [](ImGui_ImplDX12_InitInfo* desc, D3D12_CPU_DESCRIPTOR_HANDLE cpu,
        D3D12_GPU_DESCRIPTOR_HANDLE gpu)
    {
        auto& device = *static_cast<InternalDevice12*>(desc->UserData);
        DX12Descriptor descriptor;
        descriptor.Cpu = cpu;
        descriptor.Gpu = gpu;
        descriptor.Index = u32((cpu.ptr - desc->SrvDescriptorHeap->GetCPUDescriptorHandleForHeapStart().ptr) /
            device.GetDevice()->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV));
        device.Retire(descriptor);
    };
    R_ASSERT(ImGui_ImplDX12_Init(&desc));
    RenderImGuiWindow = ::ImGui::GetPlatformIO().Renderer_RenderWindow;
    if (RenderImGuiWindow)
    {
        ::ImGui::GetPlatformIO().Renderer_RenderWindow = DrawImGuiWindow;
    }
}

static void DrawImGui12()
{
    auto data = ::ImGui::GetDrawData();
    auto& device = ImGuiDevice12();
    xrCriticalSectionGuard guard(device.ContextMutex());
    PrepareImGuiImages(data);
    device.PrepareImGuiTarget();
    ImGui_ImplDX12_RenderDrawData(data, device.Commands());
}

#endif

RHI_API void RHIUtils::ImGui::Init(bool EngineTextures)
{
	IsEngineTextures = EngineTextures;

	if (IsEngineTextures)
	{
		::ImGui::GetIO().BackendFlags |= ImGuiBackendFlags_RendererHasTextures;
	}

	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_Init(DX11Device, DX11Context); ImGui_ImplDX11_ResolveTexture = LegacyTexture; break;
        case ERHI_API_LAYER::D3D12: InitImGui12(); break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::NewFrame()
{
	++ImageFrame;
	ReleaseImages(ImageFrames);

	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_NewFrame();  break;
        case ERHI_API_LAYER::D3D12: ImGui_ImplDX12_NewFrame(); break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::DrawData()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_RenderDrawData(::ImGui::GetDrawData()); static_cast<InternalDevice11*>(GRHI->DevicePtr)->InvalidateBindings(); break;
        case ERHI_API_LAYER::D3D12: DrawImGui12(); break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::Destroy()
{
#ifdef IXR_WINDOWS
	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		ImGuiDevice12().Flush();
	}
#endif

	// Before the backends, which would take the engine's textures for their own
	if (IsEngineTextures)
	{
		for (ImTextureData* Texture : ::ImGui::GetPlatformIO().Textures)
		{
			DestroyTexture(*Texture);
		}
	}

	ReleaseImages(0);
	xr_vector<EngineImage>().swap(Images);

	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_Shutdown(); ImGui_ImplDX11_ResolveTexture = nullptr; break;
        case ERHI_API_LAYER::D3D12: ImGui_ImplDX12_Shutdown(); break;
#endif
	}

	if (IsEngineTextures)
	{
		::ImGui::GetIO().BackendFlags &= ~ImGuiBackendFlags_RendererHasTextures;
	}
}

RHI_API void RHIUtils::ImGui::Reset()
{
}

RHI_API void RHIUtils::ImGui::UpdateTextures()
{
	for (ImTextureData* Texture : ::ImGui::GetPlatformIO().Textures)
	{
		switch (Texture->Status)
		{
		case ImTextureStatus_WantCreate: CreateTexture(*Texture); break;
		case ImTextureStatus_WantUpdates: UpdateTexture(*Texture); break;
		case ImTextureStatus_WantDestroy: if (Texture->UnusedFrames >= int(ImageFrames)) { DestroyTexture(*Texture); } break;
		default: break;
		}
	}
}

RHI_API IRHIShaderResourceView* RHIUtils::ImGui::ImageView(IRHISurface* Surface)
{
	const auto Found = std::find_if(Images.begin(), Images.end(), [Surface](const EngineImage& Item) { return Item.Surface == Surface; });

	if (Found != Images.end())
	{
		Found->Frame = ImageFrame;
		return Found->View;
	}

	// Held, with its view or none, so that no other surface takes its address while it is drawn
	IRHIShaderResourceView* View = nullptr;

	if (Surface->GetTextureType() == ERHI_RESOURCE_DIMENSION::TEXTURE2D && Surface->GetArraySize() == 1 && Surface->GetSampleDescCount() == 1)
	{
		RHIShaderResourceViewDesc Desc;
		Desc.Format = ReadFormat(Surface->GetFormat());
		Desc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2D;
		Desc.MipLevels = Surface->GetMipLevels();
		View = GRHI->CreateShaderResourceView(Surface, &Desc);
	}

	Surface->AddRef();
	Images.push_back({ Surface, View, ImageFrame });
	return View;
}

RHI_API void* RHIUtils::ImGui::GetBlenderState()
{
#ifdef IXR_WINDOWS
    if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
    {
        RHIBlendDesc blend = {};
        for (auto& target : blend.RenderTarget)
        {
            target.BlendEnable = true;
            target.SrcBlend = RHI_BLEND_SRC_ALPHA;
            target.DestBlend = RHI_BLEND_INV_SRC_ALPHA;
            target.BlendOp = RHI_BLEND_OP_ADD;
            target.SrcBlendAlpha = RHI_BLEND_ONE;
            target.DestBlendAlpha = RHI_BLEND_INV_SRC_ALPHA;
            target.BlendOpAlpha = RHI_BLEND_OP_ADD;
            target.RenderTargetWriteMask = RHI_COLOR_WRITE_ENABLE_ALL;
        }
        return ImGuiDevice12().GetState(blend);
    }
	if (GRHI->APILevel == ERHI_API_LAYER::D3D11)
	{
		if (auto State = ImGui_ImplDX11_GetBackendData())
		{
			return State->pBlendState;
		}
	}
#endif

	return nullptr;
}

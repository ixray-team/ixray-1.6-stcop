#include "../../RHI.h"
#include <imgui.h>

#ifdef IXR_WINDOWS
#	include <d3d11.h>

#	include "imgui_impl_dx11.h"
#	include "imgui_impl_dx12.h"
#	include "../../D3D12/Device.h"

	ImGui_ImplDX11_Data* ImGui_ImplDX11_GetBackendData();

#	define DX11Device ((ID3D11Device*)GRHI->DevicePtr->RawDevice)
#	define DX11Context ((ID3D11DeviceContext*)GRHI->GetContext())

#endif

#ifdef IXR_WINDOWS
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
            ImGuiDevice12().PrepareImage(u64(command.GetTexID()));
        }
    }
}

static void (*RenderImGuiWindow)(ImGuiViewport*, void*) = nullptr;

static void DrawImGuiWindow(ImGuiViewport* viewport, void* argument)
{
    auto& device = ImGuiDevice12();
    xrCriticalSectionGuard guard(device.ContextMutex());
    PrepareImGuiImages(viewport->DrawData);
    device.Flush();
    RenderImGuiWindow(viewport, argument);
    device.FinishExternalWork();
}

static void InitImGui12()
{
    auto& device = ImGuiDevice12();
    xrCriticalSectionGuard guard(device.ContextMutex());
    ImGui_ImplDX12_InitInfo desc = {};
    desc.Device = device.GetDevice();
    desc.CommandQueue = device.GetQueue();
    desc.NumFramesInFlight = 3;
    desc.RTVFormat = DXGI_FORMAT_B8G8R8A8_UNORM;
    desc.SrvDescriptorHeap = device.GetResourceHeap();
    desc.UserData = &device;
    desc.ResolveTextureFn = [](ImGui_ImplDX12_InitInfo* desc, ImTextureID texture)
    {
        return ImTextureID(static_cast<InternalDevice12*>(desc->UserData)->ResolveImageHandle(u64(texture)));
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

RHI_API void RHIUtils::ImGui::Init()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_Init(DX11Device, DX11Context);  break;
        case ERHI_API_LAYER::D3D12: InitImGui12(); break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::NewFrame()
{
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
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_RenderDrawData(::ImGui::GetDrawData());  break;
        case ERHI_API_LAYER::D3D12: DrawImGui12(); break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::Destroy()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_Shutdown();  break;
        case ERHI_API_LAYER::D3D12: ImGuiDevice12().Flush(); ImGui_ImplDX12_Shutdown(); break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::Reset()
{
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
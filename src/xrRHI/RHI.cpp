#include "RHI.h"

#include "D3D11/Device.h"
#ifdef IXR_WINDOWS
#include "D3D12/Device.h"
#endif
#include "D3D11/DX11GPUEvents.h"
#include "D3D11/DX11ShaderDeclaration.h"
#include "D3D11/DX11ShaderResourceStateCache.h"
#include "D3D11/RHIStateManagerDX11.h"
#include "Drivers/AMDAntiLag.h"
#include "D3D11/RHIProfiler.h"

#include <DirectXMesh.h>

#ifdef IXR_WINDOWS
#	include "Drivers/AMDGPUTransferee.h"
#	include "Drivers/NvGPUTransferee.h"
#	include "Drivers/IntelGPUTransferee.h"
#endif

#include "Private/RHIRenderViewManager.h"

RHI_API u32 g_graphicsAPI = ERHI_API_LAYER::D3D11;
RHI_API u32 psCurrentVidMode[2] = { 1024,768 };
RHI_API Flags32 psDeviceFlags = { rsDetails | mtPhysics | mtSound | mtNetwork | rsDrawStatic | rsDrawDynamic | rsDeviceActive | mtParticles };
RHI_API Ivector2 HalfTarget = { 0, 0 };
RHI_API void* g_pAnnotation = nullptr;
RHI_API CRHI* GRHI = nullptr;

#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
	TracyD3D11Ctx g_tracyD3D11GPUContext = nullptr;
#endif

CRHI::CRHI()
{
}

CRHI::~CRHI()
{
	GRHIRenderViewManager.Clear();

	xr_delete(ShaderResourceCache);
	xr_delete(StateManager);
	xr_delete(DevicePtr);
	xr_delete(DriverExt);
	xr_delete(ShaderCompiler);
	xr_delete(DriverAntiLag);
}

IRHIDevice* CRHI::CreateDevice(ERHI_API_LAYER NewAPILevel)
{
#ifdef IXR_WINDOWS
	{
		PROF_EVENT("g_pGPU");
		DriverExt = new CNvReader();
		DriverExt->Initialize();
		if (!((CNvReader*)(DriverExt))->bSupport)
		{
			xr_delete(DriverExt);
			DriverExt = new CAMDReader;
			DriverExt->Initialize();

			if (!CAMDReader::bAMDSupportADL)
			{
				xr_delete(DriverExt);
				DriverExt = new CIntelReader;
			}
		}
	}
#endif

#if defined(IXR_LINUX) 
	setenv("DXVK_WSI_DRIVER", "SDL3", 1);
#endif

	APILevel = NewAPILevel;

	switch (NewAPILevel)
	{
		case ERHI_API_LAYER::D3D11:
		{
			DevicePtr = new InternalDevice11;
			ShaderResourceCache = new DX11ShaderResourceStateCache((ID3D11DeviceContext*)GetContext());
			StateManager = new RHIStateManagerDX11(static_cast<ID3D11DeviceContext*>(GetContext()));
			DriverAntiLag = new CAMDAntiLag();
#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
			g_tracyD3D11GPUContext = TracyD3D11Context((ID3D11Device*)DevicePtr->RawDevice, (ID3D11DeviceContext*)GetContext());
#endif
			break;
		}
        #ifdef IXR_WINDOWS
            case ERHI_API_LAYER::D3D12:
            {
                auto device = new InternalDevice12;
                DevicePtr = device;
                ShaderResourceCache = device->CreateResourceCache();
                StateManager = device->CreateStateManager();
                break;
            }
        #endif
        }

    R_ASSERT2(DevicePtr, "Unsupported graphics API");
    ShaderCompiler = new CRHIShaderCompilerShell(APILevel);

	return DevicePtr;
}

void CRHI::BeginFrame()
{
    if (DevicePtr)
    {
        DevicePtr->BeginFrame();
    }
	if (DriverAntiLag != nullptr)
	{
		DriverAntiLag->Update();
	}
}

void CRHI::ResizeBuffers(u32 Width, u32 Height)
{
	DevicePtr->ResizeBuffers(Width, Height);
}

void* CRHI::GetContext()
{
    return DevicePtr ? DevicePtr->GetContext() : nullptr;
}

void* CRHI::GetImmediateContext()
{
	return GetContext();
}

void* CRHI::CreateDeferredContext()
{
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->CreateDeferredContext();
	}

	VERIFY(!"Unsupported");
	return nullptr;
}

void CRHI::ReleaseDeferredContext(void* context)
{
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		((InternalDevice11*)DevicePtr)->ReleaseDeferredContext((ID3D11DeviceContext*)context);
		return;
	}

	VERIFY(!"Unsupported");
}

IRHIStateManager* CRHI::CreateStateManager(void* context)
{
#ifdef IXR_WINDOWS
    if (APILevel == ERHI_API_LAYER::D3D12)
    {
        R_ASSERT(!context || context == GetContext());
        return static_cast<InternalDevice12*>(DevicePtr)->CreateStateManager();
    }
#endif
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		auto* dxContext = context
			? static_cast<ID3D11DeviceContext*>(context)
			: static_cast<ID3D11DeviceContext*>(GetContext());

		return new RHIStateManagerDX11(dxContext);
	}

	VERIFY(!"Unsupported");
	return nullptr;
}

void CRHI::DestroyStateManager(IRHIStateManager* manager)
{
	xr_delete(manager);
}

void* CRHI::GetSwapchain()
{
    return DevicePtr ? DevicePtr->GetSwapchain() : nullptr;
}

void CRHI::ClearRawTarget(void* Target, ERTColor Transparent)
{
	DevicePtr->ClearTarget(Target, Transparent);
}

void CRHI::ClearTarget(IRHIRenderTargetView* Target, ERTColor Transparent)
{
	if (Target == nullptr)
	{
		return;
	}
	DevicePtr->ClearTarget(Target->GetRawRTV(), Transparent);
}

void CRHI::ClearTarget(IRHIRenderTargetView* Target, const float* Color)
{
	if (Target == nullptr)
	{
		return;
	}

	DevicePtr->ClearTarget(Target->GetRawRTV(), Color);
}

void CRHI::ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil)
{
	DevicePtr->ClearDepthStencil(View, TargetFlags, Depth, Stencil);
}

void CRHI::GenerateMips(IRHIShaderResourceView* SRV)
{
	DevicePtr->GenerateMips(SRV);
}

void CRHI::CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source)
{
	DevicePtr->CopySurface(Dest, Source);
}

void CRHI::CopySurface(IRHISurface* Dest, IRHISurface* Source)
{
	DevicePtr->CopySurface(Dest, Source);
}

void CRHI::Present()
{
	DevicePtr->Present();
}

xr_vector<shared_str> CRHI::DisplaySizeArray()
{
	auto IsValidAspectLambda = [](int W, int H, bool Check21x9)
	{
		float Aspect = float(W) / float(H);

		// 4:3
		if (fabs(Aspect - 4.0f / 3.0f) < 0.02f) return true;

		// 5:4
		if (fabs(Aspect - 5.0f / 4.0f) < 0.02f) return true;

		// 16:9
		if (fabs(Aspect - 16.0f / 9.0f) < 0.02f) return true;

		// 16:10
		if (fabs(Aspect - 16.0f / 10.0f) < 0.02f) return true;

		if (Check21x9 && fabs(Aspect - 21.0f / 3.0f) < 0.02f) return true;

		return false;
	};

    xr_vector<shared_str> Result = xr_vector<shared_str>();

    int NumDisplays = 0;
    SDL_DisplayID* Displays = SDL_GetDisplays(&NumDisplays);
    if (!Displays || NumDisplays == 0)
    {
        return Result;
    }

    xr_vector<Ivector2> Modes;

    for (int D = 0; D < NumDisplays; ++D)
    {
        SDL_DisplayID DisplayID = Displays[D];
		const SDL_DisplayMode* DesktopMode = SDL_GetDesktopDisplayMode(DisplayID);

		const float Aspect = float(DesktopMode->w) / float(DesktopMode->h);
		bool bSupports21by9 = fabs(Aspect - 21.0f / 9.0f) < 0.02f;

        if (DesktopMode == nullptr)
        {
            continue;
        }

        if (DesktopMode->w < 1024)
        {
            continue;
        }

        int NumModes = 0;
        SDL_DisplayMode** SdlModes = SDL_GetFullscreenDisplayModes(DisplayID, &NumModes);
        if (SdlModes && NumModes > 0)
        {
            for (int I = 0; I < NumModes; ++I)
            {
                const SDL_DisplayMode* ModePtr = SdlModes[I];

                if (ModePtr->w >= 1024 && ModePtr->w <= DesktopMode->w && ModePtr->h <= DesktopMode->h)
                {
					if (!IsValidAspectLambda(ModePtr->w, ModePtr->h, bSupports21by9))
					{
						continue;
					}

                    Modes.push_back({ ModePtr->w, ModePtr->h });
                }
            }
            SDL_free(SdlModes);
        }

        Modes.push_back({ DesktopMode->w, DesktopMode->h });
    }

    SDL_free(Displays);

    std::sort(Modes.begin(), Modes.end(), [](const Ivector2& A, const Ivector2& B)
    {
        if (A.x == B.x)
        {
            return A.y < B.y;
        }
        return A.x < B.x;
    });

    Modes.erase(std::unique(Modes.begin(), Modes.end(), [](const Ivector2& A, const Ivector2& B)
    {
        return A.x == B.x && A.y == B.y;
    }), Modes.end());

    for (const Ivector2& M : Modes)
    {
        string32 Str;
        xr_sprintf(Str, sizeof(Str), "%dx%d", M.x, M.y);
        Result.push_back(Str);
    }

	std::reverse(Result.begin(), Result.end());

    return Result;
}

IRHISurface* CRHI::CreateTexture3D(const RHITextureDesc& Desc, RHISubResource* SubResource)
{
	return DevicePtr->GetTextureFactory()->CreateTexture3D(Desc, SubResource);
}

IRHISurface* CRHI::CreateTexture2D(const RHITextureDesc& Desc, RHISubResource& SubResource)
{
	return DevicePtr->GetTextureFactory()->CreateTexture2D(Desc, &SubResource);
}

IRHISurface* CRHI::CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc)
{
	return DevicePtr->GetTextureFactory()->CreateTextureFromMemory(data, size, desc);
}

IRHISurface* CRHI::CreateRenderTarget(const RHITextureDesc& desc)
{
	return DevicePtr->GetTextureFactory()->CreateRenderTarget(desc);
}

IRHISurface* CRHI::CreateDepthStencil(const RHITextureDesc& desc)
{
	return DevicePtr->GetTextureFactory()->CreateDepthStencil(desc);
}

IRHIShaderResourceView* CRHI::CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc)
{
    return DevicePtr->CreateShaderResourceView(Buffer, desc);
}

IRHIShaderResourceView* CRHI::CreateShaderResourceView(IRHISurface* Surface, const RHIShaderResourceViewDesc* desc)
{
	return DevicePtr->GetTextureFactory()->CreateShaderResourceView(Surface, desc);
}

IRHIRenderTargetView* CRHI::CreateRenderTargetView(IRHISurface* Surface, const RHIRenderTargetViewDesc& desc)
{
	return DevicePtr->GetTextureFactory()->CreateRenderTargetView(Surface, desc);
}

IRHIDepthStencilView* CRHI::CreateDepthStencilView(IRHISurface* Surface, const RHIDepthStencilViewDesc& desc)
{
	return DevicePtr->GetTextureFactory()->CreateDepthStencilView(Surface, desc);
}

IRHIUnorderedAccessView* CRHI::CreateUAV(IRHISurface* Surface, const RHIUAVDesc& desc)
{
	return DevicePtr->GetTextureFactory()->CreateUAV(Surface, desc);
}

IRHIBuffer* CRHI::CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource)
{
	return DevicePtr->CreateBuffer(desc, pSubresource);
}

IRHIShaderDeclaration* CRHI::CreateDecl(const RHIInputElementDesc* Desc, size_t DeclSize)
{
    return DevicePtr->CreateDecl(Desc, DeclSize);
}

void CRHI::SetConstantBuffers(u32 Start, u32 Count, IRHIBuffer* const* Buffers, ERHI_SHADER_TYPE Type)
{
    DevicePtr->SetConstantBuffers(Start, Count, Buffers, Type);
}

void CRHI::GPUStatsBegin() const
{
	if (!GPUStatsEnable)
	{
		return;
	}

#ifdef IXR_WINDOWS
    if (APILevel == ERHI_API_LAYER::D3D12)
    {
        static_cast<InternalDevice12*>(DevicePtr)->BeginGPUStats();
    }
#endif

	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
		GPUEvents_BeginRendering();
#endif
	}
}

const RHI_GPU_EVENT& CRHI::GPUStats() const
{
	static RHI_GPU_EVENT DummyEvents = {};
	if (!GPUStatsEnable)
	{
		return DummyEvents;
	}

#ifdef IXR_WINDOWS
    if (APILevel == ERHI_API_LAYER::D3D12)
    {
        return static_cast<InternalDevice12*>(DevicePtr)->GPUStats();
    }
#endif

	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
		return GPUEvents_Statistics();
#endif
	}

	return DummyEvents;
}

void CRHI::GPUStatsEnd() const
{
#ifdef IXR_WINDOWS
    if (APILevel == ERHI_API_LAYER::D3D12)
    {
        static_cast<InternalDevice12*>(DevicePtr)->EndGPUStats();
        return;
    }
#endif

	if (!GPUStatsEnable)
	{
		return;
	}

	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
		GPUEvents_EndRendering();
#endif
	}
}

void CRHI::ClearVertexBuffer(u32 vb_stride)
{
    DevicePtr->ClearVertexBuffer(vb_stride);
}

void CRHI::SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology)
{
	DevicePtr->SetPrimitiveTopology(topology);
}

void CRHI::Draw(u32 startVertex, u32 primitiveCount)
{
	DevicePtr->Draw(startVertex, primitiveCount);
}

void CRHI::DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount)
{
	DevicePtr->DrawIndexed(baseVertex, startVertex, vertexCount, startIndex, primitiveCount);
}

void CRHI::DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation)
{
	DevicePtr->DrawIndexedInstanced(baseVertex, startVertex, vertexCount, startIndex, primitiveCount, instanceCount, startInstanceLocation);
}

void CRHI::DrawNoInputAssembly(u32 vertexCount)
{
	DevicePtr->DrawNoInputAssembly(vertexCount);
}

void CRHI::ClearIndexBuffer()
{
    DevicePtr->ClearIndexBuffer();
}

bool CRHI::IsTessPass() const
{
	return Shaders[(size_t)ERHI_SHADER_TYPE::HS] || Shaders[(size_t)ERHI_SHADER_TYPE::DS];
}

#ifndef D3D11_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT
#	define D3D11_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT 32
#endif

u32 CRHI::GetInputElementDescStride(const RHIInputElementDesc* Desc, u32 DescSize)
{
#ifdef IXR_WINDOWS
	if (DevicePtr)
	{
		u32 Offsets[D3D11_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT] = {};
		u32 Strides[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT] = {};

		DirectX::ComputeInputLayout((D3D11_INPUT_ELEMENT_DESC*)Desc, DescSize, Offsets, Strides);
		return Strides[0];
	}
	else
#endif
	{
		VERIFY(!"Implement me!");
	}

	return u32(-1);
}

HRESULT CRHI::ReplaceShader(RHIObject* shader, const void* code, size_t size)
{
	return DevicePtr->ReplaceShader(shader, code, size);
}

HRESULT CRHI::BuildShader(const char* shader_dir, const void* srcData, size_t srcSize, const char* sourceName, const void* defines, void* include, const char* entryPoint, const char* target, u32 flags1, u32 flags2, xr_vector<u8>& code, xr_vector<u8>& errors)
{
	return ShaderCompiler->Build(shader_dir, srcData, srcSize, sourceName, defines, include, entryPoint, target, flags1, flags2, code, errors);
}

void CRHI::EvictManagedResources()
{
	DevicePtr->EvictManagedResources();
}

void CRHI::SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type)
{
    void* nativeShader = shader ? shader->resource : nullptr;
    if (APILevel != ERHI_API_LAYER::D3D12 && Shaders[(size_t)Type] == nativeShader)
    {
        return;
    }
    DevicePtr->SetShader(shader, Type);
    Shaders[(size_t)Type] = nativeShader;
}

void CRHI::SetViewport(RHIViewport& VP)
{
	DevicePtr->SetViewport(VP);
}

void CRHI::SetScissorRect(Irect* R)
{
	GRHI->StateManager->EnableScissoring(R != nullptr);
	DevicePtr->SetScissorRect(R);
}

void CRHI::SetUnorderedAccessViews(IRHIUnorderedAccessView* View, u32 ID, bool bForce)
{
	GRHIRenderViewManager.SetUnorderedAccessViews(View, ID, bForce);
}

void CRHI::SetRenderTargetView(IRHIRenderTargetView* pRenderTargetView, u32 ID, bool bForce)
{
	GRHIRenderViewManager.SetRenderTargetView(pRenderTargetView, ID, bForce);
}

void CRHI::SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView, bool bForce)
{
	GRHIRenderViewManager.SetDepthStencilView(pDepthStencilView, bForce);
}

void CRHI::ApplyRenderTargetChange()
{
	GRHIRenderViewManager.ApplyRenderTargetChange();
}

IRHIDepthStencilView* CRHI::GetDepthStencilView() const
{
	return GRHIRenderViewManager.DepthStencilView;
}

IRHIRenderTargetView* CRHI::GetRenderTargetView(size_t ID) const
{
	return GRHIRenderViewManager.RenderTargetViews[ID];
}
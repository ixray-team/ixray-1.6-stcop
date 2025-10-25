#include "RHI.h"

#ifdef IXR_WINDOWS
#include "D3D9/Device.h"
#include "D3D9/DX9ShaderDeclaration.h"
#include "D3D9/DX9ShaderResourceStateCache.h"

#include "D3D11/Device.h"
#include "D3D11/DX11GPUEvents.h"
#include "D3D11/DX11ShaderDeclaration.h"
#include "D3D11/DX11ShaderResourceStateCache.h"

#include <DirectXMesh.h>
#include "Drivers/AMDGPUTransferee.h"
#include "Drivers/NvGPUTransferee.h"
#endif

RHI_API u32 psCurrentVidMode[2] = { 1024,768 };
RHI_API Flags32 psDeviceFlags = { rsDetails | mtPhysics | mtSound | mtNetwork | rsDrawStatic | rsDrawDynamic | rsDeviceActive | mtParticles };
RHI_API Ivector2 HalfTarget = { 0, 0 };
RHI_API void* g_pAnnotation = nullptr;
RHI_API CRHI* GRHI = nullptr;

CRHI::CRHI() :
	m_pDepthStencilView(nullptr),
	m_bChangedRTorZB(false)
{
	memset(m_pRenderTargetViews, 0, sizeof(m_pRenderTargetViews));
}

CRHI::~CRHI()
{
	xr_delete(DevicePtr);
	xr_delete(ShaderResourceCache);
	xr_delete(DriverExt);
	xr_delete(ShaderCompiler);
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
		}
	}
#endif

	APILevel = NewAPILevel;

	switch (NewAPILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D9:  DevicePtr = new InternalDevice9;  ShaderResourceCache = new DX9ShaderResourceStateCache; break;
		case ERHI_API_LAYER::D3D11: DevicePtr = new InternalDevice11; ShaderResourceCache = new DX11ShaderResourceStateCache((ID3D11DeviceContext*)GetContext()); break;
#endif
	}

	ShaderCompiler = new CRHIShaderCompilerShell(APILevel);

	return DevicePtr;
}

void CRHI::ResizeBuffers(u32 Width, u32 Height)
{
	DevicePtr->ResizeBuffers(Width, Height);
}

void* CRHI::GetContext()
{
	if (APILevel == ERHI_API_LAYER::NOT_CREATED)
	{
		return nullptr;
	}
#ifdef IXR_WINDOWS
	else if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->HWRenderContext;
	}
#endif

	VERIFY(!"Unsupported");
	return nullptr;
}

void* CRHI::GetSwapchain()
{
	if (APILevel == ERHI_API_LAYER::NOT_CREATED)
	{
		return nullptr;
	}
#ifdef IXR_WINDOWS
	else  if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->HWSwapchain;
	}
#endif

	VERIFY(!"Unsupported");
	return nullptr;
}

void CRHI::ClearRawTarget(void* Target, ERTColor Transparent)
{
	DevicePtr->ClearTarget(Target, Transparent);
}

void CRHI::ClearTarget(IRHIRenderTargetView* Target, ERTColor Transparent)
{
	DevicePtr->ClearTarget(Target->GetRawRTV(), Transparent);
}

void CRHI::ClearTarget(IRHIRenderTargetView* Target, const float* Color)
{
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

    struct Mode
    {
        int W;
        int H;
    };
    xr_vector<Mode> Modes = xr_vector<Mode>();

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

    std::sort(Modes.begin(), Modes.end(), [](const Mode& A, const Mode& B)
    {
        if (A.W == B.W)
        {
            return A.H < B.H;
        }
        return A.W < B.W;
    });

    Modes.erase(std::unique(Modes.begin(), Modes.end(), [](const Mode& A, const Mode& B)
    {
        return A.W == B.W && A.H == B.H;
    }), Modes.end());

    for (const Mode& M : Modes)
    {
        string32 Str;
        xr_sprintf(Str, sizeof(Str), "%dx%d", M.W, M.H);
        Result.push_back(Str);
    }

	std::reverse(Result.begin(), Result.end());

    return Result;
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
#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		ID3D11Device* DxDevice = (ID3D11Device*)((InternalDevice11*)DevicePtr)->RawDevice;
		D3D11_SHADER_RESOURCE_VIEW_DESC Desc = {};

		Desc.Format = (DXGI_FORMAT)desc->Format;
		Desc.Buffer.ElementWidth = desc->ElementWidth;
		Desc.ViewDimension = D3D11_SRV_DIMENSION_BUFFER;

		ID3D11ShaderResourceView* srv = nullptr;
		R_CHK(DxDevice->CreateShaderResourceView(((CD3D11Buffer*)Buffer)->GetD3DObject(), &Desc, &srv));

		return new DX11ShaderResourceView(srv, nullptr);
	}
	else
#endif
	{
		VERIFY(!"Unsupported");
		return nullptr;
	}
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
	IRHIShaderDeclaration* Decl = nullptr;

#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		Decl = new DX11ShaderDeclaration(Desc, DeclSize);
	}
	else
	{
		Decl = new DX9ShaderDeclaration(Desc, DeclSize);
	}
#endif
	return Decl;
}

void CRHI::SetConstantBuffers(u32 Min, u32 Max, xr_vector<IRHIBuffer*> Buffers, ERHI_SHADER_TYPE Type)
{
#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		ID3D11DeviceContext* Context = (ID3D11DeviceContext*)GetContext();

		xr_vector<ID3D11Buffer*> DXBuffer;
		DXBuffer.resize(Buffers.size());

		u32 Iter = 0;
		for (IRHIBuffer* Buffer : Buffers)
		{
			if (Min > Iter)
			{
				continue;
			}

			if (Buffer == nullptr)
			{
				break;
			}

			DXBuffer[Iter] = (((CD3D11Buffer*)Buffer)->GetD3DObject());
			Iter++;
		}

		switch (Type)
		{
			case ERHI_SHADER_TYPE::PS: Context->PSSetConstantBuffers(Min, Max, DXBuffer.data()); break;
			case ERHI_SHADER_TYPE::VS: Context->VSSetConstantBuffers(Min, Max, DXBuffer.data()); break;
			case ERHI_SHADER_TYPE::GS: Context->GSSetConstantBuffers(Min, Max, DXBuffer.data()); break;
			case ERHI_SHADER_TYPE::HS: Context->HSSetConstantBuffers(Min, Max, DXBuffer.data()); break;
			case ERHI_SHADER_TYPE::DS: Context->DSSetConstantBuffers(Min, Max, DXBuffer.data()); break;
			case ERHI_SHADER_TYPE::CS: Context->CSSetConstantBuffers(Min, Max, DXBuffer.data()); break;
		}
	}
#endif
}

void CRHI::GPUStatsBegin() const
{
	if (!GPUStatsEnable)
	{
		return;
	}

#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#ifdef DEBUG_DRAW
		GPUEvents_BeginRendering();
#endif
	}
#endif
}

const RHI_GPU_EVENT& CRHI::GPUStats() const
{
	static RHI_GPU_EVENT DummyEvents = {};
	if (!GPUStatsEnable)
	{
		return DummyEvents;
	}

#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#ifdef DEBUG_DRAW
		return GPUEvents_Statistics();
#endif
	}
#endif

	return DummyEvents;
}

void CRHI::GPUStatsEnd() const
{
	if (!GPUStatsEnable)
	{
		return;
	}

#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#ifdef DEBUG_DRAW
		GPUEvents_EndRendering();
#endif
	}
#endif
}

void CRHI::ClearVertexBuffer(u32 vb_stride)
{
#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		ID3D11DeviceContext* Context = (ID3D11DeviceContext*)GetContext();

		u32	iOffset = 0;
		Context->IASetVertexBuffers(0, 1, nullptr, &vb_stride, &iOffset);
	}
	else
	{
		InternalDevice9* Device = (InternalDevice9*)DevicePtr;
		IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)DevicePtr->RawDevice;

		CHK_DX(DxDevice->SetStreamSource(0, nullptr, 0, vb_stride));
	}
#endif
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
#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		ID3D11DeviceContext* Context = (ID3D11DeviceContext*)GetContext();
		Context->IASetIndexBuffer(nullptr, DXGI_FORMAT_R16_UINT, 0);
	}
	else
	{
		InternalDevice9* Device = (InternalDevice9*)DevicePtr;
		IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)DevicePtr->RawDevice;

		CHK_DX(DxDevice->SetIndices(nullptr));
	}
#endif
}

bool CRHI::IsTessPass() const
{
	if (APILevel == ERHI_API_LAYER::D3D9)
	{
		return false;
	}

	return Shaders[(size_t)ERHI_SHADER_TYPE::HS] || Shaders[(size_t)ERHI_SHADER_TYPE::DS];
}

u32 CRHI::GetInputElementDescStride(const RHIInputElementDesc* Desc, u32 DescSize)
{
#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
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

HRESULT CRHI::BuildShader(const void* srcData, size_t srcSize, const char* sourceName, const void* defines, void* include, const char* entryPoint, const char* target, u32 flags1, u32 flags2, void** code, void** errors)
{
	return ShaderCompiler->Build(srcData, srcSize, sourceName, defines, include, entryPoint, target, flags1, flags2, code, errors);
}

void CRHI::SetShader(void* NativeShader, ERHI_SHADER_TYPE Type)
{
	if (Shaders[(size_t)Type] == NativeShader)
	{
		return;
	}

#ifdef IXR_WINDOWS
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		ID3D11DeviceContext* Context = (ID3D11DeviceContext*)GetContext();

		switch (Type)
		{
			case ERHI_SHADER_TYPE::PS: Context->PSSetShader((ID3D11PixelShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::VS: Context->VSSetShader((ID3D11VertexShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::GS: Context->GSSetShader((ID3D11GeometryShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::HS: Context->HSSetShader((ID3D11HullShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::DS: Context->DSSetShader((ID3D11DomainShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::CS: Context->CSSetShader((ID3D11ComputeShader*)NativeShader, nullptr, 0); break;
			default: break;
		}
	}
	else if (APILevel == ERHI_API_LAYER::D3D9)
	{
		IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)DevicePtr->RawDevice;

		switch (Type)
		{
			case ERHI_SHADER_TYPE::PS: CHK_DX(DxDevice->SetPixelShader((IDirect3DPixelShader9*)NativeShader)); break;
			case ERHI_SHADER_TYPE::VS: CHK_DX(DxDevice->SetVertexShader((IDirect3DVertexShader9*)NativeShader)); break;
			default: break; // DX9 supports only VS and PS in this context
		}
	}
#endif

	Shaders[(size_t)Type] = NativeShader;
}

void CRHI::SetViewport(RHIViewport& VP)
{
	DevicePtr->SetViewport(VP);
}

void CRHI::SetRenderTargetView(IRHIRenderTargetView* pRenderTargetView, u32 ID, bool bForce)
{
	if (m_pRenderTargetViews[ID] != pRenderTargetView)
	{
		m_pRenderTargetViews[ID] = pRenderTargetView;
		m_bChangedRTorZB = true;
	}

	if (bForce)
		ApplyRenderTargetChange();
}

void CRHI::SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView, bool bForce)
{
	if (m_pDepthStencilView != pDepthStencilView)
	{
		m_pDepthStencilView = pDepthStencilView;
		m_bChangedRTorZB = true;
	}

	if (bForce)
		ApplyRenderTargetChange();
}

void CRHI::ApplyRenderTargetChange()
{
	DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, m_pRenderTargetViews, m_pDepthStencilView);
	
	m_bChangedRTorZB = false;
}

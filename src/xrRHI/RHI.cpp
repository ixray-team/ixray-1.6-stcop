#include "RHI.h"

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

RHI_API u32 psCurrentVidMode[2] = { 1024,768 };
RHI_API Flags32 psDeviceFlags = { rsDetails | mtPhysics | mtSound | mtNetwork | rsDrawStatic | rsDrawDynamic | rsDeviceActive | mtParticles };
RHI_API Ivector2 HalfTarget = { 0, 0 };
RHI_API void* g_pAnnotation = nullptr;
RHI_API CRHI* GRHI = nullptr;

CRHI::~CRHI()
{
	xr_delete(DevicePtr);
	xr_delete(ShaderResourceCache);
	xr_delete(DriverExt);
}

IRHIDevice* CRHI::CreateDevice(ERHI_API_LAYER NewAPILevel)
{
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

	APILevel = NewAPILevel;

	switch (NewAPILevel)
	{
		case ERHI_API_LAYER::D3D9:  DevicePtr = new InternalDevice9;  ShaderResourceCache = new DX9ShaderResourceStateCache; break;
		case ERHI_API_LAYER::D3D11: DevicePtr = new InternalDevice11; ShaderResourceCache = new DX11ShaderResourceStateCache((ID3D11DeviceContext*)GetContext()); break;
	}

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
	else if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->HWRenderContext;
	}

	VERIFY(!"Unsupported");
	return nullptr;
}

void* CRHI::GetSwapchain()
{
	if (APILevel == ERHI_API_LAYER::NOT_CREATED)
	{
		return nullptr;
	}
	else  if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->HWSwapchain;
	}

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

void CRHI::ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil)
{
	DevicePtr->ClearDepthStencil(View, TargetFlags, Depth, Stencil);
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

struct _uniq_mode
{
	_uniq_mode(shared_str v) :_val(v) {}
	shared_str _val;
	bool operator() (shared_str _other) { return _val == _other; }
};

bool sort_vid_mode(const DXGI_MODE_DESC& left, const DXGI_MODE_DESC& right)
{
	auto leftString = xr_string::ToString(left.Width) + xr_string::ToString(left.Height);
	auto rightString = xr_string::ToString(right.Width) + xr_string::ToString(right.Height);

	if (leftString.length() == rightString.length())
	{
		if (left.Width > right.Width)
		{
			return true;
		}
		else if (left.Width == right.Width)
		{
			return left.Height > right.Height;
		}

		return false;
	}

	return leftString.length() > rightString.length();
}

xr_vector<shared_str> CRHI::DisplaySizeArray()
{
	xr_vector<shared_str> _tmp;
	xr_vector<DXGI_MODE_DESC> modes;

	IDXGIOutput* pOutput = nullptr;
	IDXGIAdapter* pAdapter = nullptr;
	IDXGIFactory* pFactory = nullptr;
	R_CHK(CreateDXGIFactory(IID_PPV_ARGS(&pFactory)));
	pFactory->EnumAdapters(0, &pAdapter);
	pAdapter->EnumOutputs(0, &pOutput);
	pAdapter->Release();
	pFactory->Release();
	VERIFY(pOutput);

	UINT num = 0;
	DXGI_FORMAT format = DXGI_FORMAT_R8G8B8A8_UNORM;
	UINT flags = 0;

	// Get the number of display modes available
	pOutput->GetDisplayModeList(format, flags, &num, 0);

	// Get the list of display modes
	modes.resize(num);
	pOutput->GetDisplayModeList(format, flags, &num, &modes.front());

	pOutput->Release();

	std::sort(modes.begin(), modes.end(), sort_vid_mode);

	for (u32 i = 0; i < num; ++i)
	{
		DXGI_MODE_DESC& desc = modes[i];
		string32 str;

		if (desc.Width < 1024)
		{
			continue;
		}

		xr_sprintf(str, sizeof(str), "%dx%d", desc.Width, desc.Height);

		if (_tmp.end() != std::find_if(_tmp.begin(), _tmp.end(), _uniq_mode(str)))
		{
			continue;
		}

		_tmp.push_back(nullptr);
		_tmp.back() = str;
	}

	return std::move(_tmp);
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

	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		Decl = new DX11ShaderDeclaration(Desc, DeclSize);
	}
	else
	{
		Decl = new DX9ShaderDeclaration(Desc, DeclSize);
	}

	return Decl;
}

void CRHI::SetConstantBuffers(u32 Min, u32 Max, xr_vector<IRHIBuffer*> Buffers, ERHI_SHADER_TYPE Type)
{
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
#endif
#ifdef DEBUG_DRAW
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
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
#endif
#ifdef DEBUG_DRAW
		return GPUEvents_Statistics();
#endif
	}

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
#endif
#ifdef DEBUG_DRAW
		GPUEvents_EndRendering();
#endif
	}
}

void CRHI::ClearVertexBuffer(u32 vb_stride)
{
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
}

void CRHI::ClearIndexBuffer()
{
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
	if (APILevel == ERHI_API_LAYER::D3D11)
	{
		u32 Offsets[D3D11_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT] = {};
		u32 Strides[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT] = {};

		DirectX::ComputeInputLayout((D3D11_INPUT_ELEMENT_DESC*)Desc, DescSize, Offsets, Strides);
		return Strides[0];
	}
	else
	{
		VERIFY(!"Implement me!");
	}

	return u32(-1);
}

void CRHI::SetShader(void* NativeShader, ERHI_SHADER_TYPE Type)
{
	if (Shaders[(size_t)Type] == NativeShader)
	{
		return;
	}

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

	Shaders[(size_t)Type] = NativeShader;
}

void CRHI::SetViewport(RHIViewport& VP)
{
	DevicePtr->SetViewport(VP);
}
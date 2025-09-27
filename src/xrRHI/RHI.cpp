#include "RHI.h"

#include "D3D9/Device.h"

#include "D3D11/Device.h"
#include "D3D11/DX11GPUEvents.h"

RHI_API u32 psCurrentVidMode[2] = { 1024,768 };
RHI_API Flags32 psDeviceFlags = { rsDetails | mtPhysics | mtSound | mtNetwork | rsDrawStatic | rsDrawDynamic | rsDeviceActive | mtParticles };
RHI_API Ivector2 HalfTarget = { 0, 0 };
RHI_API void* g_pAnnotation = nullptr;
RHI_API CRHI* GRHI = nullptr;

CRHI::~CRHI()
{
	xr_delete(DevicePtr);
}

IRHIDevice* CRHI::CreateDevice(ERHI_API_LAYER NewAPILevel)
{
	switch (NewAPILevel)
	{
		case ERHI_API_LAYER::D3D9:  DevicePtr = new InternalDevice9;  break;
		case ERHI_API_LAYER::D3D11: DevicePtr = new InternalDevice11; break;
	}

	APILevel = NewAPILevel;

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

IRHISurface* CRHI::CreateTextureFromFile(const char* filename, u32& memorySize)
{
	return DevicePtr->GetTextureFactory()->CreateTextureFromFile(filename, memorySize);
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

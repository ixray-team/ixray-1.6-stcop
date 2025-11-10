#include "Device.h"
#include "../Drivers/AMDGPUTransferee.h"
#include "../RHITopologyUtils.h"

#define DX11Device ((ID3D11Device*)RawDevice)

InternalDevice11::InternalDevice11()
{
	CreateD3D11();
}

InternalDevice11::~InternalDevice11()
{
	DestroyD3D11();
}

IRHITextureFactory* InternalDevice11::GetTextureFactory()
{
	return TextureFactory;
}

void InternalDevice11::SetTextureFactory(IRHITextureFactory* factory)
{
	if (TextureFactory)
	{
		delete TextureFactory;
	}
	TextureFactory = static_cast<DX11TextureFactory*>(factory);
	TextureFactory = TextureFactory;
}

bool InternalDevice11::UpdateBuffersD3D11()
{
	HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
	// Create a render target view
	ID3D11Texture2D* pBuffer = nullptr;
	HRESULT R = HWSwapchain->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
	R_CHK(R);

	if (pBuffer == nullptr)
	{
		return false;
	}

	ID3D11RenderTargetView* SwapChainRaw = nullptr;
	R = (DX11Device)->CreateRenderTargetView(pBuffer, nullptr, &SwapChainRaw);
	pBuffer->Release();
	R_CHK(R);

	DXGI_SWAP_CHAIN_DESC sd = {};
	sd.BufferDesc.Width = psCurrentVidMode[0];
	sd.BufferDesc.Height = psCurrentVidMode[1];
	sd.BufferDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	sd.BufferCount = 1;
	sd.SampleDesc.Count = 1;
	sd.SampleDesc.Quality = 0;
	sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
	sd.OutputWindow = hwnd;
	sd.Windowed = !psDeviceFlags.is(rsFullscreen);

	sd.BufferDesc.RefreshRate.Numerator = 0;
	sd.BufferDesc.RefreshRate.Denominator = 0;

	//	Additional set up
	sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;

	//	Create Depth/stencil buffer
	ID3D11Texture2D* pDepthStencil = nullptr;
	D3D11_TEXTURE2D_DESC descDepth = {};
	descDepth.Width = sd.BufferDesc.Width;			// TODO: render scale
	descDepth.Height = sd.BufferDesc.Height;		// TODO: render scale
	descDepth.MipLevels = 1;
	descDepth.ArraySize = 1;
	descDepth.SampleDesc.Count = 1;
	descDepth.SampleDesc.Quality = 0;
	descDepth.Usage = D3D11_USAGE_DEFAULT;
	descDepth.CPUAccessFlags = 0;
	descDepth.MiscFlags = 0;

	descDepth.Format = DXGI_FORMAT_B8G8R8X8_UNORM;
	descDepth.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
	R = (DX11Device)->CreateTexture2D(&descDepth, nullptr, (ID3D11Texture2D**)&RenderTexture);
	R_CHK(R);

	if (RenderTexture == nullptr)
	{
		return false;
	}

	ID3D11RenderTargetView* RawSRV = nullptr;
	R = DX11Device->CreateRenderTargetView((ID3D11Resource*)RenderTexture, nullptr, &RawSRV);
	R_CHK(R);

	R = DX11Device->CreateShaderResourceView((ID3D11Resource*)RenderTexture, nullptr, (ID3D11ShaderResourceView**)&RenderSRV);
	R_CHK(R);

	descDepth.Width = UINT(sd.BufferDesc.Width * RenderScale);
	descDepth.Height = UINT(sd.BufferDesc.Height * RenderScale);

	descDepth.Width += descDepth.Width % 2;
	descDepth.Height += descDepth.Height % 2;

	HalfTarget.x = descDepth.Width;
	HalfTarget.y = descDepth.Height;

	descDepth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
	descDepth.BindFlags = D3D11_BIND_DEPTH_STENCIL;
	R = DX11Device->CreateTexture2D(&descDepth, nullptr, &pDepthStencil);
	R_CHK(R);

	if (pDepthStencil == nullptr)
	{
		return false;
	}

	//	Create Depth/stencil view
	ID3D11DepthStencilView* Dsv = nullptr;
	D3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc = {};
	depthStencilViewDesc.Format = descDepth.Format;
	depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
	depthStencilViewDesc.Texture2D.MipSlice = 0;
	R = DX11Device->CreateDepthStencilView(pDepthStencil, &depthStencilViewDesc, (ID3D11DepthStencilView**)&Dsv);
	R_CHK(R);

	RenderDSV = new DX11DepthStencilView(Dsv, new DX11Surface(pDepthStencil));
	SwapChainRTV = new DX11RenderTargetView(SwapChainRaw, new DX11Surface(((ID3D11Texture2D*)pBuffer)));
	RenderRTV = new DX11RenderTargetView(RawSRV, new DX11Surface(((ID3D11Texture2D*)RenderTexture)));

	pDepthStencil->Release();
	return true;
}

IRHIBuffer* InternalDevice11::CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource)
{
	CD3D11Buffer* pBuffer = new CD3D11Buffer();
	pBuffer->Create(desc, pSubresource);
	return pBuffer;
}

void InternalDevice11::SetDSV(IRHIDepthStencilView* pDepthStencilView)
{
	DepthStencilView = pDepthStencilView;
}

void InternalDevice11::SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews)
{
	static ID3D11RenderTargetView* s_RenderTargetView11[RHI_MAX_RENDER_TARGETS];

	R_ASSERT(NumViews >= RHI_MAX_RENDER_TARGETS);

	for (int i = 0; i < NumViews; i++)
	{
		s_RenderTargetView11[i] = reinterpret_cast<ID3D11RenderTargetView*>(ppRenderTargetViews[i] ? ppRenderTargetViews[i]->GetRawRTV() : nullptr);
	}

	HWRenderContext->OMSetRenderTargets(NumViews, s_RenderTargetView11, DepthStencilView ? (ID3D11DepthStencilView*)DepthStencilView->GetRawDSV() : nullptr);
}

#if 0
void CreateRDoc()
{
	if (Core.ParamsData.test(ECoreParams::renderdoc))
	{
		if (HMODULE mod = LoadLibraryA("renderdoc.dll"))
		{
			pRENDERDOC_GetAPI RENDERDOC_GetAPI = (pRENDERDOC_GetAPI)GetProcAddress(mod, "RENDERDOC_GetAPI");

			int ret = RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_5_0, (void**)&Device.pRDocAPI);
			assert(ret == 1);

			int Major, Minor, Path;
			Device.pRDocAPI->GetAPIVersion(&Major, &Minor, &Path);
			Msg("RenderDoc API: %d.%d.%d", Major, Minor, Path);
		}
	}
}
#endif

bool InternalDevice11::CreateD3D11()
{
#if 0
	CreateRDoc();
#endif

	// Set up the presentation parameters
	DXGI_SWAP_CHAIN_DESC sd = {};

	HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
	sd.BufferDesc.Width = psCurrentVidMode[0];
	sd.BufferDesc.Height = psCurrentVidMode[1];
	sd.BufferDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	sd.BufferCount = 1;
	sd.SampleDesc.Count = 1;
	sd.SampleDesc.Quality = 0;
	sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
	sd.OutputWindow = hwnd;
	sd.Windowed = !psDeviceFlags.is(rsFullscreen);

	sd.BufferDesc.RefreshRate.Numerator = 0;
	sd.BufferDesc.RefreshRate.Denominator = 0;

	//	Additional set up
	sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;

	UINT createDeviceFlags = 0;
	bool bHasDebugRender = Core.ParamsData.test(ECoreParams::dxdebug);

	if (CAMDReader* AMDReader = GRHI->DriverExt->GetAMD(); AMDReader != nullptr && !bHasDebugRender)
	{
		u32 NewFeatureLevel = AMDReader->GetDX11Device((void**)&RawDevice, (void**)&HWRenderContext, (void**)&HWSwapchain);
		if (RawDevice != nullptr)
		{
			FeatureLevel = NewFeatureLevel;
		}
	}

	if (RawDevice == nullptr)
	{
		if (bHasDebugRender)
		{
			createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
		}

		const D3D_FEATURE_LEVEL pFeatureLevels[] =
		{
			D3D_FEATURE_LEVEL_11_1,
			D3D_FEATURE_LEVEL_11_0,
			D3D_FEATURE_LEVEL_10_1
		};

		D3D_FEATURE_LEVEL CurLevel;

		HRESULT R = D3D11CreateDeviceAndSwapChain
		(
			0, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, pFeatureLevels,
			std::size(pFeatureLevels), D3D11_SDK_VERSION, &sd, &HWSwapchain,
			(ID3D11Device**)&RawDevice, &CurLevel, &HWRenderContext
		);
		FeatureLevel = CurLevel;

		// main anotation
		if (FeatureLevel == D3D_FEATURE_LEVEL_11_1)
		{
			R_CHK(HWRenderContext->QueryInterface(__uuidof(ID3DUserDefinedAnnotation), (void**)&g_pAnnotation));
		}

		if (FAILED(R))
		{
			Msg
			(
				"Failed to initialize graphics hardware.\n"
				"Please try to restart the game.\n"
				"CreateDevice returned 0x%08x", R
			);

			xrLogger::FlushLog();
			return false;
		};

		if (bHasDebugRender)
		{
			ID3D11InfoQueue* infoQueue = nullptr;
			if (SUCCEEDED(DX11Device->QueryInterface(__uuidof(ID3D11InfoQueue), (void**)&infoQueue)))
			{
				infoQueue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_ERROR, true);

				D3D11_MESSAGE_SEVERITY Severities[] =
				{
					D3D11_MESSAGE_SEVERITY_INFO
				};

				// Suppress individual messages by their ID
				D3D11_MESSAGE_ID DenyIds[] = {
					D3D11_MESSAGE_ID_DEVICE_DRAW_RENDERTARGETVIEW_NOT_SET,
				};

				D3D11_INFO_QUEUE_FILTER NewFilter = {};
				NewFilter.DenyList.NumSeverities = _countof(Severities);
				NewFilter.DenyList.pSeverityList = Severities;
				NewFilter.DenyList.NumIDs = _countof(DenyIds);
				NewFilter.DenyList.pIDList = DenyIds;


				infoQueue->PushStorageFilter(&NewFilter);
				infoQueue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_WARNING, true);
				infoQueue->SetBreakOnID(D3D11_MESSAGE_ID_DEVICE_DRAW_RENDERTARGETVIEW_NOT_SET, false);
			}
		}
	}

	if (!UpdateBuffersD3D11())
	{
		return false;
	}

	// Initialize texture factory
	if (!TextureFactory)
	{
		TextureFactory = new DX11TextureFactory(DX11Device, HWRenderContext);
		TextureFactory = TextureFactory;
	}

	return true;
}

void InternalDevice11::ResizeBuffers(u32 Width, u32 Height)
{
	if (RenderDSV != nullptr)
	{
		RenderDSV->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr)
	{
		((ID3D11ShaderResourceView*)RenderSRV)->Release();
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr)
	{
		RenderRTV->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr)
	{
		SwapChainRTV->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr)
	{
		((ID3D11Texture2D*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}

	DXGI_MODE_DESC Desc = {};
	Desc.Width = Width;
	Desc.Height = Height;
	Desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	Desc.RefreshRate.Numerator = 0;
	Desc.RefreshRate.Denominator = 0;

	HRESULT R = ((IDXGISwapChain*)HWSwapchain)->ResizeTarget(&Desc);
	R_CHK(R);

	R = ((IDXGISwapChain*)HWSwapchain)->ResizeBuffers(0, Width, Height, DXGI_FORMAT_UNKNOWN, DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH);
	R_CHK(R);

	UpdateBuffersD3D11();
}

void InternalDevice11::ClearTarget(void* Target, ERTColor InputColor)
{
	constexpr float ColorTransparent[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	constexpr float ColorRGBA[4] = { 0.5f, 0.5f, 0.5f, 0.5f };
	constexpr float ColorBlack[4] = { 1.f, 1.f, 1.f, 1.f };

	float* ColorPtr = nullptr;

	switch (InputColor)
	{
		case ERTColor::Gray:		ColorPtr = (float*)ColorRGBA; break;
		case ERTColor::Black:		ColorPtr = (float*)ColorBlack; break;
		case ERTColor::Transparent: ColorPtr = (float*)ColorTransparent; break;
	}

	HWRenderContext->ClearRenderTargetView((ID3D11RenderTargetView*)Target, ColorPtr);
}

void InternalDevice11::ClearTarget(void* Target, const float* Color)
{
	HWRenderContext->ClearRenderTargetView((ID3D11RenderTargetView*)Target, Color);
}

void InternalDevice11::ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil)
{
	HWRenderContext->ClearDepthStencilView((ID3D11DepthStencilView*)View->GetRawDSV(), (u32)TargetFlags, Depth, Stencil);
}

void InternalDevice11::GenerateMips(IRHIShaderResourceView* SRV)
{
	HWRenderContext->GenerateMips((ID3D11ShaderResourceView*)SRV->GetRawSRV());
}

void InternalDevice11::DestroyD3D11()
{
	// Clean up texture factory
	if (TextureFactory)
	{
		xr_delete(TextureFactory);
		TextureFactory = nullptr;
	}

	if (RenderDSV != nullptr)
	{
		R_ASSERT(!RenderDSV->Release());
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr)
	{
		((ID3D11ShaderResourceView*)RenderSRV)->Release();
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr)
	{
		((ID3D11RenderTargetView*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr)
	{
		((ID3D11RenderTargetView*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr)
	{
		((ID3D11Texture2D*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}

	bool bHasDebugRender = Core.ParamsData.test(ECoreParams::dxdebug);
	//if (!bHasDebugRender && g_pGPU != nullptr && !g_pGPU->IsAMD)
	//{
	//	g_pGPU->Destroy();
	//}
	//else
	{
		if (g_pAnnotation != nullptr)
		{
			((ID3DUserDefinedAnnotation*)g_pAnnotation)->Release();
			g_pAnnotation = nullptr;
		}

		if (HWRenderContext != nullptr)
		{
			((ID3D11DeviceContext*)HWRenderContext)->Release();
			HWRenderContext = nullptr;
		}

		if (DX11Device != nullptr)
		{
			DX11Device->Release();
			RawDevice = nullptr;
		}

		if (HWSwapchain != nullptr)
		{
			((IDXGISwapChain*)HWSwapchain)->Release();
			HWSwapchain = nullptr;
		}
	}
}

void InternalDevice11::Present()
{
	HWSwapchain->Present(psDeviceFlags.test(rsVSync) ? 1 : 0, 0);
}

void InternalDevice11::CopySurface(IRHISurface* Dest, IRHISurface* Source)
{
	HWRenderContext->CopyResource(((DX11Surface*)Dest)->GetDX11Resource(), ((DX11Surface*)Source)->GetDX11Resource());
}

void InternalDevice11::CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source)
{
	DX11Surface* DestSurf = static_cast<DX11Surface*>(((DX11RenderTargetView*)Dest)->GetSurface());
	DX11Surface* SourceSurf = static_cast<DX11Surface*>(((DX11RenderTargetView*)Source)->GetSurface());

	HWRenderContext->CopyResource(DestSurf->GetDX11Resource(), SourceSurf->GetDX11Resource());
}

bool InternalDevice11::ReadRenderTargetPixels(IRHIRenderTargetView* Rtv, void* Dst, u32 DstSize, u32& OutWidth, u32& OutHeight, u32& OutRowPitch)
{
	if (!Rtv || !Dst)
	{
		return false;
	}

	ID3D11Resource* Resource = nullptr;
	ID3D11RenderTargetView* RTV11 = reinterpret_cast<ID3D11RenderTargetView*>(Rtv->GetRawRTV());
	RTV11->GetResource(&Resource);
	if (!Resource)
	{
		return false;
	}

	ID3D11Texture2D* SrcTex = nullptr;
	HRESULT Hr = Resource->QueryInterface(__uuidof(ID3D11Texture2D), (void**)&SrcTex);
	Resource->Release();
	if (FAILED(Hr) || !SrcTex)
	{
		return false;
	}

	D3D11_TEXTURE2D_DESC Desc = {};
	SrcTex->GetDesc(&Desc);

	D3D11_TEXTURE2D_DESC DescStaging = Desc;
	DescStaging.Usage = D3D11_USAGE_STAGING;
	DescStaging.BindFlags = 0;
	DescStaging.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
	DescStaging.MiscFlags = 0;
	DescStaging.SampleDesc.Count = 1;

	ID3D11Texture2D* Staging = nullptr;
	Hr = DX11Device->CreateTexture2D(&DescStaging, nullptr, &Staging);
	if (FAILED(Hr) || !Staging)
	{
		SrcTex->Release();
		return false;
	}

	HWRenderContext->CopyResource(Staging, SrcTex);

	D3D11_MAPPED_SUBRESOURCE Mapped = {};
	Hr = HWRenderContext->Map(Staging, 0, D3D11_MAP_READ, 0, &Mapped);
	if (FAILED(Hr))
	{
		Staging->Release();
		SrcTex->Release();
		return false;
	}

	OutWidth = Desc.Width;
	OutHeight = Desc.Height;
	OutRowPitch = Mapped.RowPitch;

	unsigned long long Required = (unsigned long long)OutRowPitch * (unsigned long long)OutHeight;
	if (Required > DstSize)
	{
		HWRenderContext->Unmap(Staging, 0);
		Staging->Release();
		SrcTex->Release();
		return false;
	}

	for (u32 Y = 0; Y < OutHeight; ++Y)
	{
		memcpy((u8*)Dst + (size_t)Y * OutRowPitch, (u8*)Mapped.pData + (size_t)Y * Mapped.RowPitch, OutRowPitch);
	}

	HWRenderContext->Unmap(Staging, 0);
	Staging->Release();
	SrcTex->Release();
	return true;
}

void InternalDevice11::SetViewport(RHIViewport& VP)
{
	HWRenderContext->RSSetViewports(1, (D3D11_VIEWPORT*)&VP);
}

void InternalDevice11::SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology)
{
	currentTopology = topology;
	d3dTopology = (D3D_PRIMITIVE_TOPOLOGY)(topology);
	HWRenderContext->IASetPrimitiveTopology(d3dTopology);
}

void InternalDevice11::DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount)
{
	if (primitiveCount == 0)
	{
		return;
	}

	u32 indexCount = RHITopologyUtils::GetIndexCount(primitiveCount, currentTopology);
	HWRenderContext->DrawIndexed(indexCount, startIndex, baseVertex);
}

void InternalDevice11::Draw(u32 startVertex, u32 primitiveCount)
{
	if (primitiveCount == 0)
	{
		return;
	}

	u32 vertexCount = RHITopologyUtils::GetVertexCount(primitiveCount, currentTopology);
	HWRenderContext->Draw(vertexCount, startVertex);
}

void InternalDevice11::DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation)
{
	if (primitiveCount == 0)
	{
		return;
	}

	u32 indexCount = RHITopologyUtils::GetIndexCount(primitiveCount, currentTopology);
	HWRenderContext->DrawIndexedInstanced(indexCount, instanceCount, startIndex, baseVertex, startInstanceLocation);
}

void InternalDevice11::DrawNoInputAssembly(u32 vertexCount)
{
	HWRenderContext->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	HWRenderContext->IASetInputLayout(nullptr);
	HWRenderContext->Draw(vertexCount, 0);
}

void InternalDevice11::SetScissorRect(Irect* R)
{
	if (R)
	{
		D3D11_RECT* clip = (D3D11_RECT*)R;
		HWRenderContext->RSSetScissorRects(1, clip);
	}
	else
	{
		HWRenderContext->RSSetScissorRects(0, nullptr);
	}
}
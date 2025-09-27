#include "Device.h"

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

	R = (DX11Device)->CreateRenderTargetView(pBuffer, nullptr, (ID3D11RenderTargetView**)&SwapChainRTV);
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

	R = DX11Device->CreateRenderTargetView((ID3D11Resource*)RenderTexture, nullptr, (ID3D11RenderTargetView**)&RenderRTV);
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
	D3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc = {};
	depthStencilViewDesc.Format = descDepth.Format;
	depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
	depthStencilViewDesc.Texture2D.MipSlice = 0;
	R = DX11Device->CreateDepthStencilView(pDepthStencil, &depthStencilViewDesc, (ID3D11DepthStencilView**)&RenderDSV);
	R_CHK(R);

	pDepthStencil->Release();
	return true;
}

IRHIBuffer* InternalDevice11::CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource)
{
	CD3D11Buffer* pBuffer = new CD3D11Buffer();
	pBuffer->Create(desc, pSubresource);
	return pBuffer;
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

	//if (g_pGPU != nullptr && g_pGPU->IsAMD)
	//{
	//	g_pGPU->GetDX11Device((ID3D11Device**)&HWRenderDevice, (ID3D11DeviceContext**)&HWRenderContext, (IDXGISwapChain**)&HWSwapchain, FeatureLevel);
	//}

	if (bHasDebugRender || DX11Device == nullptr)
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
	//else
	//{
	//	g_pGPU->GetDX11Device((ID3D11Device**)&HWRenderDevice, (ID3D11DeviceContext**)&HWRenderContext, (IDXGISwapChain**)&HWSwapchain, FeatureLevel);
	//}

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
		((ID3D11DepthStencilView*)RenderDSV)->Release();
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
		((ID3D11DepthStencilView*)RenderDSV)->Release();
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
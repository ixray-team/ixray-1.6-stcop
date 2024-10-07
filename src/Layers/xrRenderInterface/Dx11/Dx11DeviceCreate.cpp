#include "stdafx.h"
#include "Dx11Device.h"

#include <renderdoc/api/app/renderdoc_app.h>

bool CRenderRHI_DX11::UpdateBuffers()
{
	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);

	// Create a render target view
	ID3D11Texture2D* pBuffer = nullptr;
	HRESULT result = m_pHWSwapchain->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
	R_CHK(result);
	if (pBuffer == nullptr) {
		return false;
	}

	result = m_pHWRenderDevice->CreateRenderTargetView(pBuffer, nullptr, (ID3D11RenderTargetView**)&m_pSwapChainRTV);
	pBuffer->Release();
	R_CHK(result);

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
	result = m_pHWRenderDevice->CreateTexture2D(&descDepth, nullptr, &m_pRenderTexture);
	R_CHK(result);
	if (m_pRenderTexture == nullptr) {
		return false;
	}

	result = m_pHWRenderDevice->CreateRenderTargetView(m_pRenderTexture, nullptr, &m_pRenderRTV);
	R_CHK(result);

	result = m_pHWRenderDevice->CreateShaderResourceView(m_pRenderTexture, nullptr, (ID3D11ShaderResourceView**)&m_pRenderSRV);
	R_CHK(result);

	descDepth.Width = UINT(sd.BufferDesc.Width * m_RenderScale);			// TODO: render scale
	descDepth.Height = UINT(sd.BufferDesc.Height * m_RenderScale);		// TODO: render scale

	descDepth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
	descDepth.BindFlags = D3D11_BIND_DEPTH_STENCIL;
	result = m_pHWRenderDevice->CreateTexture2D(&descDepth, nullptr, &pDepthStencil);
	R_CHK(result);

	if (pDepthStencil == nullptr) {
		return false;
	}

	//	Create Depth/stencil view
	D3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc = {};
	depthStencilViewDesc.Format = descDepth.Format;
	depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
	depthStencilViewDesc.Texture2D.MipSlice = 0;
	result = m_pHWRenderDevice->CreateDepthStencilView(pDepthStencil, &depthStencilViewDesc, &m_pRenderDSV);
	R_CHK(result);

	pDepthStencil->Release();
	return true;
}

void CRenderRHI_DX11::CreateRDoc()
{
	if (Core.ParamsData.test(ECoreParams::renderdoc))
	{
		//if (HMODULE mod = LoadLibraryA("renderdoc.dll")) 
		//{
		//	pRENDERDOC_GetAPI RENDERDOC_GetAPI = (pRENDERDOC_GetAPI)GetProcAddress(mod, "RENDERDOC_GetAPI");
		//
		//	int ret = RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_5_0, (void**)&Device.pRDocAPI);
		//	assert(ret == 1);
		//
		//	int Major, Minor, Path;
		//	Device.pRDocAPI->GetAPIVersion(&Major, &Minor, &Path);
		//	Msg("RenderDoc API: %d.%d.%d", Major, Minor, Path);
		//}
	}
}

bool CRenderRHI_DX11::Create()
{
	CreateRDoc();

	// Set up the presentation parameters
	DXGI_SWAP_CHAIN_DESC sd = {};

	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
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

 	if (bHasDebugRender || m_pHWRenderDevice == nullptr)
	{
		if (bHasDebugRender)
		{
			createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
		}

		const D3D_FEATURE_LEVEL pFeatureLevels[] = {
			D3D_FEATURE_LEVEL_11_1,
			D3D_FEATURE_LEVEL_11_0,
			D3D_FEATURE_LEVEL_10_1
		};

		HRESULT R = D3D11CreateDeviceAndSwapChain(
			0, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, pFeatureLevels,
			std::size(pFeatureLevels), D3D11_SDK_VERSION, &sd, (IDXGISwapChain**)&m_pHWSwapchain,
			(ID3D11Device**)&m_pHWRenderDevice, &m_FeatureLevel, (ID3D11DeviceContext**)&m_pHWRenderContext
		);

		// main anotation
		
		if (m_FeatureLevel == D3D_FEATURE_LEVEL_11_1)
		{
			R_CHK(m_pHWRenderContext->QueryInterface(__uuidof(ID3DUserDefinedAnnotation), (void**)&m_pRenderAnnotation));
		}

		if (FAILED(R))
		{
			Msg("Failed to initialize graphics hardware.\n"
				"Please try to restart the game.\n"
				"CreateDevice returned 0x%08x", R
			);

			xrLogger::FlushLog();
			return false;
		};

		if (bHasDebugRender)
		{
			ID3D11InfoQueue* infoQueue = nullptr;
			if (SUCCEEDED(m_pHWRenderDevice->QueryInterface(__uuidof(ID3D11InfoQueue), (void**)&infoQueue)))
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

	if (!UpdateBuffers())
	{
		return false;
	}

	return true;
}

void CRenderRHI_DX11::ResizeBuffers(u16 Width, u16 Height)
{
	if (m_pRenderDSV != nullptr)
	{
		m_pRenderDSV->Release();
		m_pRenderDSV = nullptr;
	}

	if (m_pRenderSRV != nullptr) 
	{
		((ID3D11ShaderResourceView*)m_pRenderSRV)->Release();
		m_pRenderSRV = nullptr;
	}

	if (m_pRenderRTV != nullptr)
	{
		m_pRenderRTV->Release();
		m_pRenderRTV = nullptr;
	}

	if (m_pSwapChainRTV != nullptr) 
	{
		m_pSwapChainRTV->Release();
		m_pSwapChainRTV = nullptr;
	}

	if (m_pRenderTexture != nullptr) {
		m_pRenderTexture->Release();
		m_pRenderTexture = nullptr;
	}

	DXGI_MODE_DESC Desc = {};
	Desc.Width = Width;
	Desc.Height = Height;
	Desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	Desc.RefreshRate.Numerator = 0;
	Desc.RefreshRate.Denominator = 0;

	HRESULT R = m_pHWSwapchain->ResizeTarget(&Desc);
	R_CHK(R);

	R = m_pHWSwapchain->ResizeBuffers(0, Width, Height, DXGI_FORMAT_UNKNOWN, DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH);
	R_CHK(R);

	UpdateBuffers();
}

void CRenderRHI_DX11::Destroy()
{
	if (m_pRenderDSV != nullptr) 
	{
		m_pRenderDSV->Release();
		m_pRenderDSV = nullptr;
	}

	if (m_pRenderSRV != nullptr)
	{
		((ID3D11ShaderResourceView*)m_pRenderSRV)->Release();
		m_pRenderSRV = nullptr;
	}

	if (m_pRenderRTV != nullptr) 
	{
		m_pRenderRTV->Release();
		m_pRenderRTV = nullptr;
	}

	if (m_pSwapChainRTV != nullptr) 
	{
		m_pSwapChainRTV->Release();
		m_pSwapChainRTV = nullptr;
	}

	if (m_pRenderTexture != nullptr) 
	{
		m_pRenderTexture->Release();
		m_pRenderTexture = nullptr;
	}

	//bool bHasDebugRender = Core.ParamsData.test(ECoreParams::dxdebug);
	//if (!bHasDebugRender && g_pGPU != nullptr && !g_pGPU->IsAMD)
	//{
	//	g_pGPU->Destroy();
	//}
	//else
	{
		if (m_pRenderAnnotation != nullptr)
		{
			m_pRenderAnnotation->Release();
			m_pRenderAnnotation = nullptr;
		}

		if (m_pHWRenderContext != nullptr) 
		{
			m_pHWRenderContext->Release();
			m_pHWRenderContext = nullptr;
		}

		if (m_pHWRenderDevice != nullptr) 
		{
			m_pHWRenderDevice->Release();
			m_pHWRenderDevice = nullptr;
		}

		if (m_pHWSwapchain != nullptr)
		{
			m_pHWSwapchain->Release();
			m_pHWSwapchain = nullptr;
		}
	}

}

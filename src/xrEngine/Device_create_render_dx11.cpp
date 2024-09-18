#include "stdafx.h"
#include <d3d11.h>
#include "ICore_GPU.h"
#include <renderdoc/api/app/renderdoc_app.h>

extern D3D_FEATURE_LEVEL FeatureLevel;
extern void* HWSwapchain;

extern void* HWRenderDevice ;
extern void* HWRenderContext;

extern void* RenderTexture;
extern void* RenderSRV;
extern void* RenderRTV;

extern void* RenderDSV;
extern void* SwapChainRTV;

HWND GetHWNDFromSDLWindow(SDL_Window* window)
{
	SDL_SysWMinfo wmInfo;
	SDL_VERSION(&wmInfo.version);

	if (SDL_GetWindowWMInfo(window, &wmInfo))
	{
		return wmInfo.info.win.window;
	}
	else
	{
		// Msg("! Failed to get window info: %s", SDL_GetError());
		return nullptr;
	}
}

bool UpdateBuffersD3D11()
{
	HWND hwnd = GetHWNDFromSDLWindow(g_AppInfo.Window);

	// Create a render target view
	ID3D11Texture2D* pBuffer = nullptr;
	HRESULT R = ((IDXGISwapChain*)HWSwapchain)->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
	R_CHK(R);
	if (pBuffer == nullptr) {
		return false;
	}

	R = ((ID3D11Device*)HWRenderDevice)->CreateRenderTargetView(pBuffer, nullptr, (ID3D11RenderTargetView**)&SwapChainRTV);
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
	R = ((ID3D11Device*)HWRenderDevice)->CreateTexture2D(&descDepth, nullptr, (ID3D11Texture2D**)&RenderTexture);
	R_CHK(R);
	if (RenderTexture == nullptr) {
		return false;
	}

	R = ((ID3D11Device*)HWRenderDevice)->CreateRenderTargetView((ID3D11Resource*)RenderTexture, nullptr, (ID3D11RenderTargetView**)&RenderRTV);
	R_CHK(R);

	R = ((ID3D11Device*)HWRenderDevice)->CreateShaderResourceView((ID3D11Resource*)RenderTexture, nullptr, (ID3D11ShaderResourceView**)&RenderSRV);
	R_CHK(R);

	descDepth.Width = UINT(sd.BufferDesc.Width * Device.RenderScale);			// TODO: render scale
	descDepth.Height = UINT(sd.BufferDesc.Height * Device.RenderScale);		// TODO: render scale

	descDepth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
	descDepth.BindFlags = D3D11_BIND_DEPTH_STENCIL;
	R = ((ID3D11Device*)HWRenderDevice)->CreateTexture2D(&descDepth, nullptr, &pDepthStencil);
	R_CHK(R);

	if (pDepthStencil == nullptr) {
		return false;
	}

	//	Create Depth/stencil view
	D3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc = {};
	depthStencilViewDesc.Format = descDepth.Format;
	depthStencilViewDesc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
	depthStencilViewDesc.Texture2D.MipSlice = 0;
	R = ((ID3D11Device*)HWRenderDevice)->CreateDepthStencilView(pDepthStencil, &depthStencilViewDesc, (ID3D11DepthStencilView**)&RenderDSV);
	R_CHK(R);

	pDepthStencil->Release();
	return true;
}

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

bool CreateD3D11(bool ReturnToDX10)
{
	CreateRDoc();

	// Set up the presentation parameters
	DXGI_SWAP_CHAIN_DESC sd = {};

	HWND hwnd = GetHWNDFromSDLWindow(g_AppInfo.Window);
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

 	if (bHasDebugRender || HWRenderDevice == nullptr)
	{
		if (bHasDebugRender)
		{
			createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
		}

		constexpr D3D_FEATURE_LEVEL pFeatureLevels10[] = {
			D3D_FEATURE_LEVEL_10_1,
			D3D_FEATURE_LEVEL_10_0,
		};

		constexpr D3D_FEATURE_LEVEL pFeatureLevels[] = {
			D3D_FEATURE_LEVEL_11_1,
			D3D_FEATURE_LEVEL_11_0,
			D3D_FEATURE_LEVEL_10_1,
			D3D_FEATURE_LEVEL_10_0,
		};
		
		HRESULT R = S_OK;
		if (ReturnToDX10)
		{
			R = D3D11CreateDeviceAndSwapChain(
				0, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, pFeatureLevels10,
				std::size(pFeatureLevels10), D3D11_SDK_VERSION, &sd, (IDXGISwapChain**)&HWSwapchain,
				(ID3D11Device**)&HWRenderDevice, &FeatureLevel, (ID3D11DeviceContext**)&HWRenderContext
			);
		}
		else
		{
			R = D3D11CreateDeviceAndSwapChain(
				0, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, pFeatureLevels,
				std::size(pFeatureLevels), D3D11_SDK_VERSION, &sd, (IDXGISwapChain**)&HWSwapchain,
				(ID3D11Device**)&HWRenderDevice, &FeatureLevel, (ID3D11DeviceContext**)&HWRenderContext
			);
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
			if (SUCCEEDED(((ID3D11Device*)HWRenderDevice)->QueryInterface(__uuidof(ID3D11InfoQueue), (void**)&infoQueue)))
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

	return true;
}

void ResizeBuffersD3D11(u16 Width, u16 Height)
{
	if (RenderDSV != nullptr) {
		((ID3D11DepthStencilView*)RenderDSV)->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr) {
		((ID3D11ShaderResourceView*)RenderSRV)->Release();
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr) {
		((ID3D11RenderTargetView*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr) {
		((ID3D11RenderTargetView*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr) {
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

void DestroyD3D11()
{
	if (RenderDSV != nullptr) {
		((ID3D11DepthStencilView*)RenderDSV)->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr) {
		((ID3D11ShaderResourceView*)RenderSRV)->Release();
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr) {
		((ID3D11RenderTargetView*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr) {
		((ID3D11RenderTargetView*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr) {
		((ID3D11Texture2D*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}

	bool bHasDebugRender = Core.ParamsData.test(ECoreParams::dxdebug);
	if (!bHasDebugRender && g_pGPU != nullptr && !g_pGPU->IsAMD)
	{
		g_pGPU->Destroy();
	}
	else
	{
		if (HWRenderContext != nullptr) {
			((ID3D11DeviceContext*)HWRenderContext)->Release();
			HWRenderContext = nullptr;
		}

		if (HWRenderDevice != nullptr) {
			((ID3D11Device*)HWRenderDevice)->Release();
			HWRenderDevice = nullptr;
		}

		if (HWSwapchain != nullptr) {
			((IDXGISwapChain*)HWSwapchain)->Release();
			HWSwapchain = nullptr;
		}
	}

}

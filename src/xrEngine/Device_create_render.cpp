#include "stdafx.h"
#include "../xrCore/_std_extensions.h"

#include <d3d11.h>
#include <d3d9.h>

#pragma comment(lib, "d3d9.lib")
#pragma comment(lib, "d3d11.lib")
#pragma comment(lib, "dxgi.lib")

static APILevel CurrentAPILevel = APILevel::DX11;

D3D_FEATURE_LEVEL FeatureLevel = D3D_FEATURE_LEVEL::D3D_FEATURE_LEVEL_11_0;
static void* HWSwapchain = nullptr;

static void* HWRenderDevice = nullptr;
static void* HWRenderContext = nullptr;

static void* RenderTexture = nullptr;
static void* RenderSRV = nullptr;
static void* RenderRTV = nullptr;

static void* RenderDSV = nullptr;
static void* SwapChainRTV = nullptr;

void free_vid_mode_list()
{
	for (int i = 0; vid_mode_token[i].name; i++) {
		xr_free(vid_mode_token[i].name);
	}

	xr_free(vid_mode_token);
	vid_mode_token = NULL;
}

struct _uniq_mode
{
	_uniq_mode(LPCSTR v) :_val(v) {}
	LPCSTR _val;
	bool operator() (LPCSTR _other) { return !_stricmp(_val, _other); }
};

void fill_vid_mode_list()
{
	if (vid_mode_token != NULL)		return;
	xr_vector<LPCSTR>	_tmp;
	xr_vector<DXGI_MODE_DESC>	modes;

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

	_RELEASE(pOutput);

	for (u32 i = 0; i < num; ++i)
	{
		DXGI_MODE_DESC& desc = modes[i];
		string32		str;

		if (desc.Width < 800)
			continue;

		xr_sprintf(str, sizeof(str), "%dx%d", desc.Width, desc.Height);

		if (_tmp.end() != std::find_if(_tmp.begin(), _tmp.end(), _uniq_mode(str)))
			continue;

		_tmp.push_back(NULL);
		_tmp.back() = xr_strdup(str);
	}

	u32 _cnt = (u32)_tmp.size() + 1;

	vid_mode_token = xr_alloc<xr_token>(_cnt);

	vid_mode_token[_cnt - 1].id = -1;
	vid_mode_token[_cnt - 1].name = NULL;

#ifdef DEBUG
	Msg("Available video modes[%d]:", _tmp.size());
#endif // DEBUG
	for (u32 i = 0; i < _tmp.size(); ++i)
	{
		vid_mode_token[i].id = i;
		vid_mode_token[i].name = _tmp[i];
#ifdef DEBUG
		Msg("[%s]", _tmp[i]);
#endif // DEBUG
	}
}


static bool CreateD3D9()
{
	return false;
}

static bool UpdateBuffersD3D11()
{
	// Create a render target view
	ID3D11Texture2D* pBuffer = nullptr;
	HRESULT R = ((IDXGISwapChain*)HWSwapchain)->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
	R_CHK(R);
	if (pBuffer == nullptr) {
		return false;
	}

	R = ((ID3D11Device*)HWRenderDevice)->CreateRenderTargetView(pBuffer, NULL, (ID3D11RenderTargetView**)&SwapChainRTV);
	pBuffer->Release();
	R_CHK(R);

	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
	DXGI_SWAP_CHAIN_DESC sd = {};
	sd.BufferDesc.Width = psCurrentVidMode[0];
	sd.BufferDesc.Height = psCurrentVidMode[1];
	sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
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

	descDepth.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	descDepth.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
	R = ((ID3D11Device*)HWRenderDevice)->CreateTexture2D(&descDepth, nullptr, (ID3D11Texture2D**)&RenderTexture);
	R_CHK(R);
	if (RenderTexture == nullptr) {
		return false;
	}

	R = ((ID3D11Device*)HWRenderDevice)->CreateRenderTargetView((ID3D11Resource*)RenderTexture, NULL, (ID3D11RenderTargetView**)&RenderRTV);
	R_CHK(R);

	R = ((ID3D11Device*)HWRenderDevice)->CreateShaderResourceView((ID3D11Resource*)RenderTexture, NULL, (ID3D11ShaderResourceView**)&RenderSRV);
	R_CHK(R);

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

static bool CreateD3D11()
{
	// Set up the presentation parameters
	DXGI_SWAP_CHAIN_DESC sd = {};

	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
	sd.BufferDesc.Width = psCurrentVidMode[0];
	sd.BufferDesc.Height = psCurrentVidMode[1];
	sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
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
	if (strstr(Core.Params, "-dxdebug")) {
		createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
	}

	const D3D_FEATURE_LEVEL pFeatureLevels[] = {
		D3D_FEATURE_LEVEL_11_1,
		D3D_FEATURE_LEVEL_11_0,
		D3D_FEATURE_LEVEL_10_1,
		D3D_FEATURE_LEVEL_10_0,
	};

	HRESULT R = D3D11CreateDeviceAndSwapChain(
		0, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, pFeatureLevels,
		sizeof(pFeatureLevels) / sizeof(pFeatureLevels[0]),
		D3D11_SDK_VERSION, &sd, (IDXGISwapChain**)&HWSwapchain, 
		(ID3D11Device**)&HWRenderDevice, &FeatureLevel, (ID3D11DeviceContext**)&HWRenderContext
	);

	if (FAILED(R)) {
		Msg("Failed to initialize graphics hardware.\n"
			"Please try to restart the game.\n"
			"CreateDevice returned 0x%08x", R
		);

		FlushLog();
		return false;
	};

	if (!UpdateBuffersD3D11()) {
		return false;
	}

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];
	return true;
}

static void ResizeBuffersD3D11(u16 Width, u16 Height)
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
	Desc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	Desc.RefreshRate.Numerator = 0;
	Desc.RefreshRate.Denominator = 0;

	HRESULT R = ((IDXGISwapChain*)HWSwapchain)->ResizeTarget(&Desc);	
	R_CHK(R);

	R = ((IDXGISwapChain*)HWSwapchain)->ResizeBuffers(0, Width, Height, DXGI_FORMAT_UNKNOWN, DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH);
	R_CHK(R);

	UpdateBuffersD3D11();

	const bool Centered = strstr(Core.Params, "-no_center_screen") == nullptr;
	SDL_SetWindowSize(g_AppInfo.Window, Width, Height);
	SDL_SetWindowPosition(g_AppInfo.Window, Centered ? SDL_WINDOWPOS_CENTERED : 0, Centered ? SDL_WINDOWPOS_CENTERED : 0);

	Device.TargetWidth = Width;
	Device.TargetHeight = Height;
}

void DestroyD3D9()
{
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

bool CRenderDevice::InitRenderDevice(APILevel API)
{
	fill_vid_mode_list();

	switch (API) {

	case APILevel::DX9:
		if (!CreateD3D9()) {
			return false;
		}
		break;

	case APILevel::DX11:
		if (!CreateD3D11()) {
			return false;
		}
		break;

	default:
		break;
	}

	if (HWRenderDevice == nullptr) {
		return false;
	}

	CurrentAPILevel = API;
	return true;
}

void CRenderDevice::DestroyRenderDevice()
{
	switch (CurrentAPILevel) {

	case APILevel::DX9:
		DestroyD3D9();
		break;

	case APILevel::DX11:
		DestroyD3D11();
		break;

	default:
		break;
	}

	free_vid_mode_list();
}

void* CRenderDevice::GetRenderDevice()
{
	return HWRenderDevice;
}

void* CRenderDevice::GetRenderContext()
{
	return HWRenderContext;
}

void* CRenderDevice::GetRenderTexture()
{
	return SwapChainRTV;//RenderRTV;
}

void* CRenderDevice::GetDepthTexture()
{
	return RenderDSV;
}

void* CRenderDevice::GetSwapchainTexture()
{
	return SwapChainRTV;
}

void* CRenderDevice::GetSwapchain()
{
	return HWSwapchain;
}

u32	CRenderDevice::GetSwapchainWidth()
{
	return TargetWidth;
}

u32	CRenderDevice::GetSwapchainHeight()
{
	return TargetHeight;
}

void CRenderDevice::ResizeBuffers(u16 Width, u16 Height)
{
	switch (CurrentAPILevel) {

	case APILevel::DX9:
		break;

	case APILevel::DX11:
		ResizeBuffersD3D11(Width, Height);
		break;

	default:
		break;
	}
}

D3D_FEATURE_LEVEL CRenderDevice::GetFeatureLevel()
{
	return FeatureLevel;
}

RENDERDOC_API_1_6_0* CRenderDevice::GetRenderDocAPI()
{
	return nullptr;
}

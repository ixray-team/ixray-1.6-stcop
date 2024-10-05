#include "stdafx.h"

#include <d3d9.h>

extern void* HWSwapchain;

IDirect3D9* D3D = nullptr;
IDirect3DStateBlock9* DebugSB = nullptr;
extern void* HWRenderDevice;
extern void* HWRenderContext;

extern void* RenderTexture;
extern void* RenderSRV;
extern void* RenderRTV;

extern void* RenderDSV;
extern void* SwapChainRTV;

static u32 selectPresentInterval()
{
	D3DCAPS9	caps;
	D3D->GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, &caps);

	if (!psDeviceFlags.test(rsVSync)) {
		if (caps.PresentationIntervals & D3DPRESENT_INTERVAL_IMMEDIATE)
			return D3DPRESENT_INTERVAL_IMMEDIATE;
		if (caps.PresentationIntervals & D3DPRESENT_INTERVAL_ONE)
			return D3DPRESENT_INTERVAL_ONE;
	}

	return D3DPRESENT_INTERVAL_DEFAULT;
}
static u32 selectRefresh(u32 dwWidth, u32 dwHeight, D3DFORMAT fmt)
{
	if (psDeviceFlags.is(rsRefresh60hz)) 
	{
		return D3DPRESENT_RATE_DEFAULT;
	}
	else 
	{
		u32 selected = D3DPRESENT_RATE_DEFAULT;
		u32 count = D3D->GetAdapterModeCount(D3DADAPTER_DEFAULT, fmt);
		for (u32 I = 0; I < count; I++) 
		{
			D3DDISPLAYMODE	Mode;
			D3D->EnumAdapterModes(D3DADAPTER_DEFAULT, fmt, I, &Mode);

			if (Mode.Width == dwWidth && Mode.Height == dwHeight) 
			{
				if (Mode.RefreshRate > selected) selected = Mode.RefreshRate;
			}
		}

		return selected;
	}
}

void UpdateBuffersD3D9()
{
	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
	R_CHK(((IDirect3DDevice9*)HWRenderDevice)->CreateTexture(
		psCurrentVidMode[0], psCurrentVidMode[1], 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8,
		D3DPOOL_DEFAULT, (IDirect3DTexture9**)&RenderTexture, nullptr
	));
	RenderSRV = RenderTexture;

	R_CHK(((IDirect3DTexture9*)RenderTexture)->GetSurfaceLevel(0, (IDirect3DSurface9**)&RenderRTV));
	R_CHK(((IDirect3DDevice9*)HWRenderDevice)->GetRenderTarget(0, (IDirect3DSurface9**)&SwapChainRTV));
	R_CHK(((IDirect3DDevice9*)HWRenderDevice)->GetDepthStencilSurface((IDirect3DSurface9**)&RenderDSV));
}

D3DPRESENT_PARAMETERS GetPresentParameter(int Width = psCurrentVidMode[0], int Height = psCurrentVidMode[1])
{
	D3DPRESENT_PARAMETERS P = {};
	P.BackBufferWidth = Width;
	P.BackBufferHeight = Height;
	P.BackBufferFormat = D3DFMT_X8R8G8B8;
	P.BackBufferCount = 1;

	// Multisample
	P.MultiSampleType = D3DMULTISAMPLE_NONE;
	P.MultiSampleQuality = 0;

	// Windoze
	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
	P.SwapEffect = !psDeviceFlags.is(rsFullscreen) ? D3DSWAPEFFECT_COPY : D3DSWAPEFFECT_DISCARD;
	P.hDeviceWindow = hwnd;
	P.Windowed = !psDeviceFlags.is(rsFullscreen);

	// Depth/stencil
	P.EnableAutoDepthStencil = TRUE;
	P.AutoDepthStencilFormat = D3DFMT_D24S8;
	P.Flags = 0;

	// Refresh rate
	P.PresentationInterval = !psDeviceFlags.test(rsVSync) ? selectPresentInterval() : D3DPRESENT_INTERVAL_DEFAULT;
	P.FullScreen_RefreshRateInHz = psDeviceFlags.is(rsFullscreen) ? selectRefresh(Width, Height, D3DFMT_X8R8G8B8) : D3DPRESENT_RATE_DEFAULT;
	return P;
}

void ResizeBuffersD3D9(u16 Width, u16 Height)
{
	if (RenderDSV != nullptr) {
		((IDirect3DSurface9*)RenderDSV)->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr) {
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr) {
		((IDirect3DSurface9*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr) {
		((IDirect3DSurface9*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr) {
		((IDirect3DTexture9*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}
	
	if (DebugSB != nullptr) {
		DebugSB->Release();
		DebugSB = nullptr;
	}

	IDirect3DDevice9*& DxDevice = *((IDirect3DDevice9**)&HWRenderDevice);
	auto P = GetPresentParameter(Width, Height);
	if (HWRenderDevice != nullptr) {
		while (TRUE) {
			HRESULT _hr = DxDevice->Reset(&P);
			if (SUCCEEDED(_hr))					break;
			Msg("! ERROR: [%dx%d]: %s", P.BackBufferWidth, P.BackBufferHeight, Debug.dxerror2string(_hr));
			Sleep(100);
		}
	} else {
		HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
		HRESULT hr = D3D->CreateDevice(
			D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, hwnd,
			D3DCREATE_HARDWARE_VERTEXPROCESSING | D3DCREATE_MULTITHREADED, &P,
			&DxDevice
		);
		R_CHK(hr);
	}

#ifdef DEBUG
	R_CHK(DxDevice->CreateStateBlock(D3DSBT_ALL, &DebugSB));
#endif

	UpdateBuffersD3D9();
}

bool CreateD3D9()
{
	D3D = Direct3DCreate9(D3D_SDK_VERSION);

	auto P = GetPresentParameter();
	if (HWRenderDevice == nullptr) {
		HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(g_AppInfo.Window), "SDL.window.win32.hwnd", nullptr);
		HRESULT hr = D3D->CreateDevice(
			D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, hwnd,
			D3DCREATE_HARDWARE_VERTEXPROCESSING | D3DCREATE_MULTITHREADED, &P,
			(IDirect3DDevice9**)&HWRenderDevice
		);
		R_CHK(hr);
	}

	UpdateBuffersD3D9();
	return true;
}

void DestroyD3D9()
{
	if (RenderDSV != nullptr) {
		((IDirect3DSurface9*)RenderDSV)->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr) {
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr) {
		((IDirect3DSurface9*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr) {
		((IDirect3DSurface9*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr) {
		((IDirect3DTexture9*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}

	if (DebugSB != nullptr) {
		DebugSB->Release();
		DebugSB = nullptr;
	}

	if (HWRenderDevice != nullptr) {
		((IDirect3DDevice9*)HWRenderDevice)->Release();
		HWRenderDevice = nullptr;
	}

	if (D3D != nullptr) {
		D3D->Release();
		D3D = nullptr;
	}
}

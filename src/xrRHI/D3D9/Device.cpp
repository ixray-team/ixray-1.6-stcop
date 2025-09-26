#include "Device.h"

#define DX9Device ((IDirect3DDevice9*)RawDevice)

InternalDevice9::InternalDevice9()
{
	CreateD3D9();
}

InternalDevice9::~InternalDevice9()
{
	DestroyD3D9();
}

u32 InternalDevice9::selectPresentInterval()
{
	D3DCAPS9 caps;
	D3D->GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, &caps);

	if (!psDeviceFlags.test(rsVSync))
	{
		if (caps.PresentationIntervals & D3DPRESENT_INTERVAL_IMMEDIATE)
			return D3DPRESENT_INTERVAL_IMMEDIATE;
		if (caps.PresentationIntervals & D3DPRESENT_INTERVAL_ONE)
			return D3DPRESENT_INTERVAL_ONE;
	}

	return D3DPRESENT_INTERVAL_DEFAULT;
}

u32 InternalDevice9::selectRefresh(u32 dwWidth, u32 dwHeight, D3DFORMAT fmt)
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
			D3DDISPLAYMODE Mode;
			D3D->EnumAdapterModes(D3DADAPTER_DEFAULT, fmt, I, &Mode);

			if (Mode.Width == dwWidth && Mode.Height == dwHeight)
			{
				if (Mode.RefreshRate > selected) selected = Mode.RefreshRate;
			}
		}

		return selected;
	}
}

void InternalDevice9::UpdateBuffersD3D9()
{
	HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
	R_CHK(DX9Device->CreateTexture(
		psCurrentVidMode[0], psCurrentVidMode[1], 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8,
		D3DPOOL_DEFAULT, (IDirect3DTexture9**)&RenderTexture, nullptr
	));
	RenderSRV = RenderTexture;

	R_CHK(((IDirect3DTexture9*)RenderTexture)->GetSurfaceLevel(0, (IDirect3DSurface9**)&RenderRTV));
	R_CHK(DX9Device->GetRenderTarget(0, (IDirect3DSurface9**)&SwapChainRTV));
	R_CHK(DX9Device->GetDepthStencilSurface((IDirect3DSurface9**)&RenderDSV));

	HalfTarget.x = psCurrentVidMode[0];
	HalfTarget.y = psCurrentVidMode[1];
}

D3DPRESENT_PARAMETERS InternalDevice9::GetPresentParameter(int Width = psCurrentVidMode[0], int Height = psCurrentVidMode[1])
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
	HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
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

void InternalDevice9::ResizeBuffers(u32 Width, u32 Height)
{
	if (RenderDSV != nullptr)
	{
		((IDirect3DSurface9*)RenderDSV)->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr)
	{
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr)
	{
		((IDirect3DSurface9*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr)
	{
		((IDirect3DSurface9*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr)
	{
		((IDirect3DTexture9*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}

	if (DebugSB != nullptr)
	{
		DebugSB->Release();
		DebugSB = nullptr;
	}

	auto P = GetPresentParameter(Width, Height);
	if (DX9Device != nullptr)
	{
		while (TRUE)
		{
			HRESULT _hr = DX9Device->Reset(&P);
			if (SUCCEEDED(_hr))
				break;

			Msg("! ERROR: [%dx%d]: %s", P.BackBufferWidth, P.BackBufferHeight, Debug.dxerror2string(_hr));
			Sleep(100);
		}
	}
	else
	{
		HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
		HRESULT hr = D3D->CreateDevice
		(
			D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, hwnd,
			D3DCREATE_HARDWARE_VERTEXPROCESSING | D3DCREATE_MULTITHREADED, &P,
			(IDirect3DDevice9**)&RawDevice
		);
		R_CHK(hr);
	}

#ifdef DEBUG
	R_CHK(DX9Device->CreateStateBlock(D3DSBT_ALL, &DebugSB));
#endif

	UpdateBuffersD3D9();
}

void InternalDevice9::ClearTarget(void* Target, ERTColor InputColor)
{
	constexpr u32 ColorTransparent = color_xrgb(0, 0, 0);
	constexpr u32 Color = color_xrgb(127, 127, 127);
	constexpr u32 ColorBlack = color_xrgb(255, 255, 255);

	u32 ColorPtr = 0;
	switch (InputColor)
	{
		case ERTColor::Gray:		ColorPtr = Color; break;
		case ERTColor::Black:		ColorPtr = ColorBlack; break;
		case ERTColor::Transparent: ColorPtr = ColorTransparent; break;
	}

	DX9Device->SetRenderTarget(0, (IDirect3DSurface9*)Target);
	DX9Device->Clear(0, nullptr, D3DCLEAR_TARGET, ColorPtr, 1, 0);
}

bool InternalDevice9::CreateD3D9()
{
	D3D = Direct3DCreate9(D3D_SDK_VERSION);

	auto P = GetPresentParameter();
	if (RawDevice == nullptr)
	{
		HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
		HRESULT hr = D3D->CreateDevice
		(
			D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, hwnd,
			D3DCREATE_HARDWARE_VERTEXPROCESSING | D3DCREATE_MULTITHREADED, &P,
			(IDirect3DDevice9**)&RawDevice
		);
		R_CHK(hr);
	}

	UpdateBuffersD3D9();
	return true;
}

void InternalDevice9::DestroyD3D9()
{
	if (RenderDSV != nullptr)
	{
		((IDirect3DSurface9*)RenderDSV)->Release();
		RenderDSV = nullptr;
	}

	if (RenderSRV != nullptr)
	{
		RenderSRV = nullptr;
	}

	if (RenderRTV != nullptr)
	{
		((IDirect3DSurface9*)RenderRTV)->Release();
		RenderRTV = nullptr;
	}

	if (SwapChainRTV != nullptr)
	{
		((IDirect3DSurface9*)SwapChainRTV)->Release();
		SwapChainRTV = nullptr;
	}

	if (RenderTexture != nullptr)
	{
		((IDirect3DTexture9*)RenderTexture)->Release();
		RenderTexture = nullptr;
	}

	if (DebugSB != nullptr)
	{
		DebugSB->Release();
		DebugSB = nullptr;
	}

	if (RawDevice != nullptr)
	{
		DX9Device->Release();
		RawDevice = nullptr;
	}

	if (D3D != nullptr)
	{
		D3D->Release();
		D3D = nullptr;
	}
}
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

IRHITextureFactory* InternalDevice9::GetTextureFactory()
{
	return TextureFactory;
}

void InternalDevice9::SetTextureFactory(IRHITextureFactory* factory)
{
	xr_delete(TextureFactory);
	TextureFactory = static_cast<DX9TextureFactory*>(factory);
	TextureFactory = TextureFactory;
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

IRHIBuffer* InternalDevice9::CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource)
{
	return new CD3D9Buffer(static_cast<IDirect3DDevice9*>(RawDevice), desc, pSubresource);
}

void InternalDevice9::SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIDepthStencilView* pDepthStencilView)
{
	DX9Device->SetDepthStencilSurface(pDepthStencilView ? static_cast<IDirect3DSurface9*>(pDepthStencilView->GetRawDSV()) : NULL);

	// unrolled loop :p
	//DX9Device->SetRenderTarget(0, ppRenderTargetViews[0] ? static_cast<IDirect3DSurface9*>(ppRenderTargetViews[0]->GetRawRTV()) : NULL);
	//DX9Device->SetRenderTarget(1, ppRenderTargetViews[1] ? static_cast<IDirect3DSurface9*>(ppRenderTargetViews[1]->GetRawRTV()) : NULL);
	//DX9Device->SetRenderTarget(2, ppRenderTargetViews[2] ? static_cast<IDirect3DSurface9*>(ppRenderTargetViews[2]->GetRawRTV()) : NULL);
	//DX9Device->SetRenderTarget(3, ppRenderTargetViews[3] ? static_cast<IDirect3DSurface9*>(ppRenderTargetViews[3]->GetRawRTV()) : NULL);
}

void InternalDevice9::UpdateBuffersD3D9()
{
	HWND hwnd = (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(g_AppInfo.Window), SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
	R_CHK(DX9Device->CreateTexture(
		psCurrentVidMode[0], psCurrentVidMode[1], 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8,
		D3DPOOL_DEFAULT, (IDirect3DTexture9**)&RenderTexture, nullptr
	));
	RenderSRV = RenderTexture;

	IDirect3DSurface9* SwapchainRaw = nullptr;
	IDirect3DSurface9* RTVRaw = nullptr;
	R_CHK(((IDirect3DTexture9*)RenderTexture)->GetSurfaceLevel(0, &RTVRaw));
	R_CHK(DX9Device->GetRenderTarget(0, &SwapchainRaw));

	IDirect3DSurface9* DSV = nullptr;
	R_CHK(DX9Device->GetDepthStencilSurface((IDirect3DSurface9**)&DSV));

	RenderRTV = new DX9RenderTargetView(RTVRaw, new DX9Surface(((IDirect3DTexture9*)RenderTexture)));
	SwapChainRTV = new DX9RenderTargetView(SwapchainRaw, new DX9Surface(((IDirect3DTexture9*)RenderTexture)));
	RenderDSV = new DX9DepthStencilView(DSV, new DX9Surface(((IDirect3DTexture9*)RenderTexture)));

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
		R_ASSERT(!RenderDSV->Release());
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

void InternalDevice9::Present()
{
	DX9Device->EndScene();
	DX9Device->Present(nullptr, nullptr, nullptr, nullptr);
}

void InternalDevice9::ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil)
{
	DWORD dx9Flags = 0;

	if ((TargetFlags & ERHI_CLEAR_TARGET::DEPTH) != ERHI_CLEAR_TARGET(0))
	{
		dx9Flags |= D3DCLEAR_ZBUFFER;
	}
	
	if ((TargetFlags & ERHI_CLEAR_TARGET::STENCIL) != ERHI_CLEAR_TARGET(0))
	{
		dx9Flags |= D3DCLEAR_STENCIL;
	}

	DX9Device->Clear(0, nullptr, dx9Flags, 0, Depth, Stencil);
}


void InternalDevice9::ClearTarget(void* Target, ERTColor InputColor)
{
	constexpr u32 ColorTransparent = color_rgba(0, 0, 0, 0);
	constexpr u32 Color = color_rgba(127, 127, 127, 127);
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

void InternalDevice9::ClearTarget(void* Target, const float* Color)
{
	// Convert float[4] (RGBA) to D3D9 color format
	u32 d3d9Color = color_rgba(
		(u8)(Color[0] * 255.0f),  // R
		(u8)(Color[1] * 255.0f),  // G
		(u8)(Color[2] * 255.0f),  // B
		(u8)(Color[3] * 255.0f)   // A
	);
	
	DX9Device->SetRenderTarget(0, (IDirect3DSurface9*)Target);
	DX9Device->Clear(0, nullptr, D3DCLEAR_TARGET, d3d9Color, 1, 0);
}

void InternalDevice9::GenerateMips(IRHIShaderResourceView* SRV)
{
	// D3D9 doesn't have built-in GenerateMips, so we'll use the texture's GenerateMipSubLevels
	// This is a placeholder implementation - in practice, you might want to implement
	// software mip generation or use a different approach
	IDirect3DBaseTexture9* texture = (IDirect3DBaseTexture9*)SRV->GetSurface()->GetRawTexture();
	if (texture)
	{
		texture->GenerateMipSubLevels();
	}
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
	
	// Initialize texture factory
	if (!TextureFactory)
	{
		TextureFactory = new DX9TextureFactory(DX9Device);
		TextureFactory = TextureFactory;
	}
	
	return true;
}

void InternalDevice9::DestroyD3D9()
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

void InternalDevice9::CopySurface(IRHISurface* Dest, IRHISurface* Source)
{
	DX9Surface* pDestSurface = static_cast<DX9Surface*>(Dest);
	DX9Surface* pSourceSurface = static_cast<DX9Surface*>(Source);

	IDirect3DSurface9* pDestSurf = nullptr;
	IDirect3DSurface9* pSourceSurf = nullptr;

	if (pSourceSurface->GetDX9Texture2D())
	{
		pSourceSurface->GetDX9Texture2D()->GetSurfaceLevel(0, &pSourceSurf);
	}

	if (pDestSurface->GetDX9Texture2D())
	{
		pDestSurface->GetDX9Texture2D()->GetSurfaceLevel(0, &pDestSurf);
	}

	if (!pSourceSurf && pSourceSurface->GetDX9TextureCube())
	{
		pSourceSurface->GetDX9TextureCube()->GetCubeMapSurface(D3DCUBEMAP_FACE_POSITIVE_X, 0, &pSourceSurf);
	}

	if (!pDestSurf && pDestSurface->GetDX9TextureCube())
	{
		pDestSurface->GetDX9TextureCube()->GetCubeMapSurface(D3DCUBEMAP_FACE_POSITIVE_X, 0, &pDestSurf);
	}

	if (pSourceSurf && pDestSurf)
	{
		DX9Device->StretchRect(pSourceSurf, nullptr, pDestSurf, nullptr, D3DTEXF_NONE);
	}

	if (pSourceSurf)
	{
		pSourceSurf->Release();
	}

	if (pDestSurf)
	{
		pDestSurf->Release();
	}
}

void InternalDevice9::CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source)
{
	DX9RenderTargetView* pDestSurface = static_cast<DX9RenderTargetView*>(Dest);
	DX9RenderTargetView* pSourceSurface = static_cast<DX9RenderTargetView*>(Source);

	DX9Device->StretchRect(pSourceSurface->GetDX9Surface(), nullptr, pDestSurface->GetDX9Surface(), nullptr, D3DTEXF_NONE);
}

void InternalDevice9::SetViewport(RHIViewport& VP)
{
	D3DVIEWPORT9 D9Viewport = {};
	D9Viewport.X = static_cast<u32>(VP.TopLeftX);
	D9Viewport.Y = static_cast<u32>(VP.TopLeftY);
	D9Viewport.Width = static_cast<u32>(VP.Width);
	D9Viewport.Height = static_cast<u32>(VP.Height);
	D9Viewport.MinZ = VP.MinDepth;
	D9Viewport.MaxZ = VP.MaxDepth;

	DX9Device->SetViewport(&D9Viewport);
}

void InternalDevice9::SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology)
{
	static D3DPRIMITIVETYPE d3dTopologies[] =
	{
		D3DPT_FORCE_DWORD,    // Undefined
		D3DPT_POINTLIST,      // PointList
		D3DPT_LINELIST,       // LineList
		D3DPT_LINESTRIP,      // LineStrip
		D3DPT_TRIANGLELIST,   // TriangleList
		D3DPT_TRIANGLESTRIP,  // TriangleStrip
		D3DPT_TRIANGLEFAN     // TriangleFan
	};
	currentTopology = topology;
	d3dTopology = d3dTopologies[static_cast<size_t>(topology)];
}

void InternalDevice9::DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount)
{
	if (primitiveCount == 0)
	{
		return;
	}

	CHK_DX(DX9Device->DrawIndexedPrimitive(d3dTopology, baseVertex, startVertex, vertexCount, startIndex, primitiveCount));
}

void InternalDevice9::Draw(u32 startVertex, u32 primitiveCount)
{
	if (primitiveCount == 0)
	{
		return;
	}

	CHK_DX(DX9Device->DrawPrimitive(d3dTopology, startVertex, primitiveCount));
}

void InternalDevice9::DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation)
{
	VERIFY(!"DrawIndexedInstanced not supported in DX9");
}

void InternalDevice9::DrawNoInputAssembly(u32 vertexCount)
{
	VERIFY(!"DrawNoInputAssembly not supported in DX9");
}
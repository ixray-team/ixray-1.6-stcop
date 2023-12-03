// dx10HW.cpp: implementation of the DX10 specialisation of CHW.
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/HW.h"
#include "../../xrEngine/XR_IOConsole.h"
#include "../../Include/xrAPI/xrAPI.h"

#include "StateManager\dx10SamplerStateCache.h"
#include "StateManager\dx10StateCache.h"

#ifndef _EDITOR
void	fill_vid_mode_list			(CHW* _hw);
void	free_vid_mode_list			();

void	fill_render_mode_list		();
void	free_render_mode_list		();
#else
void	fill_vid_mode_list			(CHW* _hw)	{}
void	free_vid_mode_list			()			{}
void	fill_render_mode_list		()			{}
void	free_render_mode_list		()			{}
#endif

CHW			HW;

//	DX10: Don't neeed this?
/*
#ifdef DEBUG
IDirect3DStateBlock9*	dwDebugSB = 0;
#endif
*/

CHW::CHW() : 
//	hD3D(NULL),
	//pD3D(NULL),
	m_pAdapter(0),
	pDevice(NULL),
	m_move_window(true)
	//pBaseRT(NULL),
	//pBaseZB(NULL)
{
	Device.seqAppActivate.Add(this);
	Device.seqAppDeactivate.Add(this);

	DEVMODE dmi;
	EnumDisplaySettings(NULL, ENUM_CURRENT_SETTINGS, &dmi);
	psCurrentVidMode[0] = dmi.dmPelsWidth;
	psCurrentVidMode[1] = dmi.dmPelsHeight;
}

CHW::~CHW()
{
	Device.seqAppActivate.Remove(this);
	Device.seqAppDeactivate.Remove(this);
}
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
void CHW::CreateD3D()
{
	IDXGIFactory* pFactory = nullptr;
	R_CHK(CreateDXGIFactory(IID_PPV_ARGS(&pFactory)));

	m_pAdapter = 0;
	m_bUsePerfhud = false;

#ifndef	MASTER_GOLD
	// Look for 'NVIDIA NVPerfHUD' adapter
	// If it is present, override default settings
	UINT i = 0;
	while(pFactory->EnumAdapters(i, &m_pAdapter) != DXGI_ERROR_NOT_FOUND)
	{
		DXGI_ADAPTER_DESC desc;
		m_pAdapter->GetDesc(&desc);
		if(!wcscmp(desc.Description,L"NVIDIA PerfHUD"))
		{
			m_bUsePerfhud = true;
			break;
		}
		else
		{
			m_pAdapter->Release();
			m_pAdapter = 0;
		}
		++i;
	}
#endif	//	MASTER_GOLD

	if (!m_pAdapter)
		pFactory->EnumAdapters(0, &m_pAdapter);

	pFactory->Release();
}

void CHW::DestroyD3D()
{
	//_RELEASE					(this->pD3D);

	_SHOW_REF				("refCount:m_pAdapter",m_pAdapter);
	_RELEASE				(m_pAdapter);

//	FreeLibrary(hD3D);
}

void CHW::CreateDevice(SDL_Window* window, bool move_window )
{
	CreateRDoc();

	m_move_window = move_window;
	CreateD3D();

	// General - select adapter and device
	BOOL  bWindowed			= !psDeviceFlags.is(rsFullscreen);

	m_DriverType = Caps.bForceGPU_REF ? 
		D3D_DRIVER_TYPE_REFERENCE : D3D_DRIVER_TYPE_HARDWARE;

	if (m_bUsePerfhud)
		m_DriverType = D3D_DRIVER_TYPE_REFERENCE;

	// Display the name of video board
	DXGI_ADAPTER_DESC Desc;
	R_CHK( m_pAdapter->GetDesc(&Desc) );
	//	Warning: Desc.Description is wide string
	Msg		("* GPU [vendor:%X]-[device:%X]: %S", Desc.VendorId, Desc.DeviceId, Desc.Description);

	Caps.id_vendor	= Desc.VendorId;
	Caps.id_device	= Desc.DeviceId;

	// Select back-buffer & depth-stencil format
	D3DFORMAT&	fTarget	= Caps.fTarget;
	D3DFORMAT&	fDepth	= Caps.fDepth;

	//	HACK: DX10: Embed hard target format.
	fTarget = D3DFMT_X8R8G8B8;	//	No match in DX10. D3DFMT_A8B8G8R8->DXGI_FORMAT_R8G8B8A8_UNORM
	fDepth = selectDepthStencil(fTarget);
	// Set up the presentation parameters
	DXGI_SWAP_CHAIN_DESC	&sd	= m_ChainDesc;
	ZeroMemory				( &sd, sizeof(sd) );

	HWND hwnd = (HWND)SDL_GetProperty(SDL_GetWindowProperties(window), "SDL.window.win32.hwnd", nullptr);
	selectResolution	(sd.BufferDesc.Width, sd.BufferDesc.Height, bWindowed);
	sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	sd.BufferCount = 1;
	sd.SampleDesc.Count = 1;
	sd.SampleDesc.Quality = 0;
	sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
	sd.OutputWindow = hwnd;
	sd.Windowed = bWindowed;

	sd.BufferDesc.RefreshRate =
		selectRefresh(sd.BufferDesc.Width, sd.BufferDesc.Height, sd.BufferDesc.Format);

	//	Additional set up
	sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;

	UINT createDeviceFlags = 0;
	if (strstr(Core.Params, "-dxdebug")) {
		createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
	}

	HRESULT R;
    D3D_FEATURE_LEVEL pFeatureLevels[] =
    {
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
    };

   R =  D3D11CreateDeviceAndSwapChain(   0,//m_pAdapter,//What wrong with adapter??? We should use another version of DXGI?????
                                          m_DriverType,
                                          NULL,
                                          createDeviceFlags,
										  pFeatureLevels,
										  sizeof(pFeatureLevels)/sizeof(pFeatureLevels[0]),
										  D3D11_SDK_VERSION,
                                          &sd,
                                          &m_pSwapChain,
		                                  &pDevice,
										  &FeatureLevel,		
										  &pContext);


	if (FAILED(R))
	{
		// Fatal error! Cannot create rendering device AT STARTUP !!!
		Msg					("Failed to initialize graphics hardware.\n"
							 "Please try to restart the game.\n"
							 "CreateDevice returned 0x%08x", R
							 );
		FlushLog			();
		MessageBoxA			(NULL,"Failed to initialize graphics hardware.\nPlease try to restart the game.","Error!",MB_OK|MB_ICONERROR);
		TerminateProcess	(GetCurrentProcess(),0);
	};
	R_CHK(R);

	_SHOW_REF	("* CREATE: DeviceREF:",HW.pDevice);
	UpdateViews();

	//u32	memory									= pDevice->GetAvailableTextureMem	();
	size_t	memory									= Desc.DedicatedVideoMemory;
	Msg		("* Texture memory: %d M",		memory/(1024*1024));
#ifndef _EDITOR
	updateWindowProps							(window);
	fill_vid_mode_list							(this);
#endif
}

void CHW::CreateRDoc() {
	if (strstr(Core.Params, "-renderdoc")) {
		rdoc_api = nullptr;
		if (HMODULE mod = LoadLibraryA("renderdoc.dll")) {
			pRENDERDOC_GetAPI RENDERDOC_GetAPI = (pRENDERDOC_GetAPI)GetProcAddress(mod, "RENDERDOC_GetAPI");

			int ret = RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_5_0, (void**)&rdoc_api);
			assert(ret == 1);

			int Major, Minor, Path;
			rdoc_api->GetAPIVersion(&Major, &Minor, &Path);
			Msg("RenderDoc API: %d.%d.%d", Major, Minor, Path);

			//rdoc_api->LaunchReplayUI(1, "IX-Ray 1.6.02");
			//rdoc_api->SetActiveWindow(pDevice, Device.m_hWnd);
		}
	}

}

void CHW::DestroyDevice()
{
	//	Destroy state managers
	StateManager.Reset();
	RSManager.ClearStateArray();
	DSSManager.ClearStateArray();
	BSManager.ClearStateArray();
	SSManager.ClearStateArray();

	if (rdoc_api != nullptr) {
		rdoc_api->Shutdown();
	}

	_SHOW_REF				("refCount:pBaseZB",pBaseZB);
	_RELEASE				(pBaseZB);

	_SHOW_REF				("refCount:pBaseRT",pBaseRT);
	_RELEASE				(pBaseRT);

	//	Must switch to windowed mode to release swap chain
	if (!m_ChainDesc.Windowed) m_pSwapChain->SetFullscreenState( FALSE, NULL);
	_SHOW_REF				("refCount:m_pSwapChain",m_pSwapChain);
	_RELEASE				(m_pSwapChain);

	_RELEASE				(pContext);

	_SHOW_REF				("DeviceREF:",HW.pDevice);
	_RELEASE				(HW.pDevice);


	DestroyD3D				();

#ifndef _EDITOR
	free_vid_mode_list		();
#endif
}

//////////////////////////////////////////////////////////////////////
// Resetting device
//////////////////////////////////////////////////////////////////////
void CHW::Reset (SDL_Window* window)
{
	DXGI_SWAP_CHAIN_DESC &cd = m_ChainDesc;

	BOOL	bWindowed		= !psDeviceFlags.is	(rsFullscreen);

	cd.Windowed = bWindowed;

	m_pSwapChain->SetFullscreenState(!bWindowed, NULL);

	DXGI_MODE_DESC	&desc = m_ChainDesc.BufferDesc;

	selectResolution(desc.Width, desc.Height, bWindowed);

	desc.RefreshRate = selectRefresh(desc.Width, desc.Height, desc.Format);

	CHK_DX(m_pSwapChain->ResizeTarget(&desc));


#ifdef DEBUG
	//	_RELEASE			(dwDebugSB);
#endif
	_SHOW_REF				("refCount:pBaseZB",pBaseZB);
	_SHOW_REF				("refCount:pBaseRT",pBaseRT);

	_RELEASE(pBaseZB);
	_RELEASE(pBaseRT);

	CHK_DX(m_pSwapChain->ResizeBuffers(
		cd.BufferCount,
		desc.Width,
		desc.Height,
		desc.Format,
		DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH));

	UpdateViews();
	updateWindowProps	(window);
}

D3DFORMAT CHW::selectDepthStencil	(D3DFORMAT fTarget)
{
	// R3 hack
#pragma todo("R3 need to specify depth format")
	return D3DFMT_D24S8;
}

void CHW::selectResolution( u32 &dwWidth, u32 &dwHeight, BOOL bWindowed )
{
	fill_vid_mode_list			(this);

	if(bWindowed)
	{
		dwWidth		= psCurrentVidMode[0];
		dwHeight	= psCurrentVidMode[1];
	}
	else //check
	{
		string64					buff;
		xr_sprintf					(buff,sizeof(buff),"%dx%d",psCurrentVidMode[0],psCurrentVidMode[1]);

		if(_ParseItem(buff,vid_mode_token)==u32(-1)) //not found
		{ //select safe
			xr_sprintf				(buff,sizeof(buff),"vid_mode %s",vid_mode_token[0].name);
			Console->Execute		(buff);
		}

		dwWidth						= psCurrentVidMode[0];
		dwHeight					= psCurrentVidMode[1];
	}
}

DXGI_RATIONAL CHW::selectRefresh(u32 dwWidth, u32 dwHeight, DXGI_FORMAT fmt)
{
	// utak3r: when resizing target, let DXGI calculate the refresh rate for itself.
	// This is very important for performance, this value is correct.
	DXGI_RATIONAL refresh;
	refresh.Numerator = 0;
	refresh.Denominator = 1;
	return refresh;
}

void CHW::OnAppActivate()
{
	if ( m_pSwapChain && !m_ChainDesc.Windowed )
	{
		ShowWindow( m_ChainDesc.OutputWindow, SW_RESTORE );
		m_pSwapChain->SetFullscreenState( TRUE, NULL );
	}
}

void CHW::OnAppDeactivate()
{
	if ( m_pSwapChain && !m_ChainDesc.Windowed )
	{
		m_pSwapChain->SetFullscreenState( FALSE, NULL );
		ShowWindow( m_ChainDesc.OutputWindow, SW_MINIMIZE );
	}
}


BOOL CHW::support( D3DFORMAT fmt, DWORD type, DWORD usage)
{
	//	TODO: DX10: implement stub for this code.
	VERIFY(!"Implement CHW::support");
	return TRUE;
}

void CHW::updateWindowProps(SDL_Window* window)
{
	if (!psDeviceFlags.is(rsFullscreen)) {
		const bool Centered = strstr(Core.Params, "-no_center_screen") == nullptr;
		SDL_SetWindowSize(window, m_ChainDesc.BufferDesc.Width, m_ChainDesc.BufferDesc.Height);
		SDL_SetWindowBordered(window, m_move_window);
		SDL_SetWindowPosition(window, Centered ? SDL_WINDOWPOS_CENTERED : 0, Centered ? SDL_WINDOWPOS_CENTERED : 0);
	} else {
		SDL_SetWindowBordered(window, false);
	}

	SDL_HideCursor();
	SDL_RaiseWindow(window);
}


struct _uniq_mode
{
	_uniq_mode(LPCSTR v):_val(v){}
	LPCSTR _val;
	bool operator() (LPCSTR _other) {return !_stricmp(_val,_other);}
};

#ifndef _EDITOR

void free_vid_mode_list()
{
	for( int i=0; vid_mode_token[i].name; i++ )
	{
		xr_free					(vid_mode_token[i].name);
	}
	xr_free						(vid_mode_token);
	vid_mode_token				= NULL;
}

void fill_vid_mode_list(CHW* _hw)
{
	if(vid_mode_token != NULL)		return;
	xr_vector<LPCSTR>	_tmp;
	xr_vector<DXGI_MODE_DESC>	modes;

	IDXGIOutput *pOutput;
	//_hw->m_pSwapChain->GetContainingOutput(&pOutput);
	_hw->m_pAdapter->EnumOutputs(0, &pOutput);
	VERIFY(pOutput);

	UINT num = 0;
	DXGI_FORMAT format = DXGI_FORMAT_R8G8B8A8_UNORM;
	UINT flags         = 0;

	// Get the number of display modes available
	pOutput->GetDisplayModeList( format, flags, &num, 0);

	// Get the list of display modes
	modes.resize(num);
	pOutput->GetDisplayModeList( format, flags, &num, &modes.front());

	_RELEASE(pOutput);

	for (u32 i=0; i<num; ++i)
	{
		DXGI_MODE_DESC &desc = modes[i];
		string32		str;

		if(desc.Width < 800)
			continue;

		xr_sprintf(str, sizeof(str), "%dx%d", desc.Width, desc.Height);

		if(_tmp.end() != std::find_if(_tmp.begin(), _tmp.end(), _uniq_mode(str)))
			continue;

		_tmp.push_back				(NULL);
		_tmp.back()					= xr_strdup(str);
	}
	
	u32 _cnt						= (u32)_tmp.size()+1;

	vid_mode_token					= xr_alloc<xr_token>(_cnt);

	vid_mode_token[_cnt-1].id			= -1;
	vid_mode_token[_cnt-1].name		= NULL;

#ifdef DEBUG
	Msg("Available video modes[%d]:",_tmp.size());
#endif // DEBUG
	for( u32 i=0; i<_tmp.size(); ++i )
	{
		vid_mode_token[i].id		= i;
		vid_mode_token[i].name		= _tmp[i];
#ifdef DEBUG
		Msg							("[%s]",_tmp[i]);
#endif // DEBUG
	}
}

void CHW::UpdateViews()
{
	DXGI_SWAP_CHAIN_DESC &sd = m_ChainDesc;
	HRESULT R;

	// Create a render target view
	ID3DTexture2D* pBuffer = nullptr;
	R = m_pSwapChain->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
	R_CHK(R);

	R = pDevice->CreateRenderTargetView( pBuffer, NULL, &pBaseRT);
	pBuffer->Release();
	R_CHK(R);

	//	Create Depth/stencil buffer
	ID3DTexture2D* pDepthStencil = nullptr;
	D3D_TEXTURE2D_DESC descDepth = {};
	descDepth.Width = sd.BufferDesc.Width;
	descDepth.Height = sd.BufferDesc.Height;
	descDepth.MipLevels = 1;
	descDepth.ArraySize = 1;
	descDepth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT; // DXGI_FORMAT_D32_FLOAT_S8X24_UINT
	descDepth.SampleDesc.Count = 1;
	descDepth.SampleDesc.Quality = 0;
	descDepth.Usage = D3D_USAGE_DEFAULT;
	descDepth.BindFlags = D3D_BIND_DEPTH_STENCIL;
	descDepth.CPUAccessFlags = 0;
	descDepth.MiscFlags = 0;
	R = pDevice->CreateTexture2D( &descDepth,       // Texture desc
		NULL,                  // Initial data
		&pDepthStencil ); // [out] Texture
	R_CHK(R);

	//	Create Depth/stencil view
	D3D_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc = {};
	depthStencilViewDesc.Format = descDepth.Format;
	depthStencilViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2D;
	depthStencilViewDesc.Texture2D.MipSlice = 0;
	R = pDevice->CreateDepthStencilView(pDepthStencil, &depthStencilViewDesc, &pBaseZB);
	R_CHK(R);

	pDepthStencil->Release();
}
#endif

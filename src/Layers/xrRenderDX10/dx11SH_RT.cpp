#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/ResourceManager.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "dx10TextureUtils.h"

CRT::CRT			()
{
	pSurface		= nullptr;
	pRT				= nullptr;
	pZRT			= nullptr;
	pUAView			= nullptr;
	dwWidth			= 0;
	dwHeight		= 0;
	fmt = DxgiFormat::DXGI_FORMAT_UNKNOWN;
}
CRT::~CRT			()
{
	destroy			();

	// release external reference
	DEV->_DeleteRT	(this);
}

void CRT::create	(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, bool useUAV )
{
	if (pSurface)	return;

	R_ASSERT	(RDevice && Name && Name[0] && w && h);
	_order		= CPU::GetCLK();

	dwWidth		= w;
	dwHeight	= h;
	fmt			= f;

	// Check width-and-height of render target surface
	if (w > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION)		return;
	if (h > D3D_REQ_TEXTURE2D_U_OR_V_DIMENSION)		return;

	// Select usage
	u32 usage = 0;
	if (DxgiFormat::DXGI_FORMAT_D24_UNORM_S8_UINT == fmt)		    usage = D3DUSAGE_DEPTHSTENCIL;
	else if (DxgiFormat::DXGI_FORMAT_R24_UNORM_X8_TYPELESS == fmt)	usage = D3DUSAGE_DEPTHSTENCIL;
	else if (DxgiFormat::DXGI_FORMAT_D16_UNORM == fmt) {
		usage = D3DUSAGE_DEPTHSTENCIL;
		fmt = DxgiFormat::DXGI_FORMAT_R16_TYPELESS;
	}
	else if (DxgiFormat::DXGI_FORMAT_D32_FLOAT == fmt) {
		usage = D3DUSAGE_DEPTHSTENCIL;
		fmt = DxgiFormat::DXGI_FORMAT_R32_TYPELESS;
	}
	else if (DxgiFormat::DXGI_FORMAT_R24G8_TYPELESS == fmt)			usage = D3DUSAGE_DEPTHSTENCIL;
	else														    usage = D3DUSAGE_RENDERTARGET;

	DXGI_FORMAT dx10FMT = (DXGI_FORMAT)fmt;
	bool bUseAsDepth = (usage == D3DUSAGE_RENDERTARGET) ? false : true;

	ERHITextureFormat rhiFormat = FMT_UNKNOWN;

	// Format conversion
	// #TODO: Please remove or refactor
	switch (dx10FMT)
	{
	case DXGI_FORMAT_UNKNOWN:						rhiFormat = FMT_UNKNOWN; break;
	case DXGI_FORMAT_B8G8R8A8_UNORM:				rhiFormat = FMT_R8G8B8A8; break;
	case DXGI_FORMAT_R8G8_UNORM:					rhiFormat = FMT_R8G8; break;
	case DXGI_FORMAT_R8G8B8A8_UNORM:				rhiFormat = FMT_B8G8R8A8; break;
	case DXGI_FORMAT_B5G6R5_UNORM:					rhiFormat = FMT_R5G6B5; break;
	case DXGI_FORMAT_R16G16_UNORM:					rhiFormat = FMT_G16R16; break;
	case DXGI_FORMAT_R16G16B16A16_UNORM:			rhiFormat = FMT_A16B16G16R16; break;
	case DXGI_FORMAT_R8_UNORM:						rhiFormat = FMT_L8; break; 
	case DXGI_FORMAT_R8G8_SNORM:					rhiFormat = FMT_V8U8; break;
	case DXGI_FORMAT_R8G8B8A8_SNORM:				rhiFormat = FMT_Q8W8V8U8; break;
	case DXGI_FORMAT_R16G16_SNORM:					rhiFormat = FMT_V16U16; break;
	case DXGI_FORMAT_R24G8_TYPELESS:				rhiFormat = FMT_D24X8; break;
	case DXGI_FORMAT_D24_UNORM_S8_UINT:				rhiFormat = FMT_D24S8; break;
	case DXGI_FORMAT_R32_TYPELESS:					rhiFormat = FMT_D32F_LOCKABLE; break;
	case DXGI_FORMAT_R16G16_FLOAT:					rhiFormat = FMT_G16R16F; break;
	case DXGI_FORMAT_R16G16B16A16_FLOAT:			rhiFormat = FMT_A16B16G16R16F; break;
	case DXGI_FORMAT_R32_FLOAT:						rhiFormat = FMT_R32F; break;
	case DXGI_FORMAT_R16_FLOAT:						rhiFormat = FMT_R16F; break;
	case DXGI_FORMAT_R32G32B32A32_FLOAT:			rhiFormat = FMT_A32B32G32R32F; break;
	case DXGI_FORMAT_G8R8_G8B8_UNORM:				rhiFormat = FMT_R8G8_B8G8; break;
	case DXGI_FORMAT_R8G8_B8G8_UNORM:				rhiFormat = FMT_G8R8_G8B8; break;
	case DXGI_FORMAT_BC1_UNORM:						rhiFormat = FMT_DXT1; break;
	case DXGI_FORMAT_BC2_UNORM:						rhiFormat = FMT_DXT3; break;
	case DXGI_FORMAT_BC3_UNORM:						rhiFormat = FMT_DXT5; break;
	default:
		FATAL("Unknowed or unsupport format");
		break;
	}

	// Try to create texture/surface
	DEV->Evict				();
	// Create the render target texture
	STexture2DDesc desc;
	ZeroMemory( &desc, sizeof(desc) );
	desc.Width = dwWidth;
	desc.Height = dwHeight;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = rhiFormat;
	desc.SampleDesc.Count = SampleCount;
	desc.Usage = USAGE_DEFAULT;

	if (bUseAsDepth)
		desc.IsDepthStencil = true;
	else
		desc.IsRenderTarget = true;


	// #TODO: RHI - Mess up with this shit
#if 0
   if( SampleCount <= 1 )
	   desc.BindFlags = D3D_BIND_SHADER_RESOURCE | (bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : D3D_BIND_RENDER_TARGET);
   else
   {
      desc.BindFlags = (bUseAsDepth ? D3D_BIND_DEPTH_STENCIL : (D3D_BIND_SHADER_RESOURCE | D3D_BIND_RENDER_TARGET));
   }
#endif

   // #TODO: RHI - UNORDERED_ACCESS

#if 0
	if (!bUseAsDepth && SampleCount == 1 && useUAV )
		desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;
#endif

	desc.NoShaderResourceView = true;

	pSurface = g_RenderRHI->CreateTexture2D( desc, nullptr );

	// OK
#ifdef DEBUG
	Msg			("* created RT(%s), %dx%d, format = %d samples = %d",Name,w,h, dx10FMT, SampleCount );
#endif // DEBUG
	//R_CHK		(pSurface->GetSurfaceLevel	(0,&pRT));
	if (bUseAsDepth)
	{
		SDepthStencilViewDesc	ViewDesc;
		ZeroMemory( &ViewDesc, sizeof(ViewDesc) );

		ViewDesc.Format = FMT_UNKNOWN;
		if( SampleCount <= 1 )
		{
			ViewDesc.ViewDimension = DSV_DIMENSION_TEXTURE2D;
		}
		else
		{
			FATAL("Multisample render target is unsuppoerted");
		}


		// #TODO: RHI - MULTISAMPLE TEXTURES
#if 0
		else
		{
			ViewDesc.ViewDimension = D3D_DSV_DIMENSION_TEXTURE2DMS;
			ViewDesc.Texture2DMS.UnusedField_NothingToDefine = 0;
		}
#endif

		ViewDesc.Texture2D.MipSlice = 0;
	
		// #TODO: RHI - Format naming mess up
		switch (desc.Format)
		{
		case FMT_D24X8:
			ViewDesc.Format = FMT_D24S8;
			break;
		case DXGI_FORMAT_R32_TYPELESS:
			ViewDesc.Format = FMT_D32F_LOCKABLE;
			break;
		}

		pZRT = g_RenderRHI->CreateDepthStencilView( pSurface, &ViewDesc );
	}
	else
	{
		pRT = g_RenderRHI->CreateRenderTargetView(pSurface, 0);
	}
		

	// #TODO: RHI - UNORDERED_ACCESS
#if 0
	if (!bUseAsDepth &&  SampleCount == 1 && useUAV)
    {
	    D3D11_UNORDERED_ACCESS_VIEW_DESC UAVDesc;
		ZeroMemory( &UAVDesc, sizeof( D3D11_UNORDERED_ACCESS_VIEW_DESC ) );
		UAVDesc.Format = dx10FMT;
		UAVDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
		UAVDesc.Buffer.FirstElement = 0;
		UAVDesc.Buffer.NumElements = dwWidth * dwHeight;
		CHK_DX( RDevice->CreateUnorderedAccessView( pSurface, &UAVDesc, &pUAView ) );
    }
#endif

	pTexture	= DEV->_CreateTexture	(Name);
	pTexture->surface_set(pSurface);
}

void CRT::destroy		()
{
	if (pTexture._get())	{
		pTexture->surface_set	(0);
		pTexture				= nullptr;
	}
	_RELEASE	(pRT		);
	_RELEASE	(pZRT		);
	
	_RELEASE	(pSurface	);
	_RELEASE	(pUAView);
}
void CRT::reset_begin	()
{
	destroy		();
}
void CRT::reset_end		()
{
	create		(*cName,dwWidth,dwHeight,fmt);
}

void resptrcode_crt::create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, bool useUAV )
{
	_set			(DEV->_CreateRT(Name,w,h,f, SampleCount, useUAV ));
}

//////////////////////////////////////////////////////////////////////////
/*	DX10 cut
CRTC::CRTC			()
{
	if (pSurface)	return;

	pSurface									= nullptr;
	pRT[0]=pRT[1]=pRT[2]=pRT[3]=pRT[4]=pRT[5]	= nullptr;
	dwSize										= 0;
	fmt											= D3DFMT_UNKNOWN;
}
CRTC::~CRTC			()
{
	destroy			();

	// release external reference
	DEV->_DeleteRTC	(this);
}

void CRTC::create	(LPCSTR Name, u32 size,	D3DFORMAT f)
{
	R_ASSERT	(RDevice && Name && Name[0] && size && btwIsPow2(size));
	_order		= CPU::GetCLK();	//Device.GetTimerGlobal()->GetElapsed_clk();

	HRESULT		_hr;

	dwSize		= size;
	fmt			= f;

	// Get caps
	//D3DCAPS9	caps;
	//R_CHK		(RDevice->GetDeviceCaps(&caps));

	//	DirectX 10 supports non-power of two textures
	// Pow2
	//if (!btwIsPow2(size))
	//{
	//	if (!HW.Caps.raster.bNonPow2)	return;
	//}

	// Check width-and-height of render target surface
	if (size>D3Dxx_REQ_TEXTURECUBE_DIMENSION)		return;

	//	TODO: DX10: Validate cube texture format
	// Validate render-target usage
	//_hr = HW.pD3D->CheckDeviceFormat(
	//	HW.DevAdapter,
	//	HW.m_DriverType,
	//	HW.Caps.fTarget,
	//	D3DUSAGE_RENDERTARGET,
	//	D3DRTYPE_CUBETEXTURE,
	//	f
	//	);
	//if (FAILED(_hr))					return;

	// Try to create texture/surface
	DEV->Evict					();
	_hr = RDevice->CreateCubeTexture	(size, 1, D3DUSAGE_RENDERTARGET, f, D3DPOOL_DEFAULT, &pSurface,nullptr);
	if (FAILED(_hr) || (0==pSurface))	return;

	// OK
	Msg			("* created RTc(%s), 6(%d)",Name,size);
	for (u32 face=0; face<6; face++)
		R_CHK	(pSurface->GetCubeMapSurface	((D3DCUBEMAP_FACES)face, 0, pRT+face));
	pTexture	= DEV->_CreateTexture	(Name);
	pTexture->surface_set						(pSurface);
}

void CRTC::destroy		()
{
	pTexture->surface_set	(0);
	pTexture				= nullptr;
	for (u32 face=0; face<6; face++)
		_RELEASE	(pRT[face]	);
	_RELEASE	(pSurface	);
}
void CRTC::reset_begin	()
{
	destroy		();
}
void CRTC::reset_end	()
{
	create		(*cName,dwSize,fmt);
}

void resptrcode_crtc::create(LPCSTR Name, u32 size, D3DFORMAT f)
{
	_set		(DEV->_CreateRTC(Name,size,f));
}
*/
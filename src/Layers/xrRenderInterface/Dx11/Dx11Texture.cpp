#include "stdafx.h"
#include "Dx11API.h"
#include <d3d11.h>

#include "Dx11Texture.h"
#include "../DxGI/Converter.h"

D3D11_MAP GetD3D11Map(eLockType lockType);

struct DX11TextureFormatPairs
{
	ERHITextureFormat RHIFormat;
	DXGI_FORMAT	DX9Format;
};

static DX11TextureFormatPairs TextureFormatList[] =
{
	{FMT_UNKNOWN,        DXGI_FORMAT_UNKNOWN},
	{FMT_A8R8G8B8,       DXGI_FORMAT_B8G8R8A8_UNORM },
	{FMT_R8G8B8,		 DXGI_FORMAT_R8G8B8A8_UNORM		},
	{FMT_X8R8G8B8,		 DXGI_FORMAT_B8G8R8X8_UNORM 		},
	{FMT_R5G6B5,         DXGI_FORMAT_B5G6R5_UNORM },
	{FMT_A8B8G8R8,       DXGI_FORMAT_R8G8B8A8_UNORM},
	{FMT_G16R16,         DXGI_FORMAT_R16G16_UNORM },
	{FMT_A16B16G16R16,   DXGI_FORMAT_R16G16B16A16_UNORM },
	{FMT_L8,             DXGI_FORMAT_R8_UNORM},
	{FMT_V8U8,           DXGI_FORMAT_R8G8_SNORM },
	{FMT_Q8W8V8U8,       DXGI_FORMAT_R8G8B8A8_SNORM },
	{FMT_V16U16,         DXGI_FORMAT_R16G16_SNORM },
	{FMT_D24X8,          DXGI_FORMAT_R24G8_TYPELESS },
	{FMT_D32F_LOCKABLE,  DXGI_FORMAT_R32_TYPELESS },
	{FMT_G16R16F,        DXGI_FORMAT_R16G16_FLOAT},
	{FMT_A16B16G16R16F,  DXGI_FORMAT_R16G16B16A16_FLOAT },
	{FMT_R32F,			 DXGI_FORMAT_R32_FLOAT },
	{FMT_R16F,			 DXGI_FORMAT_R16_FLOAT },
	{FMT_A32B32G32R32F,  DXGI_FORMAT_R32G32B32A32_FLOAT },

	{ FMT_UYVY      ,   DXGI_FORMAT_UNKNOWN },
    { FMT_R8G8_B8G8 ,   DXGI_FORMAT_G8R8_G8B8_UNORM },
    { FMT_YUY2      ,   DXGI_FORMAT_UNKNOWN },
    { FMT_G8R8_G8B8 ,   DXGI_FORMAT_R8G8_B8G8_UNORM },
    { FMT_DXT1      ,   DXGI_FORMAT_BC1_UNORM },
    { FMT_DXT2      ,   DXGI_FORMAT_BC2_UNORM },
    { FMT_DXT3      ,   DXGI_FORMAT_BC2_UNORM },
    { FMT_DXT4      ,   DXGI_FORMAT_BC3_UNORM },
    { FMT_DXT5      ,   DXGI_FORMAT_BC3_UNORM },
};

static DXGI_FORMAT ConvertTextureFormat(ERHITextureFormat dx9FMT)
{
	int arrayLength = sizeof(TextureFormatList) / sizeof(TextureFormatList[0]);
	for (int i = 0; i < arrayLength; ++i)
	{
		if (TextureFormatList[i].RHIFormat == dx9FMT)
			return TextureFormatList[i].DX9Format;
	}

	VERIFY(!"ConvertTextureFormat didn't find appropriate dx10 texture format!");
	return DXGI_FORMAT_UNKNOWN;
}

ERHITextureFormat ConvertTextureFormatAPI(DXGI_FORMAT dx9FMT)
{
	int arrayLength = sizeof(TextureFormatList) / sizeof(TextureFormatList[0]);
	for (int i = 0; i < arrayLength; ++i)
	{
		if (TextureFormatList[i].DX9Format == dx9FMT)
			return TextureFormatList[i].RHIFormat;
	}

	VERIFY(!"ConvertTextureFormat didn't find appropriate dx10 texture format!");
	return FMT_UNKNOWN;
}

CD3D11Texture2D::CD3D11Texture2D() :
	m_pTexture(nullptr),
	m_pTextureSRV(nullptr),
	m_Pitch(0)
{
	memset(&m_TextureDesc, 0, sizeof(m_TextureDesc));
}

CD3D11Texture2D::~CD3D11Texture2D()
{
	if (m_pTextureSRV)
	{
		m_pTextureSRV->Release();
		m_pTextureSRV = nullptr;
	}

	if (m_pTexture)
	{
		m_pTexture->Release();
		m_pTexture = nullptr;
	}
}

HRESULT CD3D11Texture2D::Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData)
{
	R_ASSERT(pTextureDesc);

	m_TextureDesc = *pTextureDesc;
	m_Pitch = pSubresourceData->SysMemPitch;

	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	D3D11_TEXTURE2D_DESC d3dTextureDesc = {};
	d3dTextureDesc.Width = pTextureDesc->Width;
	d3dTextureDesc.Height = pTextureDesc->Height;
	d3dTextureDesc.MipLevels = pTextureDesc->NumMips;
	d3dTextureDesc.ArraySize = pTextureDesc->DepthOrSliceNum;
	d3dTextureDesc.Format = ConvertTextureFormat(pTextureDesc->Format);
	d3dTextureDesc.SampleDesc.Count = 1;
	d3dTextureDesc.Usage = D3D11_USAGE_DEFAULT;
	d3dTextureDesc.BindFlags |= D3D11_BIND_SHADER_RESOURCE;

	if ((pTextureDesc->TextureFlags & eTextureRenderTarget) != 0)
		d3dTextureDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;

	if ((pTextureDesc->TextureFlags & eTextureDepthStencil) != 0)
		d3dTextureDesc.BindFlags |= D3D11_BIND_DEPTH_STENCIL;

	d3dTextureDesc.CPUAccessFlags = 0;
	d3dTextureDesc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subresourceData = {};
	subresourceData.pSysMem = pSubresourceData->pSysMem;
	subresourceData.SysMemPitch = pSubresourceData->SysMemPitch;
	subresourceData.SysMemSlicePitch = pSubresourceData->SysMemSlicePitch;

	R_CHK(pDevice->CreateTexture2D(&d3dTextureDesc, pSubresourceData ? &subresourceData : NULL, &m_pTexture));

	if ((d3dTextureDesc.BindFlags & D3D11_BIND_SHADER_RESOURCE) != 0)
	{
		D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc = {};
		DXGI_FORMAT typelessFormat = ConvertToTypelessFmt(d3dTextureDesc.Format);
		DXGI_FORMAT srvFormat = ConvertToShaderResourceFmt(typelessFormat);
		shaderResourceViewDesc.Format = typelessFormat == srvFormat ? d3dTextureDesc.Format : srvFormat;
		shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
		shaderResourceViewDesc.Texture2D.MipLevels = -1;
		shaderResourceViewDesc.Texture2D.MostDetailedMip = 0;

		R_ASSERT(pDevice->CreateShaderResourceView(m_pTexture, &shaderResourceViewDesc, &m_pTextureSRV));
	}

	return S_OK;
}

bool CD3D11Texture2D::LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	D3D11_TEXTURE2D_DESC desc = {};
	m_pTexture->GetDesc(&desc);

	R_ASSERT2(desc.BindFlags == 0, "Failed to lock staging or static texture!");

	// Map command 
	D3D11_MAPPED_SUBRESOURCE mappedSubresource = {};
	HRESULT hr = pImmediateContext->Map(m_pTexture, D3D11CalcSubresource(Level, 0, 0), GetD3D11Map(Flags), 0, &mappedSubresource);
	if (FAILED(hr))
	{
		Msg("CD3D11Texture2D::Lock: Failed to lock texture. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	pLockedRect->Pitch = mappedSubresource.RowPitch;
	pLockedRect->pBits = mappedSubresource.pData;

	if (pRect)
	{
		u32 offset = pRect->top * mappedSubresource.RowPitch;
		offset += BitsPerPixel(desc.Format) * pRect->left;
		pLockedRect->pBits = ((byte*)pLockedRect->pBits) + offset;
	}

	return true;
}

bool CD3D11Texture2D::UnlockRect(u32 Level)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->Unmap(m_pTexture, 0);

	return true;
}

void CD3D11Texture2D::SetStage(u32 Stage)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	if (m_pTextureSRV)
		pImmediateContext->PSSetShaderResources(Stage, 1, &m_pTextureSRV);
}

Ivector2 CD3D11Texture2D::GetTextureSize() const
{
	return Ivector2();
}

EResourceType CD3D11Texture2D::GetType()
{
	return eResourceTexture;
}

u32 CD3D11Texture2D::GetLevelCount()
{
	return m_TextureDesc.NumMips;
}

bool CD3D11Texture2D::GetSurfaceLevel(u32 Level, LPIRHISURFACE* ppSurfaceLevel)
{
	return false;
}

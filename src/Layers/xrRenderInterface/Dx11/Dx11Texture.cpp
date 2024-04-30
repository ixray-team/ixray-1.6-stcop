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
	{UNKNOWN,        DXGI_FORMAT_UNKNOWN},
	{A8R8G8B8,       DXGI_FORMAT_R8G8B8A8_UNORM},
	{R8G8B8,		 DXGI_FORMAT_R8G8B8A8_UNORM		},
	{R5G6B5,         DXGI_FORMAT_R8G8B8A8_UNORM},
	{A8B8G8R8,       DXGI_FORMAT_R8G8B8A8_UNORM},
	{G16R16,         DXGI_FORMAT_R16G16_UNORM },
	{A16B16G16R16,   DXGI_FORMAT_R16G16B16A16_UNORM },
	{L8,             DXGI_FORMAT_R8_UNORM},
	{V8U8,           DXGI_FORMAT_R8G8_SNORM },
	{Q8W8V8U8,       DXGI_FORMAT_R8G8B8A8_SNORM },
	{V16U16,         DXGI_FORMAT_R16G16_SNORM },
	{D24X8,          DXGI_FORMAT_R24G8_TYPELESS },
	{D32F_LOCKABLE,  DXGI_FORMAT_R32_TYPELESS },
	{G16R16F,        DXGI_FORMAT_R16G16_FLOAT},
	{A16B16G16R16F,  DXGI_FORMAT_R16G16B16A16_FLOAT },
	{R32F,			 DXGI_FORMAT_R32_FLOAT },
	{R16F,			 DXGI_FORMAT_R16_FLOAT },
	{A32B32G32R32F,  DXGI_FORMAT_R32G32B32A32_FLOAT },
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

HRESULT CD3D11Texture2D::Create(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch)
{
	R_ASSERT(pTextureDesc);

	m_TextureDesc = *pTextureDesc;
	m_Pitch = Pitch;

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
	subresourceData.pSysMem = pData;
	subresourceData.SysMemPitch = Pitch;

	R_ASSERT(pDevice->CreateTexture2D(&d3dTextureDesc, pData ? &subresourceData : NULL, &m_pTexture));

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
}

#include "stdafx.h"
#include <d3d11.h>
#include "../../../xrEngine/ICore_GPU.h"
#include <renderdoc/api/app/renderdoc_app.h>
#include "Dx11API.h"

UINT GetD3D11BindFlags(eBufferType bufferType)
{
	switch (bufferType)
	{
	case eVertexBuffer:
		return D3D11_BIND_VERTEX_BUFFER;
	case eIndexBuffer:
		return D3D11_BIND_INDEX_BUFFER;
	case eConstantBuffer:
		return D3D11_BIND_CONSTANT_BUFFER;
	}

	return 0;
}

D3D11_MAP GetD3D11Map(eLockType lockType)
{
	switch (lockType)
	{
	case eLOCK_DISCARD:
		return D3D11_MAP_WRITE_DISCARD;
	case eLOCK_NOOVERWRITE:
		return D3D11_MAP_WRITE_NO_OVERWRITE;
	case eLOCK_READONLY:
		return D3D11_MAP_READ;

		// #TODO: To Implement
	case eLOCK_NO_DIRTY_UPDATE:
	case eLOCK_NOSYSLOCK:
	default:
		break;
	}

	R_ASSERT(0);
	return (D3D11_MAP)0;
}

DXGI_FORMAT ConvertToDepthStencilFormat(DXGI_FORMAT nFormat)
{
	switch (nFormat)
	{
	case DXGI_FORMAT_R24G8_TYPELESS:
		return DXGI_FORMAT_D24_UNORM_S8_UINT;
	case DXGI_FORMAT_R32G8X24_TYPELESS:
		return DXGI_FORMAT_D32_FLOAT_S8X24_UINT;
		//	case DXGI_FORMAT_R32G32_TYPELESS:   
		//		return DXGI_FORMAT_D32_FLOAT_S8X24_UINT;
	case DXGI_FORMAT_R32_TYPELESS:
		return DXGI_FORMAT_D32_FLOAT;
	case DXGI_FORMAT_R16_TYPELESS:
		return DXGI_FORMAT_D16_UNORM;
	case DXGI_FORMAT_D32_FLOAT:
		return DXGI_FORMAT_D32_FLOAT;
	default:
		return nFormat;
		break;
	}
}

DXGI_FORMAT ConvertToShaderResourceFmt(DXGI_FORMAT nFormat)
{
	//handle special cases
	switch (nFormat)
	{
	case DXGI_FORMAT_R24G8_TYPELESS:
		return DXGI_FORMAT_R24_UNORM_X8_TYPELESS;
	case DXGI_FORMAT_R32G8X24_TYPELESS:
		return DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS;
		//	case DXGI_FORMAT_R32G32_TYPELESS:   
		//		return DXGI_FORMAT_D32_FLOAT_S8X24_UINT;
	case DXGI_FORMAT_R32_TYPELESS:
		return DXGI_FORMAT_R32_FLOAT;
	case DXGI_FORMAT_R16_TYPELESS:
		return DXGI_FORMAT_R16_UNORM;
	default:
		return nFormat;	
	}
}

DXGI_FORMAT ConvertToTypelessFmt(DXGI_FORMAT fmt)
{
	switch (fmt)
	{
	case DXGI_FORMAT_R32G32B32A32_FLOAT:
	case DXGI_FORMAT_R32G32B32A32_UINT:
	case DXGI_FORMAT_R32G32B32A32_SINT:
		return DXGI_FORMAT_R32G32B32A32_TYPELESS;
		break;

	case DXGI_FORMAT_R32G32B32_FLOAT:
	case DXGI_FORMAT_R32G32B32_UINT:
	case DXGI_FORMAT_R32G32B32_SINT:
		return DXGI_FORMAT_R32G32B32_TYPELESS;
		break;

	case DXGI_FORMAT_R16G16B16A16_FLOAT:
	case DXGI_FORMAT_R16G16B16A16_UNORM:
	case DXGI_FORMAT_R16G16B16A16_UINT:
	case DXGI_FORMAT_R16G16B16A16_SNORM:
	case DXGI_FORMAT_R16G16B16A16_SINT:
		return DXGI_FORMAT_R16G16B16A16_TYPELESS;
		break;

	case DXGI_FORMAT_R32G32_FLOAT:
	case DXGI_FORMAT_R32G32_UINT:
	case DXGI_FORMAT_R32G32_SINT:
		return DXGI_FORMAT_R32G32_TYPELESS;
		break;

	case DXGI_FORMAT_R10G10B10A2_UNORM:
	case DXGI_FORMAT_R10G10B10A2_UINT:
		return DXGI_FORMAT_R10G10B10A2_TYPELESS;
		break;

	case DXGI_FORMAT_R8G8B8A8_UNORM:
	case DXGI_FORMAT_R8G8B8A8_UNORM_SRGB:
	case DXGI_FORMAT_R8G8B8A8_UINT:
	case DXGI_FORMAT_R8G8B8A8_SNORM:
	case DXGI_FORMAT_R8G8B8A8_SINT:
		return DXGI_FORMAT_R8G8B8A8_TYPELESS;
		break;

	case DXGI_FORMAT_BC1_UNORM:
	case DXGI_FORMAT_BC1_UNORM_SRGB:
		return DXGI_FORMAT_BC1_TYPELESS;
		break;

	case DXGI_FORMAT_BC2_UNORM:
	case DXGI_FORMAT_BC2_UNORM_SRGB:
		return DXGI_FORMAT_BC2_TYPELESS;
		break;

	case DXGI_FORMAT_BC3_UNORM:
	case DXGI_FORMAT_BC3_UNORM_SRGB:
		return DXGI_FORMAT_BC3_TYPELESS;
		break;
	default:
		//ASSERT(false);
		return fmt;
	}
}

DXGI_FORMAT GetDxgiFormat(PixelFormat format)
{
	switch (format)
	{
	case FMT_R16G16B16FA16F:
		return DXGI_FORMAT_R16G16B16A16_FLOAT;
	case FMT_DEPTH24_STENCIL8:
		return DXGI_FORMAT_D24_UNORM_S8_UINT;
	case FMT_DEPTH32:
		return DXGI_FORMAT_D32_FLOAT;
	case FMT_R8G8B8:
		return DXGI_FORMAT_R8G8B8A8_UNORM;
	case FMT_R8G8B8A8:
		return DXGI_FORMAT_R8G8B8A8_UNORM;
	case FMT_R32G32B32FA32F:
		return DXGI_FORMAT_R32G32B32A32_FLOAT;
	default:
		break;
	}

	R_ASSERT2(0, "Unknowed image format passed");

	return DXGI_FORMAT_UNKNOWN;
}

size_t BitsPerPixel(DXGI_FORMAT fmt)
{
	switch (static_cast<int>(fmt))
	{
	case DXGI_FORMAT_R32G32B32A32_TYPELESS:
	case DXGI_FORMAT_R32G32B32A32_FLOAT:
	case DXGI_FORMAT_R32G32B32A32_UINT:
	case DXGI_FORMAT_R32G32B32A32_SINT:
		return 128;

	case DXGI_FORMAT_R32G32B32_TYPELESS:
	case DXGI_FORMAT_R32G32B32_FLOAT:
	case DXGI_FORMAT_R32G32B32_UINT:
	case DXGI_FORMAT_R32G32B32_SINT:
		return 96;

	case DXGI_FORMAT_R16G16B16A16_TYPELESS:
	case DXGI_FORMAT_R16G16B16A16_FLOAT:
	case DXGI_FORMAT_R16G16B16A16_UNORM:
	case DXGI_FORMAT_R16G16B16A16_UINT:
	case DXGI_FORMAT_R16G16B16A16_SNORM:
	case DXGI_FORMAT_R16G16B16A16_SINT:
	case DXGI_FORMAT_R32G32_TYPELESS:
	case DXGI_FORMAT_R32G32_FLOAT:
	case DXGI_FORMAT_R32G32_UINT:
	case DXGI_FORMAT_R32G32_SINT:
	case DXGI_FORMAT_R32G8X24_TYPELESS:
	case DXGI_FORMAT_D32_FLOAT_S8X24_UINT:
	case DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS:
	case DXGI_FORMAT_X32_TYPELESS_G8X24_UINT:
	case DXGI_FORMAT_Y416:
	case DXGI_FORMAT_Y210:
	case DXGI_FORMAT_Y216:
		return 64;

	case DXGI_FORMAT_R10G10B10A2_TYPELESS:
	case DXGI_FORMAT_R10G10B10A2_UNORM:
	case DXGI_FORMAT_R10G10B10A2_UINT:
	case DXGI_FORMAT_R11G11B10_FLOAT:
	case DXGI_FORMAT_R8G8B8A8_TYPELESS:
	case DXGI_FORMAT_R8G8B8A8_UNORM:
	case DXGI_FORMAT_R8G8B8A8_UNORM_SRGB:
	case DXGI_FORMAT_R8G8B8A8_UINT:
	case DXGI_FORMAT_R8G8B8A8_SNORM:
	case DXGI_FORMAT_R8G8B8A8_SINT:
	case DXGI_FORMAT_R16G16_TYPELESS:
	case DXGI_FORMAT_R16G16_FLOAT:
	case DXGI_FORMAT_R16G16_UNORM:
	case DXGI_FORMAT_R16G16_UINT:
	case DXGI_FORMAT_R16G16_SNORM:
	case DXGI_FORMAT_R16G16_SINT:
	case DXGI_FORMAT_R32_TYPELESS:
	case DXGI_FORMAT_D32_FLOAT:
	case DXGI_FORMAT_R32_FLOAT:
	case DXGI_FORMAT_R32_UINT:
	case DXGI_FORMAT_R32_SINT:
	case DXGI_FORMAT_R24G8_TYPELESS:
	case DXGI_FORMAT_D24_UNORM_S8_UINT:
	case DXGI_FORMAT_R24_UNORM_X8_TYPELESS:
	case DXGI_FORMAT_X24_TYPELESS_G8_UINT:
	case DXGI_FORMAT_R9G9B9E5_SHAREDEXP:
	case DXGI_FORMAT_R8G8_B8G8_UNORM:
	case DXGI_FORMAT_G8R8_G8B8_UNORM:
	case DXGI_FORMAT_B8G8R8A8_UNORM:
	case DXGI_FORMAT_B8G8R8X8_UNORM:
	case DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM:
	case DXGI_FORMAT_B8G8R8A8_TYPELESS:
	case DXGI_FORMAT_B8G8R8A8_UNORM_SRGB:
	case DXGI_FORMAT_B8G8R8X8_TYPELESS:
	case DXGI_FORMAT_B8G8R8X8_UNORM_SRGB:
	case DXGI_FORMAT_AYUV:
	case DXGI_FORMAT_Y410:
	case DXGI_FORMAT_YUY2:
		return 32;

	case DXGI_FORMAT_P010:
	case DXGI_FORMAT_P016:
		return 24;

	case DXGI_FORMAT_R8G8_TYPELESS:
	case DXGI_FORMAT_R8G8_UNORM:
	case DXGI_FORMAT_R8G8_UINT:
	case DXGI_FORMAT_R8G8_SNORM:
	case DXGI_FORMAT_R8G8_SINT:
	case DXGI_FORMAT_R16_TYPELESS:
	case DXGI_FORMAT_R16_FLOAT:
	case DXGI_FORMAT_D16_UNORM:
	case DXGI_FORMAT_R16_UNORM:
	case DXGI_FORMAT_R16_UINT:
	case DXGI_FORMAT_R16_SNORM:
	case DXGI_FORMAT_R16_SINT:
	case DXGI_FORMAT_B5G6R5_UNORM:
	case DXGI_FORMAT_B5G5R5A1_UNORM:
	case DXGI_FORMAT_A8P8:
	case DXGI_FORMAT_B4G4R4A4_UNORM:
		return 16;

	case DXGI_FORMAT_NV12:
	case DXGI_FORMAT_420_OPAQUE:
	case DXGI_FORMAT_NV11:
		return 12;

	case DXGI_FORMAT_R8_TYPELESS:
	case DXGI_FORMAT_R8_UNORM:
	case DXGI_FORMAT_R8_UINT:
	case DXGI_FORMAT_R8_SNORM:
	case DXGI_FORMAT_R8_SINT:
	case DXGI_FORMAT_A8_UNORM:
	case DXGI_FORMAT_AI44:
	case DXGI_FORMAT_IA44:
	case DXGI_FORMAT_P8:
		return 8;

	case DXGI_FORMAT_R1_UNORM:
		return 1;

	case DXGI_FORMAT_BC1_TYPELESS:
	case DXGI_FORMAT_BC1_UNORM:
	case DXGI_FORMAT_BC1_UNORM_SRGB:
	case DXGI_FORMAT_BC4_TYPELESS:
	case DXGI_FORMAT_BC4_UNORM:
	case DXGI_FORMAT_BC4_SNORM:
		return 4;

	case DXGI_FORMAT_BC2_TYPELESS:
	case DXGI_FORMAT_BC2_UNORM:
	case DXGI_FORMAT_BC2_UNORM_SRGB:
	case DXGI_FORMAT_BC3_TYPELESS:
	case DXGI_FORMAT_BC3_UNORM:
	case DXGI_FORMAT_BC3_UNORM_SRGB:
	case DXGI_FORMAT_BC5_TYPELESS:
	case DXGI_FORMAT_BC5_UNORM:
	case DXGI_FORMAT_BC5_SNORM:
	case DXGI_FORMAT_BC6H_TYPELESS:
	case DXGI_FORMAT_BC6H_UF16:
	case DXGI_FORMAT_BC6H_SF16:
	case DXGI_FORMAT_BC7_TYPELESS:
	case DXGI_FORMAT_BC7_UNORM:
	case DXGI_FORMAT_BC7_UNORM_SRGB:
		return 8;

	default:
		return 0;
	}
}

class CD3D11Texture2D : public IRHITexture
{
public:
	CD3D11Texture2D();
	~CD3D11Texture2D();

	HRESULT Create(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;

private:
	ID3D11Texture2D*			m_pTexture;
	ID3D11ShaderResourceView*	m_pTextureSRV;
	TextureDesc					m_TextureDesc;
	int							m_Pitch;
};

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

	m_TextureDesc		= *pTextureDesc;
	m_Pitch				= Pitch;

	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	D3D11_TEXTURE2D_DESC d3dTextureDesc = {};
	d3dTextureDesc.Width = pTextureDesc->Width;
	d3dTextureDesc.Height = pTextureDesc->Height;
	d3dTextureDesc.MipLevels = pTextureDesc->NumMips;
	d3dTextureDesc.ArraySize = pTextureDesc->DepthOrSliceNum;
	d3dTextureDesc.Format = GetDxgiFormat(pTextureDesc->Format);
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

IRHITexture* CreateD3D11Texture(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch)
{
	CD3D11Texture2D* pTexture = new CD3D11Texture2D();

	R_CHK(pTexture->Create(pTextureDesc, pData, Size, Pitch));

	pTexture->AddRef();

	return pTexture;
}

class CD3D11Buffer : public IRHIBuffer
{
public:
	CD3D11Buffer();
	~CD3D11Buffer();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void UpdateData(const void* data, int size) override;

	bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) override;
	bool Unlock() override;

	ID3D11Buffer* GetD3DBufferObject();

private:
	ID3D11Buffer* m_pBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;
};

CD3D11Buffer::CD3D11Buffer() :
	m_pBuffer(nullptr),
	m_bImmutable(false)
{
}

CD3D11Buffer::~CD3D11Buffer()
{
	if (m_pBuffer)
	{
		m_pBuffer->Release();
		m_pBuffer = nullptr;
	}
}

HRESULT CD3D11Buffer::Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	ID3D11Device* pDevice = ((ID3D11Device*)HWRenderDevice);
	R_ASSERT(pDevice);

	m_BufferType = bufferType;
	m_bImmutable = bImmutable;

	D3D11_BUFFER_DESC desc;
	desc.ByteWidth = DataSize;
	desc.Usage = bImmutable ? D3D11_USAGE_DEFAULT : D3D11_USAGE_DYNAMIC;
	desc.BindFlags = GetD3D11BindFlags(bufferType);
	desc.CPUAccessFlags = bImmutable ? 0 : D3D11_CPU_ACCESS_WRITE;
	desc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subData;
	subData.pSysMem = pData;

	HRESULT res = pDevice->CreateBuffer(&desc, pData ? &subData : NULL, &m_pBuffer);
	//R_CHK(res);
	return res;
}

void CD3D11Buffer::UpdateData(const void* data, int size)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->UpdateSubresource(m_pBuffer, 0, NULL, data, 0, 0);
}

bool CD3D11Buffer::Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags)
{
	//	R_ASSERT(m_bImmutable);

	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	D3D11_MAPPED_SUBRESOURCE mappedSubresource = {};
	HRESULT hr = pImmediateContext->Map(m_pBuffer, 0, GetD3D11Map(Flags), 0, &mappedSubresource);
	if (FAILED(hr))
	{
		Msg("CD3D11Buffer::Lock: Failed to lock buffer. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	*ppbData = (byte*)mappedSubresource.pData + OffsetToLock;

	return true;
}

bool CD3D11Buffer::Unlock()
{
	//	R_ASSERT(m_bImmutable);

	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	pImmediateContext->Unmap(m_pBuffer, 0);

	return true;
}

ID3D11Buffer* CD3D11Buffer::GetD3DBufferObject()
{
	return m_pBuffer;
}

IRHIBuffer* CreateD3D11Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	CD3D11Buffer* pBuffer = new CD3D11Buffer();

	R_CHK(pBuffer->Create(bufferType, pData, DataSize, bImmutable));

	pBuffer->AddRef();

	return pBuffer;
}

void SetVertexBufferD3D11(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	CD3D11Buffer* pAPIBuffer = (CD3D11Buffer*)pVertexBuffer;
	if (pAPIBuffer)
	{
		ID3D11Buffer* pD3DBuffer = pAPIBuffer->GetD3DBufferObject();

		const UINT uStrides = Strides;
		const UINT uOffsets = Offsets;
		pImmediateContext->IASetVertexBuffers(StartSlot, 1, &pD3DBuffer, &uStrides, &uOffsets);
	}
	else
	{
		pImmediateContext->IASetVertexBuffers(StartSlot, 0, nullptr, 0, 0);
	}
}

void SetIndexBufferD3D11(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset)
{
	ID3D11DeviceContext* pImmediateContext = (ID3D11DeviceContext*)HWRenderContext;
	R_ASSERT(pImmediateContext);

	CD3D11Buffer* pAPIBuffer = (CD3D11Buffer*)pIndexBuffer;
	if (pAPIBuffer)
	{
		ID3D11Buffer* pD3DBuffer = pAPIBuffer->GetD3DBufferObject();
		DXGI_FORMAT indicesFormat = Is32BitBuffer ? DXGI_FORMAT_R32_UINT : DXGI_FORMAT_R16_UINT;
		pImmediateContext->IASetIndexBuffer(pD3DBuffer, indicesFormat, Offset);
	}
	else
	{
		pImmediateContext->IASetIndexBuffer(nullptr, DXGI_FORMAT_UNKNOWN, 0);
	}
}

#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

#include "Dx9Texture.h"

struct DX9TextureFormatPairs
{
	ERHITextureFormat RHIFormat;
	D3DFORMAT	DX9Format;
};

static DX9TextureFormatPairs TextureFormatList[] =
{
	{FMT_UNKNOWN, D3DFMT_UNKNOWN,		},
	{FMT_R8G8B8, D3DFMT_R8G8B8,		},
	{FMT_A8R8G8B8, D3DFMT_A8R8G8B8,		},
	{FMT_R5G6B5, D3DFMT_R5G6B5,		},
	{FMT_A8B8G8R8, D3DFMT_A8B8G8R8,		},
	{FMT_G16R16, D3DFMT_G16R16,		},
	{FMT_A16B16G16R16, D3DFMT_A16B16G16R16,	},
	{FMT_L8, D3DFMT_L8,			},
	{FMT_V8U8, D3DFMT_V8U8,			},
	{FMT_Q8W8V8U8, D3DFMT_Q8W8V8U8,		},
	{FMT_V16U16, D3DFMT_V16U16,		},
	{FMT_D24X8, D3DFMT_D24X8,			},
	{FMT_D32F_LOCKABLE, D3DFMT_D32F_LOCKABLE, },
	{FMT_G16R16F, D3DFMT_G16R16F,		},
	{FMT_A16B16G16R16F, D3DFMT_A16B16G16R16F,	},
	{FMT_R32F, D3DFMT_R32F,			},
	{FMT_R16F, D3DFMT_R16F,			},
	{FMT_A32B32G32R32F, D3DFMT_A32B32G32R32F, },

	{ FMT_UYVY          , D3DFMT_UYVY       },
	{ FMT_R8G8_B8G8     , D3DFMT_R8G8_B8G8  },
	{ FMT_YUY2          , D3DFMT_YUY2       },
	{ FMT_G8R8_G8B8     , D3DFMT_G8R8_G8B8  },
	{ FMT_DXT1          , D3DFMT_DXT1       },
	{ FMT_DXT2          , D3DFMT_DXT2       },
	{ FMT_DXT3          , D3DFMT_DXT3       },
	{ FMT_DXT4          , D3DFMT_DXT4       },
	{ FMT_DXT5          , D3DFMT_DXT5       },
};

static D3DFORMAT ConvertTextureFormat(ERHITextureFormat dx9FMT)
{
	int arrayLength = sizeof(TextureFormatList) / sizeof(TextureFormatList[0]);
	for (int i = 0; i < arrayLength; ++i)
	{
		if (TextureFormatList[i].RHIFormat == dx9FMT)
			return TextureFormatList[i].DX9Format;
	}

	VERIFY(!"ConvertTextureFormat didn't find appropriate dx9 texture format!");
	return D3DFMT_UNKNOWN;
}

CD3D9Texture::CD3D9Texture() :
	m_pTexture(nullptr)
{
	memset(&m_textureDesc, 0, sizeof(m_textureDesc));
}

CD3D9Texture::~CD3D9Texture()
{
	if (m_pTexture)
	{
		m_pTexture->Release();
		m_pTexture = nullptr;
	}
}

HRESULT CD3D9Texture::Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData)
{
	R_ASSERT(pTextureDesc);

	m_textureDesc = *pTextureDesc;

	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	D3DFORMAT Format = ConvertTextureFormat(pTextureDesc->Format);

	HRESULT hr = pDevice->CreateTexture(
		pTextureDesc->Width,
		pTextureDesc->Height,
		pTextureDesc->NumMips,
		pTextureDesc->Usage,
		Format,
		pTextureDesc->DefaultPool ? D3DPOOL_DEFAULT : D3DPOOL_MANAGED,
		&m_pTexture,
		NULL);

	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::Create: Failed to create texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return hr;
	}

	// SetData(pData, Size);

	return S_OK;
}

bool CD3D9Texture::LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags)
{
	R_ASSERT(m_pTexture);

	D3DLOCKED_RECT lockedRect;
	memset(&lockedRect, 0, sizeof(lockedRect));

	RECT rect;
	memset(&rect, 0, sizeof(rect));
	if (pRect)
		rect = { pRect->x1, pRect->y1, pRect->x2, pRect->y2 };

	HRESULT hr = m_pTexture->LockRect(Level, &lockedRect, pRect ? &rect : NULL, Flags);
	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::LockRect: Failed to lock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	*pLockedRect = *(LOCKED_RECT*)&lockedRect;

	return true;
}

bool CD3D9Texture::UnlockRect(u32 Level)
{
	HRESULT hr = m_pTexture->UnlockRect(Level);
	if (FAILED(hr))
	{
		Msg("CRenderTextureDX9::UnlockRect: Failed to unlock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	return true;
}

void CD3D9Texture::SetStage(u32 Stage)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	pDevice->SetTexture(Stage, m_pTexture);
}

void CD3D9Texture::SetData(const void* pData, const int size)
{
	//if (!pData)
	//	return;

	//if (!size)
	//	return;

	//// #TODO: Level fill

	//// #TODO: Properly format pitch calc
	//int pitchSize = 0;
	//if (m_textureDesc.Format == R8G8B8)
	//	pitchSize = 3;
	//else if (m_textureDesc.Format == A8R8G8B8)
	//	pitchSize = 4;

	//// lock rect
	//LOCKED_RECT lockRect;
	//if (this->LockRect(0, &lockRect, NULL, eLOCK_DISCARD))
	//{
	//	// copy image data
	//	uint8_t* textureData = (uint8_t*)lockRect.pBits;
	//	memcpy(textureData, pData, m_textureDesc.Width * m_textureDesc.Height * pitchSize);

	//	this->UnlockRect(0);
	//}
}

EResourceType CD3D9Texture::GetType()
{
	return eResourceTexture;
}

u32 CD3D9Texture::GetLevelCount()
{
	return m_pTexture->GetLevelCount();
}

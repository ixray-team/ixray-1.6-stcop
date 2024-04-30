#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

#include "Dx9Texture.h"

#define DDS_CUBEMAP_POSITIVEX 0x00000600 // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_POSITIVEX
#define DDS_CUBEMAP_NEGATIVEX 0x00000a00 // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_NEGATIVEX
#define DDS_CUBEMAP_POSITIVEY 0x00001200 // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_POSITIVEY
#define DDS_CUBEMAP_NEGATIVEY 0x00002200 // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_NEGATIVEY
#define DDS_CUBEMAP_POSITIVEZ 0x00004200 // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_POSITIVEZ
#define DDS_CUBEMAP_NEGATIVEZ 0x00008200 // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_NEGATIVEZ

#define DDS_CUBEMAP 0x00000200			 // DDSCAPS2_CUBEMAP

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
	{FMT_X8R8G8B8, D3DFMT_X8R8G8B8,		},
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


//--------------------------------------------------------------------------------------
// Return the BPP for a particular format
//--------------------------------------------------------------------------------------
size_t BitsPerPixel(_In_ D3DFORMAT fmt) noexcept
{
	switch (static_cast<int>(fmt))
	{
	case D3DFMT_A32B32G32R32F:
		return 128;

	case D3DFMT_A16B16G16R16:
	case D3DFMT_Q16W16V16U16:
	case D3DFMT_A16B16G16R16F:
	case D3DFMT_G32R32F:
		return 64;

	case D3DFMT_A8R8G8B8:
	case D3DFMT_X8R8G8B8:
	case D3DFMT_A2B10G10R10:
	case D3DFMT_A8B8G8R8:
	case D3DFMT_X8B8G8R8:
	case D3DFMT_G16R16:
	case D3DFMT_A2R10G10B10:
	case D3DFMT_Q8W8V8U8:
	case D3DFMT_V16U16:
	case D3DFMT_X8L8V8U8:
	case D3DFMT_A2W10V10U10:
	case D3DFMT_D32:
	case D3DFMT_D24S8:
	case D3DFMT_D24X8:
	case D3DFMT_D24X4S4:
	case D3DFMT_D32F_LOCKABLE:
	case D3DFMT_D24FS8:
	case D3DFMT_INDEX32:
	case D3DFMT_G16R16F:
	case D3DFMT_R32F:
#if !defined(D3D_DISABLE_9EX)
	case D3DFMT_D32_LOCKABLE:
#endif
		return 32;

	case D3DFMT_R8G8B8:
		return 24;

	case D3DFMT_A4R4G4B4:
	case D3DFMT_X4R4G4B4:
	case D3DFMT_R5G6B5:
	case D3DFMT_L16:
	case D3DFMT_A8L8:
	case D3DFMT_X1R5G5B5:
	case D3DFMT_A1R5G5B5:
	case D3DFMT_A8R3G3B2:
	case D3DFMT_V8U8:
	case D3DFMT_CxV8U8:
	case D3DFMT_L6V5U5:
	case D3DFMT_G8R8_G8B8:
	case D3DFMT_R8G8_B8G8:
	case D3DFMT_D16_LOCKABLE:
	case D3DFMT_D15S1:
	case D3DFMT_D16:
	case D3DFMT_INDEX16:
	case D3DFMT_R16F:
	case D3DFMT_YUY2:
		// From DX docs, reference/d3d/enums/d3dformat.asp
		// (note how it says that D3DFMT_R8G8_B8G8 is "A 16-bit packed RGB format analogous to UYVY (U0Y0, V0Y1, U2Y2, and so on)")
	case D3DFMT_UYVY:
		return 16;

	case D3DFMT_R3G3B2:
	case D3DFMT_A8:
	case D3DFMT_A8P8:
	case D3DFMT_P8:
	case D3DFMT_L8:
	case D3DFMT_A4L4:
	case D3DFMT_DXT2:
	case D3DFMT_DXT3:
	case D3DFMT_DXT4:
	case D3DFMT_DXT5:
		// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/directshow/htm/directxvideoaccelerationdxvavideosubtypes.asp
	case MAKEFOURCC('A', 'I', '4', '4'):
	case MAKEFOURCC('I', 'A', '4', '4'):
#if !defined(D3D_DISABLE_9EX)
	case D3DFMT_S8_LOCKABLE:
#endif
		return 8;

	case D3DFMT_DXT1:
		return 4;

	case MAKEFOURCC('Y', 'V', '1', '2'):
		return 12;

#if !defined(D3D_DISABLE_9EX)
	case D3DFMT_A1:
		return 1;
#endif

	default:
		return 0;
	}
}

//--------------------------------------------------------------------------------------
// Get surface information for a particular format
//--------------------------------------------------------------------------------------
HRESULT GetSurfaceInfo(
	_In_ size_t width,
	_In_ size_t height,
	_In_ D3DFORMAT fmt,
	size_t* outNumBytes,
	_Out_opt_ size_t* outRowBytes,
	_Out_opt_ size_t* outNumRows) noexcept
{
	uint64_t numBytes = 0;
	uint64_t rowBytes = 0;
	uint64_t numRows = 0;

	bool bc = false;
	bool packed = false;
	size_t bpe = 0;
	switch (static_cast<int>(fmt))
	{
	case D3DFMT_DXT1:
		bc = true;
		bpe = 8;
		break;

	case D3DFMT_DXT2:
	case D3DFMT_DXT3:
	case D3DFMT_DXT4:
	case D3DFMT_DXT5:
		bc = true;
		bpe = 16;
		break;

	case D3DFMT_R8G8_B8G8:
	case D3DFMT_G8R8_G8B8:
	case D3DFMT_UYVY:
	case D3DFMT_YUY2:
		packed = true;
		bpe = 4;
		break;

	default:
		break;
	}

	if (bc)
	{
		uint64_t numBlocksWide = 0;
		if (width > 0)
		{
			numBlocksWide = std::max<uint64_t>(1u, (uint64_t(width) + 3u) / 4u);
		}
		uint64_t numBlocksHigh = 0;
		if (height > 0)
		{
			numBlocksHigh = std::max<uint64_t>(1u, (uint64_t(height) + 3u) / 4u);
		}
		rowBytes = numBlocksWide * bpe;
		numRows = numBlocksHigh;
		numBytes = rowBytes * numBlocksHigh;
	}
	else if (packed)
	{
		rowBytes = ((uint64_t(width) + 1u) >> 1) * bpe;
		numRows = uint64_t(height);
		numBytes = rowBytes * height;
	}
	else
	{
		const size_t bpp = BitsPerPixel(fmt);
		if (!bpp)
			return E_INVALIDARG;

		rowBytes = (uint64_t(width) * bpp + 7u) / 8u; // round up to nearest byte
		numRows = uint64_t(height);
		numBytes = rowBytes * height;
	}

#if defined(_M_IX86) || defined(_M_ARM) || defined(_M_HYBRID_X86_ARM64)
	static_assert(sizeof(size_t) == 4, "Not a 32-bit platform!");
	if (numBytes > UINT32_MAX || rowBytes > UINT32_MAX || numRows > UINT32_MAX)
		return HRESULT_FROM_WIN32(ERROR_ARITHMETIC_OVERFLOW);
#else
	static_assert(sizeof(size_t) == 8, "Not a 64-bit platform!");
#endif

	if (outNumBytes)
	{
		*outNumBytes = static_cast<size_t>(numBytes);
	}
	if (outRowBytes)
	{
		*outRowBytes = static_cast<size_t>(rowBytes);
	}
	if (outNumRows)
	{
		*outNumRows = static_cast<size_t>(numRows);
	}

	return S_OK;
}

DWORD GetD3D9Usage( ERHIUsage RHIUsage )
{
	DWORD dwUsage = 0;

	switch (RHIUsage)
	{
	case eUsageRenderTarget:
		dwUsage = D3DUSAGE_RENDERTARGET;
		break;
	case eUsageDepthStencil:
		dwUsage = D3DUSAGE_DEPTHSTENCIL;
		break;
	case eUsageStatic:
		break;
	case eUsageDynamic:
		break;
	case eUsageScratch:
		break;
	default:
		break;
	}

	return dwUsage;
}

CD3D9Texture::CD3D9Texture() :
	m_pTexture(nullptr)
{
	memset(&m_TextureDesc, 0, sizeof(m_TextureDesc));
}

CD3D9Texture::~CD3D9Texture()
{
	if (m_pTexture)
	{
		m_pTexture->Release();
		m_pTexture = nullptr;
	}
}

Ivector2 CD3D9Texture::GetTextureSize() const
{
	D3DSURFACE_DESC desc;
	((IDirect3DTexture9*)m_pTexture)->GetLevelDesc(0, &desc);

	return { (int)desc.Width , (int)desc.Height };
}

HRESULT CD3D9Texture::Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData)
{
	R_ASSERT(pTextureDesc);
	m_TextureDesc = *pTextureDesc;

	HRESULT hr = S_OK;

	if (m_TextureDesc.IsCube) // Cubemap
		hr = CreateTextureCube(pTextureDesc, pSubresourceData);
	else // 2D Texture
		hr = CreateTexture2D(pTextureDesc, pSubresourceData);

	return hr;
}

HRESULT CD3D9Texture::CreateTexture2D(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	D3DFORMAT Format = ConvertTextureFormat(pTextureDesc->Format);

	IDirect3DTexture9* pTexture = nullptr;
	HRESULT hr = pDevice->CreateTexture(
		pTextureDesc->Width,
		pTextureDesc->Height,
		pTextureDesc->NumMips,
		GetD3D9Usage((ERHIUsage)pTextureDesc->Usage),
		Format,
		pTextureDesc->DefaultPool ? D3DPOOL_DEFAULT : D3DPOOL_MANAGED,
		&pTexture,
		NULL);

	if (FAILED(hr))
	{
		Msg("! CRenderTextureDX9::Create: Failed to create texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return hr;
	}

	if (pTextureDesc->Usage == eUsageRenderTarget)
		Msg("* CRenderTextureDX9::Create: Created render target [%ix%i]", pTextureDesc->Width,
			pTextureDesc->Height);

	if (pTextureDesc->Usage == eUsageDepthStencil)
		Msg("* CRenderTextureDX9::Create: Created depth stencil target [%ix%i]", pTextureDesc->Width,
			pTextureDesc->Height);

	m_pTexture = pTexture;

	SetData(pSubresourceData);

	return hr;
}

HRESULT CD3D9Texture::CreateTextureCube(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	D3DFORMAT Format = ConvertTextureFormat(pTextureDesc->Format);

	IDirect3DCubeTexture9* pCubeTexture = nullptr;

	HRESULT hr = pDevice->CreateCubeTexture(
		pTextureDesc->Width, 
		pTextureDesc->NumMips,
		pTextureDesc->Usage, 
		Format, 
		pTextureDesc->DefaultPool ? D3DPOOL_DEFAULT : D3DPOOL_MANAGED, 
		&pCubeTexture,
		nullptr);

	if (FAILED(hr))
	{
		Msg("! CRenderTextureDX9::Create: Failed to create texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return hr;
	}

	SetDataCube(pCubeTexture, pSubresourceData);

	m_pTexture = pCubeTexture;

	return hr;
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

	HRESULT hr = ((IDirect3DTexture9*)m_pTexture)->LockRect(Level, &lockedRect, pRect ? &rect : NULL, Flags);
	if (FAILED(hr))
	{
		Msg("! CRenderTextureDX9::LockRect: Failed to lock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	*pLockedRect = *(LOCKED_RECT*)&lockedRect;

	return true;
}

bool CD3D9Texture::UnlockRect(u32 Level)
{
	HRESULT hr = ((IDirect3DTexture9*)m_pTexture)->UnlockRect(Level);
	if (FAILED(hr))
	{
		Msg("! CRenderTextureDX9::UnlockRect: Failed to unlock texture. DirectX Error: %s", Debug.dxerror2string(hr));
		return false;
	}

	return true;
}

void CD3D9Texture::SetStage(u32 Stage)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	pDevice->SetTexture(Stage, m_pTexture);
}

void CD3D9Texture::SetData( LPSUBRESOURCE_DATA pSubresourceData )
{
	if (!pSubresourceData || !pSubresourceData->pSysMem)
		return;

	UINT iWidth			= m_TextureDesc.Width;
	UINT iHeight		= m_TextureDesc.Height;
	UINT iMipCount		= m_TextureDesc.NumMips;
	D3DFORMAT Format	= ConvertTextureFormat(m_TextureDesc.Format);

	// Lock, fill, unlock
	size_t NumBytes = 0;
	size_t RowBytes = 0;
	size_t NumRows = 0;
	const uint8_t* pSrcBits = (const uint8_t*)pSubresourceData->pSysMem;
	//const uint8_t* pEndBits = (const uint8_t*)pSubresourceData->pSysMem + bitSize;
	D3DLOCKED_RECT LockedRect = {};

	for (UINT i = 0; i < iMipCount; ++i)
	{
		GetSurfaceInfo(iWidth, iHeight, Format, &NumBytes, &RowBytes, &NumRows);

		if (NumBytes > UINT32_MAX || RowBytes > UINT32_MAX)
			__debugbreak();
			//return HRESULT_FROM_WIN32(ERROR_ARITHMETIC_OVERFLOW);

		//if ((pSrcBits + NumBytes) > pEndBits)
		//{
		//	__debugbreak();
		//	//return HRESULT_FROM_WIN32(ERROR_HANDLE_EOF);
		//}

		if (SUCCEEDED(((IDirect3DTexture9*)m_pTexture)->LockRect(i, &LockedRect, nullptr, 0)))
		{
			auto pDestBits = static_cast<uint8_t*>(LockedRect.pBits);

			// Copy stride line by line
			for (UINT h = 0; h < NumRows; h++)
			{
				memcpy_s(pDestBits, static_cast<size_t>(LockedRect.Pitch), pSrcBits, RowBytes);
				pDestBits += LockedRect.Pitch;
				pSrcBits += RowBytes;
			}

			((IDirect3DTexture9*)m_pTexture)->UnlockRect(i);
		}

		iWidth = iWidth >> 1;
		iHeight = iHeight >> 1;
		if (iWidth == 0)
			iWidth = 1;
		if (iHeight == 0)
			iHeight = 1;
	}
}

void CD3D9Texture::SetDataCube(IDirect3DCubeTexture9* pCubeTexture, LPSUBRESOURCE_DATA pSubresourceData)
{
	UINT iWidth = m_TextureDesc.Width;
	UINT iHeight = m_TextureDesc.Height;
	UINT iMipCount = m_TextureDesc.NumMips;
	D3DFORMAT Format = ConvertTextureFormat(m_TextureDesc.Format);

	// Lock, fill, unlock
	size_t NumBytes = 0;
	size_t RowBytes = 0;
	size_t NumRows = 0;
	const uint8_t* pSrcBits = (const uint8_t*)pSubresourceData->pSysMem;
	//const uint8_t* pEndBits = (const uint8_t*)pSubresourceData->pSysMem + bitSize;
	D3DLOCKED_RECT LockedRect = {};

	UINT mask = DDS_CUBEMAP_POSITIVEX & ~DDS_CUBEMAP;
	for (UINT f = 0; f < 6; ++f, mask <<= 1)
	{
		UINT w = iWidth;
		UINT h = iHeight;
		for (UINT i = 0; i < m_TextureDesc.DepthOrSliceNum; ++i)
		{
			GetSurfaceInfo(w, h, Format, &NumBytes, &RowBytes, &NumRows);

			if (NumBytes > UINT32_MAX || RowBytes > UINT32_MAX)
				return;

			if (SUCCEEDED(pCubeTexture->LockRect(static_cast<D3DCUBEMAP_FACES>(f), i, &LockedRect, nullptr, 0)))
			{
				auto pDestBits = static_cast<uint8_t*>(LockedRect.pBits);

				// Copy stride line by line
				for (size_t r = 0; r < NumRows; r++)
				{
					memcpy_s(pDestBits, static_cast<size_t>(LockedRect.Pitch), pSrcBits, RowBytes);
					pDestBits += LockedRect.Pitch;
					pSrcBits += RowBytes;
				}

				pCubeTexture->UnlockRect(static_cast<D3DCUBEMAP_FACES>(f), i);
			}

			w = w >> 1;
			h = h >> 1;
			if (w == 0)
				w = 1;
			if (h == 0)
				h = 1;
		}
	}
}

u64 CD3D9Texture::Release()
{
	if (this == nullptr)
		return 0;

	R_ASSERT(m_RefCount > 0);
	--m_RefCount;

	if (m_RefCount == 0)
	{
		if (m_pTexture != nullptr)
		{
			ULONG refCount = m_pTexture->AddRef();
			while (refCount > 0)
			{
				refCount = m_pTexture->Release();
			}

			m_pTexture = nullptr;

			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
			//m_pTexture->Release();
		}

		//delete this;
		return 0;
	}

	return m_RefCount;
}

u64 CD3D9Texture::AddRef()
{
	//m_pTexture->AddRef();
	return IRHIUnknown::AddRef();
}

EResourceType CD3D9Texture::GetType()
{
	return eResourceTexture;
}

u32 CD3D9Texture::GetLevelCount()
{
	return m_pTexture->GetLevelCount();
}

bool CD3D9Texture::GetSurfaceLevel(u32 Level, LPIRHISURFACE* ppSurfaceLevel)
{
	IDirect3DSurface9* pSurfaceAPI = nullptr;
	R_CHK(((IDirect3DTexture9*)m_pTexture)->GetSurfaceLevel(0, &pSurfaceAPI));

	CD3D9Surface* pSurfaceRHI = new CD3D9Surface(pSurfaceAPI);
	pSurfaceRHI->AddRef();
	*ppSurfaceLevel = pSurfaceRHI;

	return true;
}

//---------------------------------------------------------------------------------------

CD3D9Surface::CD3D9Surface(IDirect3DSurface9* pSurfaceAPI) :
	m_pSurfaceAPI(pSurfaceAPI)
{
}

CD3D9Surface::~CD3D9Surface()
{
	if (m_pSurfaceAPI != nullptr)
	{
		m_pSurfaceAPI->Release();
	}
}

IDirect3DSurface9* CD3D9Surface::GetD3D9SurfaceObject()
{
	return m_pSurfaceAPI;
}

EResourceType CD3D9Surface::GetType()
{
	return eResourceUnknown;
}

///////////////////////////////////////////////////////////
// Cubemap implmenetatation

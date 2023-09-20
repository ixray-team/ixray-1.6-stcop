#include "stdafx.h"
#include "dx10TextureUtils.h"

namespace dx10TextureUtils
{
struct TextureFormatPairs
{
	D3DFORMAT	m_dx9FMT;
	DXGI_FORMAT	m_dx10FMT;
};

TextureFormatPairs	TextureFormatList[] = 
{
	{ D3DFMT_UNKNOWN,		DXGI_FORMAT_UNKNOWN },
	{ D3DFMT_A8R8G8B8,		DXGI_FORMAT_R8G8B8A8_UNORM },	// Not available 
	{ D3DFMT_R5G6B5,		DXGI_FORMAT_R8G8B8A8_UNORM },		// Not available 
	{ D3DFMT_A8B8G8R8,		DXGI_FORMAT_R8G8B8A8_UNORM},// & DXGI_FORMAT_R8G8B8A8_UNORM_SRGB 
	{ D3DFMT_G16R16,		DXGI_FORMAT_R16G16_UNORM},
	{ D3DFMT_A16B16G16R16,	DXGI_FORMAT_R16G16B16A16_UNORM},
	{ D3DFMT_L8,			DXGI_FORMAT_R8_UNORM}, // Note: Use .r swizzle in shader to duplicate red to other components to get D3D9 behavior. 
	{ D3DFMT_V8U8,			DXGI_FORMAT_R8G8_SNORM},
	{ D3DFMT_Q8W8V8U8,		DXGI_FORMAT_R8G8B8A8_SNORM},
	{ D3DFMT_V16U16,		DXGI_FORMAT_R16G16_SNORM},
	{ D3DFMT_D24X8,			DXGI_FORMAT_R24G8_TYPELESS},	//DXGI_FORMAT_D24_UNORM_S8_UINT},	// Not available 
	{ D3DFMT_D32F_LOCKABLE, DXGI_FORMAT_R32_TYPELESS},
	{ D3DFMT_G16R16F,		DXGI_FORMAT_R16G16_FLOAT},
	{ D3DFMT_A16B16G16R16F,	DXGI_FORMAT_R16G16B16A16_FLOAT},
	{ D3DFMT_R32F,			DXGI_FORMAT_R32_FLOAT},
	{ D3DFMT_R16F,			DXGI_FORMAT_R16_FLOAT},
	{ D3DFMT_A32B32G32R32F, DXGI_FORMAT_R32G32B32A32_FLOAT },
};

DXGI_FORMAT	ConvertTextureFormat(D3DFORMAT dx9FMT)
{
	int arrayLength = sizeof(TextureFormatList)/sizeof(TextureFormatList[0]);
	for (int i=0; i<arrayLength; ++i)
	{
		if (TextureFormatList[i].m_dx9FMT==dx9FMT)
			return TextureFormatList[i].m_dx10FMT;
	}

	VERIFY(!"ConvertTextureFormat didn't find appropriate dx10 texture format!");
	return DXGI_FORMAT_UNKNOWN;
}
}
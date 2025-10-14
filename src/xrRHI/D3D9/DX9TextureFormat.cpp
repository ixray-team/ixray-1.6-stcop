#include "DX9Texture.h"

D3DFORMAT ConvertRHIFormatToDX9(ERHI_FORMAT rhiFormat)
{
	switch (rhiFormat)
	{
	// 32-bit RGBA formats
	case ERHI_FORMAT::R32G32B32A32_FLOAT:    return D3DFMT_A32B32G32R32F;
	case ERHI_FORMAT::R32G32B32A32_UINT:     return D3DFMT_UNKNOWN; // Not supported in DX9
	case ERHI_FORMAT::R32G32B32A32_SINT:     return D3DFMT_UNKNOWN; // Not supported in DX9

	// 32-bit RGB formats
	case ERHI_FORMAT::R32G32B32_FLOAT:       return D3DFMT_UNKNOWN; // No equivalent in DX9

	// 16-bit RGBA formats
	case ERHI_FORMAT::R16G16B16A16_FLOAT:    return D3DFMT_A16B16G16R16F;
	case ERHI_FORMAT::R16G16B16A16_UNORM:    return D3DFMT_A16B16G16R16;
	case ERHI_FORMAT::R16G16B16A16_UINT:     return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R16G16B16A16_SNORM:    return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R16G16B16A16_SINT:     return D3DFMT_UNKNOWN;

	// 32-bit RG formats
	case ERHI_FORMAT::R32G32_FLOAT:          return D3DFMT_G32R32F;
	case ERHI_FORMAT::R32G32_UINT:           return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R32G32_SINT:           return D3DFMT_UNKNOWN;

	// Depth-stencil formats
	case ERHI_FORMAT::D32_FLOAT_S8X24_UINT:  return D3DFMT_D32F_LOCKABLE;
	case ERHI_FORMAT::D24_UNORM_S8_UINT:     return D3DFMT_D24S8;
	case ERHI_FORMAT::R24_UNORM_X8_TYPELESS: return D3DFMT_D24X8;
	case ERHI_FORMAT::D32_FLOAT:             return D3DFMT_D32F_LOCKABLE;
	case ERHI_FORMAT::D16_UNORM:             return D3DFMT_D16;

	// 10-bit RGB formats
	case ERHI_FORMAT::R10G10B10A2_UNORM:     return D3DFMT_A2B10G10R10;
	case ERHI_FORMAT::R10G10B10A2_UINT:      return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R11G11B10_FLOAT:       return D3DFMT_UNKNOWN;

	// 8-bit RGBA formats
	case ERHI_FORMAT::R8G8B8A8_UNORM:        return D3DFMT_A8R8G8B8;
	case ERHI_FORMAT::R8G8B8A8_UNORM_SRGB:   return D3DFMT_A8B8G8R8;
	case ERHI_FORMAT::R8G8B8A8_UINT:         return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R8G8B8A8_SNORM:        return D3DFMT_Q8W8V8U8;
	case ERHI_FORMAT::R8G8B8A8_SINT:         return D3DFMT_UNKNOWN;

	// 16-bit RG formats
	case ERHI_FORMAT::R16G16_FLOAT:          return D3DFMT_G16R16F;
	case ERHI_FORMAT::R16G16_UNORM:          return D3DFMT_G16R16;
	case ERHI_FORMAT::R16G16_UINT:           return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R16G16_SNORM:          return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R16G16_SINT:           return D3DFMT_UNKNOWN;

	// 32-bit single channel
	case ERHI_FORMAT::R32_FLOAT:             return D3DFMT_R32F;
	case ERHI_FORMAT::R32_UINT:              return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R32_SINT:              return D3DFMT_UNKNOWN;

	// 8-bit RG formats
	case ERHI_FORMAT::R8G8_UNORM:            return D3DFMT_A8L8;
	case ERHI_FORMAT::R8G8_UINT:             return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R8G8_SNORM:            return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R8G8_SINT:             return D3DFMT_UNKNOWN;

	// 16-bit single channel
	case ERHI_FORMAT::R16_FLOAT:             return D3DFMT_R16F;
	case ERHI_FORMAT::R16_UNORM:             return D3DFMT_L16;
	case ERHI_FORMAT::R16_UINT:              return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R16_SNORM:             return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R16_SINT:              return D3DFMT_UNKNOWN;

	// 8-bit single channel
	case ERHI_FORMAT::R8_UNORM:              return D3DFMT_L8;
	case ERHI_FORMAT::R8_UINT:               return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R8_SNORM:              return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::R8_SINT:               return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::A8_UNORM:              return D3DFMT_A8;

	// Compressed formats (BC/DXT)
	case ERHI_FORMAT::BC1_UNORM:             return D3DFMT_DXT1;
	case ERHI_FORMAT::BC1_UNORM_SRGB:        return D3DFMT_DXT1;
	case ERHI_FORMAT::BC2_UNORM:             return D3DFMT_DXT3;
	case ERHI_FORMAT::BC2_UNORM_SRGB:        return D3DFMT_DXT3;
	case ERHI_FORMAT::BC3_UNORM:             return D3DFMT_DXT5;
	case ERHI_FORMAT::BC3_UNORM_SRGB:        return D3DFMT_DXT5;
	//case ERHI_FORMAT::BC4_UNORM:             return D3DFMT_BC4_UNORM;
	//case ERHI_FORMAT::BC4_SNORM:             return D3DFMT_BC4_SNORM;
	//case ERHI_FORMAT::BC5_UNORM:             return D3DFMT_BC5_UNORM;
	//case ERHI_FORMAT::BC5_SNORM:             return D3DFMT_BC5_SNORM;

	// Legacy formats
	case ERHI_FORMAT::B5G6R5_UNORM:          return D3DFMT_R5G6B5;
	case ERHI_FORMAT::B5G5R5A1_UNORM:        return D3DFMT_A1R5G5B5;
	case ERHI_FORMAT::B8G8R8A8_UNORM:        return D3DFMT_A8R8G8B8;
	case ERHI_FORMAT::B8G8R8X8_UNORM:        return D3DFMT_X8R8G8B8;
	case ERHI_FORMAT::B4G4R4A4_UNORM:        return D3DFMT_A4R4G4B4;

	// Video formats (частичная поддержка)
	//case ERHI_FORMAT::NV12:                  return D3DFMT_NV12;
	case ERHI_FORMAT::YUY2:                  return D3DFMT_YUY2;

	// Неподдерживаемые форматы
	case ERHI_FORMAT::UNKNOWN:				return D3DFMT_UNKNOWN;
	case ERHI_FORMAT::FORCE_UINT:
	default:
		VERIFY(!"Unsupported");
		return D3DFMT_UNKNOWN;
	}
}

ERHI_FORMAT ConvertDX9FormatToRHI(D3DFORMAT dx9Format)
{
	switch (dx9Format)
	{
	// 32-bit RGBA formats
	case D3DFMT_A32B32G32R32F:    return ERHI_FORMAT::R32G32B32A32_FLOAT;

	// 16-bit RGBA formats
	case D3DFMT_A16B16G16R16F:    return ERHI_FORMAT::R16G16B16A16_FLOAT;
	case D3DFMT_A16B16G16R16:     return ERHI_FORMAT::R16G16B16A16_UNORM;

	// 32-bit RG formats
	case D3DFMT_G32R32F:          return ERHI_FORMAT::R32G32_FLOAT;

	// Depth-stencil formats
	case D3DFMT_D32F_LOCKABLE:    return ERHI_FORMAT::D32_FLOAT;
	case D3DFMT_D24S8:            return ERHI_FORMAT::D24_UNORM_S8_UINT;
	case D3DFMT_D16:              return ERHI_FORMAT::D16_UNORM;
	case D3DFMT_D24X8:            return ERHI_FORMAT::R24_UNORM_X8_TYPELESS;
	case D3DFMT_D15S1:            return ERHI_FORMAT::D24_UNORM_S8_UINT; // Ближайший аналог

	// 10-bit RGB formats
	case D3DFMT_A2B10G10R10:      return ERHI_FORMAT::R10G10B10A2_UNORM;

	// 8-bit RGBA formats
	case D3DFMT_A8B8G8R8:         return ERHI_FORMAT::R8G8B8A8_UNORM;
	case D3DFMT_X8B8G8R8:         return ERHI_FORMAT::R8G8B8A8_UNORM; // Без альфа-канала

	// 16-bit RG formats
	case D3DFMT_G16R16F:          return ERHI_FORMAT::R16G16_FLOAT;
	case D3DFMT_G16R16:           return ERHI_FORMAT::R16G16_UNORM;

	// 32-bit single channel
	case D3DFMT_R32F:             return ERHI_FORMAT::R32_FLOAT;

	// 16-bit single channel
	case D3DFMT_R16F:             return ERHI_FORMAT::R16_FLOAT;
	case D3DFMT_L16:              return ERHI_FORMAT::R16_UNORM;

	// 8-bit single channel
	case D3DFMT_L8:               return ERHI_FORMAT::R8_UNORM;
	case D3DFMT_A8:               return ERHI_FORMAT::A8_UNORM;

	// Compressed formats (BC/DXT)
	case D3DFMT_DXT1:             return ERHI_FORMAT::BC1_UNORM;
	case D3DFMT_DXT2:             return ERHI_FORMAT::BC2_UNORM; // DXT2 аналогичен DXT3
	case D3DFMT_DXT3:             return ERHI_FORMAT::BC2_UNORM;
	case D3DFMT_DXT4:             return ERHI_FORMAT::BC3_UNORM; // DXT4 аналогичен DXT5
	case D3DFMT_DXT5:             return ERHI_FORMAT::BC3_UNORM;

	// Legacy formats
	case D3DFMT_R5G6B5:           return ERHI_FORMAT::B5G6R5_UNORM;
	case D3DFMT_A1R5G5B5:         return ERHI_FORMAT::B5G5R5A1_UNORM;
	case D3DFMT_A8R8G8B8:         return ERHI_FORMAT::B8G8R8A8_UNORM;
	case D3DFMT_X8R8G8B8:         return ERHI_FORMAT::B8G8R8X8_UNORM;
	case D3DFMT_A4R4G4B4:         return ERHI_FORMAT::B4G4R4A4_UNORM;
	case D3DFMT_Q8W8V8U8:		  return ERHI_FORMAT::R8G8B8A8_SNORM;
	case D3DFMT_A8L8:			  return ERHI_FORMAT::R8G8_UNORM;

	// Video formats
	case D3DFMT_YUY2:             return ERHI_FORMAT::YUY2;
	case D3DFMT_UYVY:             return ERHI_FORMAT::YUY2; // Ближайший аналог

	// Дополнительные форматы, которые могут быть в DX9
	case D3DFMT_A4L4:             return ERHI_FORMAT::R8_UNORM; // Ближайший аналог
	case D3DFMT_A2B10G10R10_XR_BIAS: return ERHI_FORMAT::R10G10B10A2_UNORM; // Ближайший аналог
	case D3DFMT_A8P8:             return ERHI_FORMAT::R8_UNORM; // Ближайший аналог
	case D3DFMT_P8:               return ERHI_FORMAT::R8_UNORM; // Ближайший аналог

	// Неподдерживаемые форматы
	case D3DFMT_UNKNOWN:
	default:
		return ERHI_FORMAT::UNKNOWN;
	}
}

u32 GetDX9FormatSize(D3DFORMAT dx9Format)
{
	switch (dx9Format)
	{
	case D3DFMT_R32F:               return 4;
	case D3DFMT_A32B32G32R32F:      return 16;
	case D3DFMT_A16B16G16R16F:      return 8;
	case D3DFMT_A16B16G16R16:       return 8;
	case D3DFMT_G32R32F:            return 8;
	case D3DFMT_G16R16F:            return 4;
	case D3DFMT_G16R16:             return 4;
	case D3DFMT_A8B8G8R8:           return 4;
	case D3DFMT_R16F:               return 2;
	case D3DFMT_L16:                return 2;
	case D3DFMT_L8:                 return 1;
	case D3DFMT_A8:                 return 1;
	case D3DFMT_DXT1:               return 8;  // на блок 4x4
	case D3DFMT_DXT3:               return 16; // на блок 4x4
	case D3DFMT_DXT5:               return 16; // на блок 4x4
	case D3DFMT_R5G6B5:             return 2;
	case D3DFMT_A1R5G5B5:           return 2;
	case D3DFMT_A8R8G8B8:           return 4;
	case D3DFMT_X8R8G8B8:           return 4;
	case D3DFMT_A4R4G4B4:           return 2;
	default:                        return 0;
	}
}
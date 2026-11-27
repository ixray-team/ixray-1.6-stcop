#include "stdafx.h"
#include "ViewerFallbackTextures.h"
#include "dxRenderDeviceRender.h"
#include "ResourceManager.h"
#include <array>

namespace ViewerFallbackTextures
{
namespace Detail
{
	constexpr std::array<u8, 16 * 16 * 4> MakeGroundPixels()
	{
		std::array<u8, 16 * 16 * 4> pixels = {};
		for (u32 y = 0; y < 16; ++y)
		{
			for (u32 x = 0; x < 16; ++x)
			{
				const u32 grain = (x * 17 + y * 31 + (x ^ y) * 7) % 31;
				const u32 index = (y * 16 + x) * 4;
				pixels[index] = u8(58 + grain);
				pixels[index + 1] = u8(76 + grain);
				pixels[index + 2] = u8(43 + grain / 2);
				pixels[index + 3] = 255;
			}
		}
		return pixels;
	}

	constexpr std::array<u8, 6 * 32 * 32 * 4> MakeSkyPixels()
	{
		std::array<u8, 6 * 32 * 32 * 4> pixels = {};
		for (u32 face = 0; face < 6; ++face)
		{
			for (u32 y = 0; y < 32; ++y)
			{
				const float v = 1.f - 2.f * float(y) / 31.f;
				float elevation = -v;
				if (face == 2)
					elevation = 1.f;
				else if (face == 3)
					elevation = -1.f;
				const float skyBlend = elevation > 0.f ? elevation * elevation : 0.f;
				const float groundBlend = elevation < 0.f ? -elevation : 0.f;
				for (u32 x = 0; x < 32; ++x)
				{
					const u32 index = ((face * 32 + y) * 32 + x) * 4;
					pixels[index] = u8(182.f - 127.f * skyBlend - 110.f * groundBlend);
					pixels[index + 1] = u8(204.f - 93.f * skyBlend - 116.f * groundBlend);
					pixels[index + 2] = u8(226.f - 45.f * skyBlend - 143.f * groundBlend);
					pixels[index + 3] = 255;
				}
			}
		}
		return pixels;
	}

	CTexture* EnsureTexture(const char* name, const void* pixels, u32 width, u32 height, bool cube = false)
	{
		RHITextureDesc desc(width, height, ERHI_FORMAT::R8G8B8A8_UNORM);
		desc.MipLevels = 1;
		desc.ArraySize = cube ? 6 : 1;
		desc.MiscFlags = cube ? u32(ERHI_RESOURCE_MISC_FLAG::TEXTURECUBE) : 0;
		desc.BindFlags = ERHI_BIND_FLAG(SHADER_RESOURCE);
		desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
		return DEV->_CreateTextureFromMemory(name, pixels, width * 4, desc);
	}
}

CTexture* EnsureGroundTexture()
{
	constexpr auto pixels = Detail::MakeGroundPixels();
	return Detail::EnsureTexture("$memory$viewer_ground", pixels.data(), 16, 16);
}

CTexture* EnsureSkyTexture()
{
	constexpr auto pixels = Detail::MakeSkyPixels();
	return Detail::EnsureTexture("$memory$viewer_sky", pixels.data(), 32, 32, true);
}
}

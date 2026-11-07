#pragma once

#include <DirectXTex.h>
#include <stb/stb_image_write.h>

namespace ScreenshotEncoding
{
inline int WriteBGRATGA(stbi_write_func* Write, void* Context, const DirectX::Image& Image)
{
	// stb consumes RGBA; keep the original BGRA image intact for DDS paths.
	DirectX::ScratchImage RGBA;
	if (FAILED(DirectX::Convert(Image, DXGI_FORMAT_R8G8B8A8_UNORM, DirectX::TEX_FILTER_DEFAULT, 0.0f, RGBA)))
	{
		return 0;
	}
	const auto* Pixels = RGBA.GetImage(0, 0, 0);
	return stbi_write_tga_to_func(Write, Context, Pixels->width, Pixels->height, 4, Pixels->pixels);
}
} // namespace ScreenshotEncoding


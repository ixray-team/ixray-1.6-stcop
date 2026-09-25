#include "StdAfx.h"
#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

#include "EmbeddedTexture.h"
namespace v_obj
{
    Texture LoadTexture(
        const unsigned char* data,
        size_t size)
    {
        if (!data || size == 0)
            return {};

        int width = 0;
        int height = 0;
        int channels = 0;

        unsigned char* pixels = stbi_load_from_memory(
            data,
            static_cast<int>(size),
            &width,
            &height,
            &channels,
            4);

        if (!pixels)
            return {};

        RHITextureDesc textureDesc{};
        textureDesc.Width = width;
        textureDesc.Height = height;
        textureDesc.MipLevels = 1;
        textureDesc.ArraySize = 1;
        textureDesc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
        textureDesc.SampleDescCount = 1;
		textureDesc.Usage = ERHI_USAGE::USAGE_DEFAULT;
		textureDesc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

        RHISubResource textureData{};

        textureData.Data = pixels;
        textureData.DataSize = width * 4;

        IRHISurface* texture = GRHI->CreateTexture2D(
            textureDesc,
            textureData);

        stbi_image_free(pixels);

        Texture result;

        result.srv = GRHI->CreateShaderResourceView(texture, nullptr);

        result.width = width;
        result.height = height;

        return result;
    }
}
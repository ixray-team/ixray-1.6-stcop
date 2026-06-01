#include "XRayTexture2D.h"

#include <RedImage/RedImage.hpp>

XRayTexture2D::XRayTexture2D(const shared_str& InName):XRayTexture(InName)
{
}

nri::Format ConvertFormatToNRI(RedImageTool::RedTexturePixelFormat InFormat,bool bSrgb)
{
    switch (InFormat) 
    {
    case RedImageTool::RedTexturePixelFormat::R8:
        return nri::Format::R8_UNORM;
    case RedImageTool::RedTexturePixelFormat::R8G8:
        return nri::Format::RG8_UNORM;
    case RedImageTool::RedTexturePixelFormat::R8G8B8:
        return bSrgb?nri::Format::RGBA8_SRGB:nri::Format::RGBA8_UNORM;
    case RedImageTool::RedTexturePixelFormat::R8G8B8A8:
        return bSrgb?nri::Format::RGBA8_SRGB:nri::Format::RGBA8_UNORM;
    case RedImageTool::RedTexturePixelFormat::R32F:
        return nri::Format::R32_SFLOAT;
    case RedImageTool::RedTexturePixelFormat::R32G32F:
        return nri::Format::RG32_SFLOAT;
    case RedImageTool::RedTexturePixelFormat::R32G32B32F:
        return nri::Format::RGB32_SFLOAT;
    case RedImageTool::RedTexturePixelFormat::R32G32B32A32F:
        return nri::Format::RGBA32_SFLOAT;
    case RedImageTool::RedTexturePixelFormat::BC1:
        return bSrgb?nri::Format::BC1_RGBA_SRGB:nri::Format::BC1_RGBA_UNORM;
    case RedImageTool::RedTexturePixelFormat::BC2:
        return bSrgb?nri::Format::BC2_RGBA_SRGB:nri::Format::BC2_RGBA_UNORM;
    case RedImageTool::RedTexturePixelFormat::BC3:
        return bSrgb?nri::Format::BC3_RGBA_SRGB:nri::Format::BC3_RGBA_UNORM;
    case RedImageTool::RedTexturePixelFormat::BC4:
        return nri::Format::BC4_R_UNORM;
    case RedImageTool::RedTexturePixelFormat::BC5:
        return nri::Format::BC5_RG_SNORM;
    case RedImageTool::RedTexturePixelFormat::BC6:
        return nri::Format::BC6H_RGB_UFLOAT;
    case RedImageTool::RedTexturePixelFormat::BC7:
        return bSrgb?nri::Format::BC7_RGBA_SRGB:nri::Format::BC7_RGBA_UNORM;
    }
}

bool XRayTexture2D::LoadFromFile(const char* FilePath, bool bSrgb)
{
    RedImageTool::RedImage RedImage;
    
    IReader* FileReader = FS.r_open(FilePath);
    if (!RedImage.LoadFromMemory(FileReader->pointer(), FileReader->length()))
    {
        FS.r_close(FileReader);
        return false;
    }
    FS.r_close(FileReader);

    if (RedImage.GetFormat() == RedImageTool::RedTexturePixelFormat::R8G8B8)
    {
        RedImage.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
    }
    return LoadFromImage(RedImage, bSrgb);
}

bool XRayTexture2D::LoadFromImage(const RedImageTool::RedImage& FromImage, bool bSrgb)
{
    if (FromImage.GetFormat() == RedImageTool::RedTexturePixelFormat::R8G8B8)
    {
        return false;
    }
    
    TextureDescription.type = nri::TextureType::TEXTURE_2D;
    TextureDescription.usage = nri::TextureUsageBits::SHADER_RESOURCE;
    TextureDescription.format = ConvertFormatToNRI(FromImage.GetFormat(), bSrgb);
    TextureDescription.width = FromImage.GetWidth();
    TextureDescription.height = FromImage.GetHeight();
    TextureDescription.mipNum = FromImage.GetMips();
    TextureDescription.layerNum = FromImage.GetDepth();

    NRI_CHECK(GRenderDevice.CoreInterface.CreatePlacedTexture(*GRenderDevice.Device, NriDeviceHeap, TextureDescription, Texture));

    nri::TextureViewDesc TextureViewDescription = {Texture, nri::TextureView::TEXTURE, TextureDescription.format};
    NRI_CHECK(GRenderDevice.CoreInterface.CreateTextureView(TextureViewDescription, Descriptor));

    xr_vector<nri::TextureSubresourceUploadDesc> SubresourceUploadDescriptions;
    const uint8_t* Pointer = static_cast<const uint8_t*>(*FromImage);
    for (int32_t i = 0; i < FromImage.GetDepth() ; i++)
    {
        for (int32_t a = 0; a < FromImage.GetMips() ; a++)
        {
            size_t Width = RedImageTool::RedTextureUtils::GetMip(FromImage.GetWidth() , a);
            size_t Height = RedImageTool::RedTextureUtils::GetMip(FromImage.GetHeight() , a);
            nri::TextureSubresourceUploadDesc& SubresourceUploadDescription = SubresourceUploadDescriptions.emplace_back();
            SubresourceUploadDescription.slices = Pointer; 
            SubresourceUploadDescription.rowPitch = RedImageTool::RedTextureUtils::GetSizeWidth(Width,FromImage.GetFormat());
            SubresourceUploadDescription.slicePitch = RedImageTool::RedTextureUtils::GetSizeDepth(Width,Height,FromImage.GetFormat());
            SubresourceUploadDescription.sliceNum = 1;
            Pointer += SubresourceUploadDescription.slicePitch;
        }
    }
  

    nri::TextureUploadDesc textureUploadDesc = {};
    textureUploadDesc.subresources = SubresourceUploadDescriptions.data();
    textureUploadDesc.texture = Texture;
    textureUploadDesc.after = {nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE};

    NRI_CHECK(GRenderDevice.HelperInterface.UploadData(*GRenderDevice.GraphicsQueue, &textureUploadDesc, 1, nullptr, 0));
    return true;
}

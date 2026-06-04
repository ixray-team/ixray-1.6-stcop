#include "TRenderTarget2D.h"

#include "TRenderTargetResourceProxy.h"

TRenderTarget2D::TRenderTarget2D(uint32_t InWidth, uint32_t InHeight, nri::Format InRenderTargetFormat, nri::ClearValue InClearValue, const shared_str& InName):TRenderTexture(InName)
{
    CheckIsGameThread();
    TextureDescription.type = nri::TextureType::TEXTURE_2D;
    TextureDescription.format = InRenderTargetFormat;
    TextureDescription.width = InWidth;
    TextureDescription.height = InHeight;
    TextureDescription.usage = nri::TextureUsageBits::SHADER_RESOURCE;
    TextureDescription.optimizedClearValue = InClearValue;
    
    switch (InRenderTargetFormat)
    {
    case nri::Format::D16_UNORM:
    case nri::Format::D32_SFLOAT:
    case nri::Format::D24_UNORM_S8_UINT:
    case nri::Format::D32_SFLOAT_S8_UINT:
        TextureDescription.usage |= nri::TextureUsageBits::DEPTH_STENCIL_ATTACHMENT;
        break;
    default:
        TextureDescription.usage |= nri::TextureUsageBits::COLOR_ATTACHMENT;
        break;
    }

    RenderTargetResourceProxy = new TRenderTargetResourceProxy;
    RenderTargetResourceProxy->TextureDescription = TextureDescription;
    ResourceProxy = RenderTargetResourceProxy;
    
    ENQUEUE_RENDER_COMMAND(TRenderTexture2D::LoadFromImage)([RenderTargetResourceProxy = RenderTargetResourceProxy]()
    {
        NRI_CHECK(GRenderDevice.CoreInterface.CreatePlacedTexture(*GRenderDevice.Device, NriDeviceHeap, RenderTargetResourceProxy->TextureDescription, RenderTargetResourceProxy->Texture));

        nri::TextureViewDesc TextureViewDescription = {RenderTargetResourceProxy->Texture, nri::TextureView::TEXTURE, RenderTargetResourceProxy->TextureDescription.format};
        NRI_CHECK(GRenderDevice.CoreInterface.CreateTextureView(TextureViewDescription, RenderTargetResourceProxy->Descriptor));


        nri::TextureViewDesc TextureViewAttachmentDescription = {RenderTargetResourceProxy->Texture, nri::TextureView::COLOR_ATTACHMENT, RenderTargetResourceProxy->TextureDescription.format};

        if (!!(RenderTargetResourceProxy->TextureDescription.usage &  nri::TextureUsageBits::DEPTH_STENCIL_ATTACHMENT))
        {
            TextureViewAttachmentDescription.type = nri::TextureView::DEPTH_STENCIL_ATTACHMENT;
        }
      
        NRI_CHECK(GRenderDevice.CoreInterface.CreateTextureView(TextureViewAttachmentDescription, RenderTargetResourceProxy->DescriptorAttachment));
    }); 
}

TRenderTarget2D::~TRenderTarget2D()
{
}

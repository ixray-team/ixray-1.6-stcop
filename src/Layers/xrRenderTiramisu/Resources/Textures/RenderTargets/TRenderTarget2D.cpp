#include "TRenderTarget2D.h"

TRenderTarget2D::TRenderTarget2D(uint32_t InWidth, uint32_t InHeight, nri::Format InRenderTargetFormat, nri::ClearValue InClearValue, const shared_str& InName):TRenderTexture(InName)
{
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

    NRI_CHECK(GRenderDevice.CoreInterface.CreatePlacedTexture(*GRenderDevice.Device, NriDeviceHeap, TextureDescription, Texture));

    nri::TextureViewDesc TextureViewDescription = {Texture, nri::TextureView::TEXTURE, TextureDescription.format};
    NRI_CHECK(GRenderDevice.CoreInterface.CreateTextureView(TextureViewDescription, Descriptor));
    
    
    nri::TextureViewDesc TextureViewAttachmentDescription = {Texture, nri::TextureView::COLOR_ATTACHMENT, TextureDescription.format};
    
    switch (InRenderTargetFormat)
    {
    case nri::Format::D16_UNORM:
    case nri::Format::D32_SFLOAT:
    case nri::Format::D24_UNORM_S8_UINT:
    case nri::Format::D32_SFLOAT_S8_UINT:
        TextureViewAttachmentDescription.type = nri::TextureView::DEPTH_STENCIL_ATTACHMENT;
        break;
    };
    
    NRI_CHECK(GRenderDevice.CoreInterface.CreateTextureView(TextureViewAttachmentDescription, DescriptorAttachment));
    
    LastAccessLayoutStage = {nri::AccessBits::NONE, nri::Layout::UNDEFINED};

}

TRenderTarget2D::~TRenderTarget2D()
{
    if (DescriptorAttachment)
    {
        GRenderDevice.CoreInterface.DestroyDescriptor(DescriptorAttachment);
    }
}

void TRenderTarget2D::SetNewAccessLayoutStage(nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage)
{
    TextureBarrierDescription.before = LastAccessLayoutStage;
    TextureBarrierDescription.after = AfterAccessLayoutStage;
    LastAccessLayoutStage = AfterAccessLayoutStage;
}

#pragma once

class XRayRenderTarget2D:public XRayTexture
{
public:
    XRayRenderTarget2D(uint32_t InWidth, uint32_t InHeight,nri::Format InRenderTargetFormat, nri::ClearValue InClearValue = {}, const shared_str& InName = "None");
    ~XRayRenderTarget2D();
    
    void SetNewAccessLayoutStage(nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage);
    
    nri::AccessLayoutStage LastAccessLayoutStage;
    nri::Descriptor*    DescriptorAttachment = nullptr;
};

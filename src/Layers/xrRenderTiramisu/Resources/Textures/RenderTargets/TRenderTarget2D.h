#pragma once

class TRenderTarget2D:public TRenderTexture
{
public:
            TRenderTarget2D         (uint32_t InWidth, uint32_t InHeight,nri::Format InRenderTargetFormat, nri::ClearValue InClearValue = {}, const shared_str& InName = "None");
            ~TRenderTarget2D        ();
    
    void    SetNewAccessLayoutStage (nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage);
    
    nri::AccessLayoutStage LastAccessLayoutStage;
    nri::Descriptor*    DescriptorAttachment = nullptr;
};

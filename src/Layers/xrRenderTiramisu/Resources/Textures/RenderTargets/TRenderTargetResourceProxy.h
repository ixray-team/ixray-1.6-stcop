#pragma once

class TRenderTargetResourceProxy:public TRenderTextureResourceProxy
{
public:
    virtual                         ~TRenderTargetResourceProxy     () override;
    void                            SetNewAccessLayoutStage         (nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage);
    
    nri::AccessLayoutStage          LastAccessLayoutStage;
    nri::Descriptor*                DescriptorAttachment = nullptr;
};

#pragma once

#include "TiramisuRenderTypes.h"

// Render-thread proxy render target с attachment descriptors.
class TiramisuRenderTargetResourceProxy:public TiramisuRenderTextureResourceProxy
{
public:
    virtual                         ~TiramisuRenderTargetResourceProxy     () override;
    void                            SetNewAccessLayoutStage         (nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage);
    
    nri::AccessLayoutStage          LastAccessLayoutStage;
    nri::Descriptor*                DescriptorAttachment = nullptr;
};

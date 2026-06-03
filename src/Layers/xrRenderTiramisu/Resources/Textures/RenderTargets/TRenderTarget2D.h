#pragma once
#include "TRenderTargetResourceProxy.h"

class TRenderTarget2D:public TRenderTexture
{
public:
                    TRenderTarget2D         (uint32_t InWidth, uint32_t InHeight,nri::Format InRenderTargetFormat, nri::ClearValue InClearValue = {}, const shared_str& InName = "None");
    virtual         ~TRenderTarget2D        () override;
    

    
    TRenderTargetResourceProxy*	    RenderTargetResourceProxy = nullptr;
};

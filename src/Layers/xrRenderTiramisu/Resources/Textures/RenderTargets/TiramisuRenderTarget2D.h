#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderTargetResourceProxy.h"

// Двумерная texture, используемая как render target.
class TiramisuRenderTarget2D : public TiramisuRenderTexture
{
public:
                    TiramisuRenderTarget2D         (u32 InWidth, u32 InHeight,nri::Format InRenderTargetFormat, nri::ClearValue InClearValue = {}, const shared_str& InName = "None");
    virtual         ~TiramisuRenderTarget2D        () override;
    

    
    TiramisuRenderTargetResourceProxy*	    RenderTargetResourceProxy = nullptr;
};

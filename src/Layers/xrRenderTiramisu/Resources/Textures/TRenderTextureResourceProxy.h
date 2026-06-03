#pragma once
#include "Resources/TRenderResourceProxy.h"

class TRenderTextureResourceProxy:public TRenderResourceProxy
{
public:
                        ~TRenderTextureResourceProxy() override;
    nri::Texture*       Texture = nullptr;
    nri::TextureDesc    TextureDescription  = {};
};

#pragma once
#include "TMaterialRenderProxy.h"

class TMaterialInstanceDynamicRenderProxy:public TMaterialRenderProxy
{
public:
    virtual const xr_map<EVertexType,nri::Pipeline*>&   GetPipelines    () const override;
    virtual TRenderTextureResourceProxy*                GetTexture      () const override;
    TMaterialRenderProxy*                               ParentMaterialRenderProxy = nullptr;
    TRenderTextureResourceProxy*                        TextureResourceProxy = nullptr;
};

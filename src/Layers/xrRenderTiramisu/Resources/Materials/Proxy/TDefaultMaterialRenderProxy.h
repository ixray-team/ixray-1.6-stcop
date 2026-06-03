#pragma once
#include "TMaterialRenderProxy.h"

class TDefaultMaterialRenderProxy:public TMaterialRenderProxy
{
public:
    TDefaultMaterialRenderProxy();
    virtual                                             ~TDefaultMaterialRenderProxy    ();
    virtual const xr_map<EVertexType,nri::Pipeline*>&   GetPipelines                    () const;
    virtual TRenderTextureResourceProxy*                GetTexture                      () const override;
    
    xr_map<EVertexType,nri::Pipeline*>                  Pipelines;
    TRenderTextureResourceProxy*                        TextureResourceProxy = nullptr;
};

#pragma once

class TMaterialRenderProxy
{
public:
    virtual                                             ~TMaterialRenderProxy   ();
    virtual const xr_map<EVertexType,nri::Pipeline*>&   GetPipelines            () const = 0;
    virtual TRenderTextureResourceProxy*                GetTexture              () const = 0; 
    
#ifdef DEBUG
    class TRenderMaterialInterface*                     DebugOwner = nullptr;
#endif
};

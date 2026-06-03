#include "TMaterialInstanceDynamicRenderProxy.h"

const xr_map<EVertexType, nri::Pipeline*>& TMaterialInstanceDynamicRenderProxy::GetPipelines() const
{
    VERIFY(ParentMaterialRenderProxy);
    return ParentMaterialRenderProxy->GetPipelines();
}

TRenderTextureResourceProxy* TMaterialInstanceDynamicRenderProxy::GetTexture() const
{
    if (TextureResourceProxy)
    {
        return TextureResourceProxy;
    }
    
    VERIFY(ParentMaterialRenderProxy);
    return ParentMaterialRenderProxy->GetTexture();
}

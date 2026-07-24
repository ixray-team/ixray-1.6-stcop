#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuMaterialRenderProxy.h"
#include <utility>

// Render proxy стандартного или диагностического master material.
class TiramisuDefaultMaterialRenderProxy : public TiramisuMaterialRenderProxy
{
public:
    explicit TiramisuDefaultMaterialRenderProxy(FMaterialAssetId InAssetReference);
    virtual ~TiramisuDefaultMaterialRenderProxy();
    [[nodiscard]] xr_optional<FMaterialPassProxy> ResolvePass(EMaterialPass Pass, EVertexType VertexType) const override;
    [[nodiscard]] const FMaterialAssetId& GetAssetReference() const override;
    [[nodiscard]] xr_span<const FMaterialTextureParameterBinding> GetTextureParameters() const override;
    virtual TiramisuRenderTextureResourceProxy* GetTexture() const override;

    // Одна material permutation и созданный для неё NRI pipeline.
    struct FPipelineEntry
    {
        FMaterialPipelineHandle Handle;
        u64 PipelineKey = 0;
        xr_string VertexFactory;
    };

    using FPipelineMapKey = xr_pair<EMaterialPass, EVertexType>;
    xr_map<FPipelineMapKey, FPipelineEntry>             Pipelines;
    TiramisuRenderTextureResourceProxy*                        TextureResourceProxy = nullptr;
    FMaterialAssetId                AssetReference;

private:
    void Initialize_RenderThread(const FMaterialAssetId& AssetReference);
};

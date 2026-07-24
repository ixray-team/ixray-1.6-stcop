#pragma once

#include "TiramisuRenderTypes.h"

#include <MaterialRuntime.h>
#include <optional>
#include <span>
#include <vector>

class TiramisuRenderTextureResourceProxy;

// Bindless texture descriptor, привязанный к стабильному parameter id.
struct FMaterialTextureParameterBinding
{
    FMaterialParameterId Parameter;
    TiramisuRenderTextureResourceProxy* Texture = nullptr;
};

// Базовый render-thread proxy материала для разрешения pass pipeline и параметров.
class TiramisuMaterialRenderProxy
{
public:
    virtual                                             ~TiramisuMaterialRenderProxy   ();
    [[nodiscard]] virtual xr_optional<FMaterialPassProxy>
                                                        ResolvePass             (EMaterialPass Pass, EVertexType VertexType) const = 0;
    [[nodiscard]] virtual const FMaterialAssetId&
                                                        GetAssetReference       () const = 0;
    [[nodiscard]] virtual xr_span<const FMaterialTextureParameterBinding>
                                                        GetTextureParameters    () const = 0;
    virtual TiramisuRenderTextureResourceProxy*                GetTexture              () const = 0; 
    
#ifdef DEBUG
    // Общий runtime-контракт master material и его instances.
    class TiramisuRenderMaterialInterface*                     DebugOwner = nullptr;
#endif
};

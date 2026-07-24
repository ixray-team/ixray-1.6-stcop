#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuMaterialRenderProxy.h"

// Render-thread proxy динамического material instance.
class TiramisuMaterialInstanceDynamicRenderProxy : public TiramisuMaterialRenderProxy
{
public:
	[[nodiscard]] xr_optional<FMaterialPassProxy>
	ResolvePass(EMaterialPass Pass, EVertexType VertexType) const override;
	[[nodiscard]] const FMaterialAssetId&
	GetAssetReference() const override;
	[[nodiscard]] xr_span<const FMaterialTextureParameterBinding>
	GetTextureParameters() const override;
	virtual TiramisuRenderTextureResourceProxy* GetTexture() const override;
	TiramisuMaterialRenderProxy* ParentMaterialRenderProxy = nullptr;
	xr_vector<FMaterialTextureParameterBinding> TextureParameters;
};

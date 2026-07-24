#include "TiramisuMaterialInstanceDynamicRenderProxy.h"

#include <LegacyMaterialResolver.h>

#include <algorithm>

xr_optional<FMaterialPassProxy> TiramisuMaterialInstanceDynamicRenderProxy::ResolvePass(const EMaterialPass Pass, const EVertexType VertexType) const
{
	CheckIsRenderThread();
	VERIFY(ParentMaterialRenderProxy);
	return ParentMaterialRenderProxy->ResolvePass(Pass, VertexType);
}

TiramisuRenderTextureResourceProxy* TiramisuMaterialInstanceDynamicRenderProxy::GetTexture() const
{
	CheckIsRenderThread();
	const auto BaseTexture = std::ranges::find_if
	(
		TextureParameters, 
		[](const FMaterialTextureParameterBinding& Binding)
		{
			return Binding.Parameter.Value == LegacyBaseTextureParameterId; 
		}
	);

	if (BaseTexture != TextureParameters.end() && BaseTexture->Texture)
	{
		return BaseTexture->Texture;
	}

	VERIFY(ParentMaterialRenderProxy);
	return ParentMaterialRenderProxy->GetTexture();
}

const FMaterialAssetId& TiramisuMaterialInstanceDynamicRenderProxy::GetAssetReference() const
{
	CheckIsRenderThread();
	VERIFY(ParentMaterialRenderProxy);
	return ParentMaterialRenderProxy->GetAssetReference();
}

xr_span<const FMaterialTextureParameterBinding> TiramisuMaterialInstanceDynamicRenderProxy::GetTextureParameters() const
{
	CheckIsRenderThread();
	if (!TextureParameters.empty())
	{
		return TextureParameters;
	}
	VERIFY(ParentMaterialRenderProxy);
	return ParentMaterialRenderProxy->GetTextureParameters();
}

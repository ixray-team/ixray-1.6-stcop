#include "TiramisuRenderMaterialInstanceDynamic.h"
#include "TiramisuRenderMaterialsManager.h"
#include "Proxy/TiramisuMaterialInstanceDynamicRenderProxy.h"

#include <LegacyMaterialResolver.h>

#include <algorithm>

TiramisuRenderMaterialInstanceDynamic::TiramisuRenderMaterialInstanceDynamic(TiramisuRenderMaterialInterface* InParent)
	: Parent(InParent)
{
	CheckIsGameThread();
	MaterialInstanceRenderProxy = new TiramisuMaterialInstanceDynamicRenderProxy;
	MaterialInstanceRenderProxy->ParentMaterialRenderProxy = InParent->MaterialRenderProxy;
#ifdef DEBUG
	MaterialInstanceRenderProxy->DebugOwner = this;
#endif

	MaterialRenderProxy = MaterialInstanceRenderProxy;
}

TiramisuRenderMaterialInstanceDynamic::~TiramisuRenderMaterialInstanceDynamic()
{
	CheckIsGameThread();
	for (const auto& [Parameter, Texture] : TextureParameters)
	{
		GRenderResourcesManager->TexturesManager->Free(Texture);
	}
	GRenderResourcesManager->MaterialsManager->Free(Parent);
}

void TiramisuRenderMaterialInstanceDynamic::SetTexture(TiramisuRenderTexture* NewTexture)
{
	SetTextureParameter(FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)}, NewTexture);
}

void TiramisuRenderMaterialInstanceDynamic::SetTextureParameter(
	const FMaterialParameterId& Parameter,
	TiramisuRenderTexture* NewTexture
)
{
	CheckIsGameThread();
	VERIFY(Parameter.IsValid());
	VERIFY(NewTexture && NewTexture->ResourceProxy);

	ENQUEUE_RENDER_COMMAND(TiramisuRenderMaterialInstanceDynamic::SetTextureParameter)(
		[MaterialInstanceRenderProxy = MaterialInstanceRenderProxy,
		 Parameter,
		 TextureProxy = NewTexture->ResourceProxy]()
		{
			CheckIsRenderThread();
			auto Existing = std::ranges::find_if(
				MaterialInstanceRenderProxy->TextureParameters,
				[&Parameter](const FMaterialTextureParameterBinding& Binding)
				{
					return Binding.Parameter == Parameter;
				}
			);
			if (Existing == MaterialInstanceRenderProxy->TextureParameters.end())
			{
				MaterialInstanceRenderProxy->TextureParameters.push_back(
					{Parameter, TextureProxy}
				);
				std::ranges::sort(MaterialInstanceRenderProxy->TextureParameters, {}, &FMaterialTextureParameterBinding::Parameter);
			}
			else
			{
				Existing->Texture = TextureProxy;
			}
		}
	);

	if (const auto Existing = TextureParameters.find(Parameter);
		Existing != TextureParameters.end())
	{
		GRenderResourcesManager->TexturesManager->Free(Existing->second);
		Existing->second = NewTexture;
	}
	else
	{
		TextureParameters.emplace(Parameter, NewTexture);
	}
}

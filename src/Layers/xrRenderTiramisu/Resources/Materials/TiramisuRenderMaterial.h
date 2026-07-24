#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderMaterialInterface.h"

class TiramisuDefaultMaterialRenderProxy;

// Runtime master material, создающий proxy для выбранной permutation.
class TiramisuRenderMaterial : public TiramisuRenderMaterialInterface
{
public:
	TiramisuRenderMaterial(const shared_str& InAssetReference);
	~TiramisuRenderMaterial() override;

	TiramisuRenderTexture* Texture = nullptr;
	TiramisuDefaultMaterialRenderProxy* DefaultMaterialRenderProxy;
};

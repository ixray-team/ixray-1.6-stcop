#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderMaterialInterface.h"

class TiramisuMaterialInstanceDynamicRenderProxy;

// Изменяемый material instance; static parameters после создания запрещены.
class TiramisuRenderMaterialInstanceDynamic : public TiramisuRenderMaterialInterface
{
public:
                                        TiramisuRenderMaterialInstanceDynamic  (TiramisuRenderMaterialInterface* InParent);
                                        ~TiramisuRenderMaterialInstanceDynamic ();
    void                                SetTexture                      (TiramisuRenderTexture* NewTexture);
    void                                SetTextureParameter             (
                                            const FMaterialParameterId& Parameter,
                                            TiramisuRenderTexture* NewTexture);
private:
    xr_map<FMaterialParameterId, TiramisuRenderTexture*>
                                        TextureParameters;
    TiramisuRenderMaterialInterface*           Parent = nullptr;
    TiramisuMaterialInstanceDynamicRenderProxy*       MaterialInstanceRenderProxy = nullptr;
};

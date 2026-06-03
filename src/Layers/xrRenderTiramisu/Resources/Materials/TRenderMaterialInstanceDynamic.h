#pragma once
#include "TRenderMaterialInterface.h"

class TMaterialInstanceDynamicRenderProxy;

class TRenderMaterialInstanceDynamic:public TRenderMaterialInterface
{
public:
                                        TRenderMaterialInstanceDynamic  (TRenderMaterialInterface* Parent);
                                        ~TRenderMaterialInstanceDynamic ();
    void                                SetTexture                      (TRenderTexture* NewTexture);
private:
    TRenderTexture*                     Texture = nullptr;
    TRenderMaterialInterface*           Parent = nullptr;
    TMaterialInstanceDynamicRenderProxy*       MaterialInstanceRenderProxy = nullptr;
};

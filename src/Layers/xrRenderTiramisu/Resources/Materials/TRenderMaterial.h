#pragma once
#include "TRenderMaterialInterface.h"

class TDefaultMaterialRenderProxy;

class TRenderMaterial: public TRenderMaterialInterface
{
public:
                                        TRenderMaterial     ();
                                        ~TRenderMaterial    () override;
    
    TRenderTexture*                     Texture = nullptr;
    TDefaultMaterialRenderProxy*        DefaultMaterialRenderProxy;
};

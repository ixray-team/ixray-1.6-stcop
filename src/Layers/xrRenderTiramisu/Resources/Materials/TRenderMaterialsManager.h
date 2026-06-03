#pragma once
#include "TRenderMaterial.h"
#include "TRenderMaterialInstanceDynamic.h"

class TRenderMaterialsManager
{
public:
                                                        TRenderMaterialsManager     ();
                                                        ~TRenderMaterialsManager    ();
    
    TRenderMaterialInterface*                           Get                         (const shared_str& InName);
    TRenderMaterialInstanceDynamic*                     CreateInstanceDynamic       (const shared_str& InName,TRenderMaterialInterface* Parent);
    void                                                Free                        (TRenderMaterialInterface* Material);
    TRenderMaterialInterface*				            Copy					    (TRenderMaterialInterface* Material);
    
private:
    xr_map<shared_str, TRenderMaterialInterface*>		Materials;
};

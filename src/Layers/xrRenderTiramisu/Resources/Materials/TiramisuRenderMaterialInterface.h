#pragma once

#include "TiramisuRenderTypes.h"

#include <MaterialTypes.h>

class TiramisuMaterialRenderProxy;
// Общий runtime-контракт master material и его instances.
class TiramisuRenderMaterialInterface
{
public:
    
    virtual                             ~TiramisuRenderMaterialInterface    ();
    TiramisuMaterialRenderProxy*               MaterialRenderProxy = nullptr;
    shared_str              	        Name = "";
    u32                	        Counter = 1;
    FMaterialHandle CoreMaterialHandle;
};

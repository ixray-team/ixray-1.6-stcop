#pragma once

class TMaterialRenderProxy;
class TRenderMaterialInterface
{
public:
    
    virtual                             ~TRenderMaterialInterface    ();
    TMaterialRenderProxy*               MaterialRenderProxy = nullptr;
    shared_str              	        Name = "";
    uint32_t                	        Counter = 1;
};

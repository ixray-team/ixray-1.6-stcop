#include "TiramisuRenderMaterialInterface.h"
#include "Proxy/TiramisuMaterialRenderProxy.h"

TiramisuRenderMaterialInterface::~TiramisuRenderMaterialInterface()
{
    CheckIsGameThread();
    VERIFY(Counter == 0);
    ENQUEUE_RENDER_COMMAND(TiramisuRenderMaterial::~TiramisuRenderMaterial)([RenderProxy = MaterialRenderProxy]()
    {
        CheckIsRenderThread();
        delete RenderProxy;
    });
}

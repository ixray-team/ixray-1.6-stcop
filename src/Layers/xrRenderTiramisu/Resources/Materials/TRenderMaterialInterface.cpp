#include "TRenderMaterialInterface.h"
#include "Proxy/TMaterialRenderProxy.h"

TRenderMaterialInterface::~TRenderMaterialInterface()
{
    VERIFY(Counter == 0);
    ENQUEUE_RENDER_COMMAND(TRenderMaterial::~TRenderMaterial)([RenderProxy = MaterialRenderProxy]()
    {
        delete RenderProxy;
    });
}

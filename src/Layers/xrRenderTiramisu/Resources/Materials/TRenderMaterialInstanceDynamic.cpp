#include "TRenderMaterialInstanceDynamic.h"
#include "TRenderMaterialsManager.h"
#include "Proxy/TMaterialInstanceDynamicRenderProxy.h"

TRenderMaterialInstanceDynamic::TRenderMaterialInstanceDynamic(TRenderMaterialInterface* Parent)
{
    Texture = GRenderResourcesManager->BlackTexture;
    
    MaterialInstanceRenderProxy = new TMaterialInstanceDynamicRenderProxy;
    MaterialInstanceRenderProxy->ParentMaterialRenderProxy = Parent->MaterialRenderProxy;
#ifdef DEBUG
    MaterialInstanceRenderProxy->DebugOwner = this;
#endif
    
    MaterialRenderProxy = MaterialInstanceRenderProxy;
}

TRenderMaterialInstanceDynamic::~TRenderMaterialInstanceDynamic()
{
    GRenderResourcesManager->TexturesManager->Free(Texture);
    GRenderResourcesManager->MaterialsManager->Free(Parent);
}

void TRenderMaterialInstanceDynamic::SetTexture(TRenderTexture* NewTexture)
{
    CheckIsGameThread();
    ENQUEUE_RENDER_COMMAND(TRenderMaterialInstanceDynamic::SetTexture)([MaterialInstanceRenderProxy = MaterialInstanceRenderProxy,NewTexture = NewTexture->ResourceProxy]()
    {
        MaterialInstanceRenderProxy->TextureResourceProxy = NewTexture;
    });
    
    GRenderResourcesManager->TexturesManager->Free(Texture);
    Texture = NewTexture;
}

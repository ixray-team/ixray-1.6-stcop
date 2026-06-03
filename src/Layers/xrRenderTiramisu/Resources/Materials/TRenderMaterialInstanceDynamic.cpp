#include "TRenderMaterialInstanceDynamic.h"
#include "TRenderMaterialsManager.h"
#include "Proxy/TMaterialInstanceDynamicRenderProxy.h"

TRenderMaterialInstanceDynamic::TRenderMaterialInstanceDynamic(TRenderMaterialInterface* Parent)
{
    Texture = GRenderResourcesManager->BlackTexture;
    
    MaterialInstanceRenderProxy = new TMaterialInstanceDynamicRenderProxy;
    MaterialInstanceRenderProxy->ParentMaterialRenderProxy = Parent->MaterialRenderProxy;
    MaterialInstanceRenderProxy->DebugOwner = this;
    
    MaterialRenderProxy = MaterialInstanceRenderProxy;
}

TRenderMaterialInstanceDynamic::~TRenderMaterialInstanceDynamic()
{
    GRenderResourcesManager->TexturesManager->Free(Texture);
    GRenderResourcesManager->MaterialsManager->Free(Parent);
}

void TRenderMaterialInstanceDynamic::SetTexture(TRenderTexture* NewTexture)
{
    ENQUEUE_RENDER_COMMAND(TRenderMaterialInstanceDynamic::SetTexture)([MaterialInstanceRenderProxy = MaterialInstanceRenderProxy,NewTexture = NewTexture->ResourceProxy]()
    {
        MaterialInstanceRenderProxy->TextureResourceProxy = NewTexture;
    });
    
    GRenderResourcesManager->TexturesManager->Free(Texture);
    Texture = NewTexture;
}

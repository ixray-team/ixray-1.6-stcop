#include "TRenderMaterial.h"
#include "Proxy/TDefaultMaterialRenderProxy.h"


TRenderMaterial::TRenderMaterial()
{
    Texture = GRenderResourcesManager->BlackTexture;
    
    DefaultMaterialRenderProxy = new TDefaultMaterialRenderProxy;
    DefaultMaterialRenderProxy->TextureResourceProxy = Texture->ResourceProxy;
#ifdef DEBUG
    DefaultMaterialRenderProxy->DebugOwner = this;
#endif
    MaterialRenderProxy = DefaultMaterialRenderProxy;
}

TRenderMaterial::~TRenderMaterial()
{
    GRenderResourcesManager->TexturesManager->Free(Texture);
}

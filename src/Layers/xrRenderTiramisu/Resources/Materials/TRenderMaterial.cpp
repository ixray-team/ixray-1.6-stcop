#include "TRenderMaterial.h"
#include "Proxy/TDefaultMaterialRenderProxy.h"


TRenderMaterial::TRenderMaterial()
{
    Texture = GRenderResourcesManager->BlackTexture;
    
    DefaultMaterialRenderProxy = new TDefaultMaterialRenderProxy;
    DefaultMaterialRenderProxy->TextureResourceProxy = Texture->ResourceProxy;
    DefaultMaterialRenderProxy->DebugOwner = this;
    
    MaterialRenderProxy = DefaultMaterialRenderProxy;
}

TRenderMaterial::~TRenderMaterial()
{
    GRenderResourcesManager->TexturesManager->Free(Texture);
}

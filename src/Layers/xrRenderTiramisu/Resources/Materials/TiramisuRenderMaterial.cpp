#include "TiramisuRenderMaterial.h"
#include "Proxy/TiramisuDefaultMaterialRenderProxy.h"


TiramisuRenderMaterial::TiramisuRenderMaterial(const shared_str& InAssetReference)
{
    CheckIsGameThread();
    Texture = GRenderResourcesManager->BlackTexture;

    DefaultMaterialRenderProxy = new TiramisuDefaultMaterialRenderProxy(
        FMaterialAssetId{InAssetReference.c_str()});
    DefaultMaterialRenderProxy->TextureResourceProxy = Texture->ResourceProxy;
#ifdef DEBUG
    DefaultMaterialRenderProxy->DebugOwner = this;
#endif
    MaterialRenderProxy = DefaultMaterialRenderProxy;
}

TiramisuRenderMaterial::~TiramisuRenderMaterial()
{
    CheckIsGameThread();
    GRenderResourcesManager->TexturesManager->Free(Texture);
}

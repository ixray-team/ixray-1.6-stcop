#include "TRenderTextureResourceProxy.h"

TRenderTextureResourceProxy::~TRenderTextureResourceProxy()
{
    if (Texture)
    {
        GRenderDevice.CoreInterface.DestroyTexture(Texture);
    }
}

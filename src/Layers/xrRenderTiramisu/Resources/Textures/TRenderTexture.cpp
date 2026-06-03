#include "TRenderTexture.h"

TRenderTexture::TRenderTexture(const shared_str& InName): Name(InName)
{
}

TRenderTexture::~TRenderTexture()
{
    if (Owner)
    {
        VERIFY(Counter == 0);
    }
    if (ResourceProxy)
    {
        ENQUEUE_RENDER_COMMAND(RemoveTexture)([InResourceProxy = ResourceProxy]()
       {
           delete InResourceProxy;
       }); 
    }
}

void TRenderTexture::Update()
{
}

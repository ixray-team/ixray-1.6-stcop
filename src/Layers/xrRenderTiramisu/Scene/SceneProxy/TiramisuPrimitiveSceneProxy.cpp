#include "TiramisuPrimitiveSceneProxy.h"

TiramisuPrimitiveSceneProxy::TiramisuPrimitiveSceneProxy()
{
    CheckIsGameThread();
}

TiramisuPrimitiveSceneProxy::~TiramisuPrimitiveSceneProxy()
{
    CheckIsRenderThread();
}

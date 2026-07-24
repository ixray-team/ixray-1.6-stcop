#include "TiramisuMaterialRenderProxy.h"

TiramisuMaterialRenderProxy::~TiramisuMaterialRenderProxy()
{
	CheckIsRenderThread();
}

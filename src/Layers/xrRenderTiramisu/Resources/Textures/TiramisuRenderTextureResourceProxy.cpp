#include "TiramisuRenderTextureResourceProxy.h"

TiramisuRenderTextureResourceProxy::~TiramisuRenderTextureResourceProxy()
{
	CheckIsRenderThread();
	if (Texture)
	{
		GRenderDevice.CoreInterface.DestroyTexture(Texture);
	}
}

#include "TiramisuRenderTexture.h"

TiramisuRenderTexture::TiramisuRenderTexture(const shared_str& InName)
	: Name(InName)
{
	CheckIsGameThread();
}

TiramisuRenderTexture::~TiramisuRenderTexture()
{
	CheckIsGameThread();
	if (Owner)
	{
		VERIFY(Counter == 0);
	}
	if (ResourceProxy)
	{
		ENQUEUE_RENDER_COMMAND(RemoveTexture)([InResourceProxy = ResourceProxy]()
											  {
           CheckIsRenderThread();
           delete InResourceProxy; });
	}
}

void TiramisuRenderTexture::Update()
{
	CheckIsGameThread();
}

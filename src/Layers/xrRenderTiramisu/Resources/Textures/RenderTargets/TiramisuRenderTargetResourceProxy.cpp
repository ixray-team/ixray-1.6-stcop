#include "TiramisuRenderTargetResourceProxy.h"

TiramisuRenderTargetResourceProxy::~TiramisuRenderTargetResourceProxy()
{
	CheckIsRenderThread();
	if (DescriptorAttachment)
	{
		GRenderDevice.CoreInterface.DestroyDescriptor(DescriptorAttachment);
		DescriptorAttachment = nullptr;
	}
	LastAccessLayoutStage = {nri::AccessBits::NONE, nri::Layout::UNDEFINED};
}

void TiramisuRenderTargetResourceProxy::SetNewAccessLayoutStage(nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage)
{
	CheckIsRenderThread();
	TextureBarrierDescription.before = LastAccessLayoutStage;
	TextureBarrierDescription.after = AfterAccessLayoutStage;
	LastAccessLayoutStage = AfterAccessLayoutStage;
}

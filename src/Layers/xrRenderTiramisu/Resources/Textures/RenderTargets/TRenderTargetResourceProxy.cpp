#include "TRenderTargetResourceProxy.h"

TRenderTargetResourceProxy::~TRenderTargetResourceProxy()
{
    LastAccessLayoutStage = {nri::AccessBits::NONE, nri::Layout::UNDEFINED};
}

void TRenderTargetResourceProxy::SetNewAccessLayoutStage(nri::TextureBarrierDesc& TextureBarrierDescription, nri::AccessLayoutStage AfterAccessLayoutStage)
{
    TextureBarrierDescription.before = LastAccessLayoutStage;
    TextureBarrierDescription.after = AfterAccessLayoutStage;
    LastAccessLayoutStage = AfterAccessLayoutStage;
}

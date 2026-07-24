#include "TiramisuRenderResourceProxy.h"

TiramisuRenderResourceProxy::TiramisuRenderResourceProxy()
= default;

TiramisuRenderResourceProxy::~TiramisuRenderResourceProxy()
{
    CheckIsRenderThread();
    if (Descriptor)
    {
        GRenderDevice.CoreInterface.DestroyDescriptor(Descriptor);
    }
    if (HeapID != INDEX_NONE)
    {
        GRenderResourcesManager->DescriptorHeapAllocator->Free(HeapID);
    }
}

u32 TiramisuRenderResourceProxy::GetOrCreateHeapID()
{
    CheckIsRenderThread();
    if (HeapID == INDEX_NONE)
    {
        HeapID = GRenderResourcesManager->DescriptorHeapAllocator->Alloc(Descriptor);
    }
    return HeapID;
}

u32 TiramisuRenderResourceProxy::GetHeapID() const
{
    CheckIsRenderThread();
    return HeapID;
}

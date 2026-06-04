#include "TRenderResourceProxy.h"

TRenderResourceProxy::TRenderResourceProxy()
= default;

TRenderResourceProxy::~TRenderResourceProxy()
{
    if (Descriptor)
    {
        GRenderDevice.CoreInterface.DestroyDescriptor(Descriptor);
    }
    if (HeapID != INDEX_NONE)
    {
        GRenderResourcesManager->DescriptorHeapAllocator->Free(HeapID);
    }
}

u32 TRenderResourceProxy::GetOrCreateHeapID()
{
    CheckIsRenderThread();
    if (HeapID == INDEX_NONE)
    {
        HeapID = GRenderResourcesManager->DescriptorHeapAllocator->Alloc(Descriptor);
    }
    return HeapID;
}

u32 TRenderResourceProxy::GetHeapID() const
{
    return HeapID;
}

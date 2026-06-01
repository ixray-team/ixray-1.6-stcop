#include "XRayTexture.h"

XRayTexture::XRayTexture(const shared_str& InName): Name(InName)
{
}

XRayTexture::~XRayTexture()
{
    if (Descriptor)
    {
        GRenderDevice.CoreInterface.DestroyDescriptor(Descriptor);
        Descriptor = nullptr;
    }
    
    if (Texture)
    {
        GRenderDevice.CoreInterface.DestroyTexture(Texture);
        Texture = nullptr;
    }
    
    if (HeapIndex != INDEX_NONE)
    {
        GRenderResourcesManager->DescriptorHeapAllocator->Free(HeapIndex);
    }
}

uint32_t XRayTexture::GetOrCreateHeapIndex()
{
    if (HeapIndex == INDEX_NONE && Descriptor)
    {
        HeapIndex = GRenderResourcesManager->DescriptorHeapAllocator->Alloc(Descriptor);
    }

    return HeapIndex;
}

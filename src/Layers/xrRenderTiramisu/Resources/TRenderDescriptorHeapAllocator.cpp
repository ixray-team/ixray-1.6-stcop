#include "TRenderDescriptorHeapAllocator.h"
#include "TRenderResourcesManager.h"

TRenderDescriptorHeapAllocator::TRenderDescriptorHeapAllocator()
{
    DescriptorCaches.emplace_back(new FXRayDescriptorCache);
}

TRenderDescriptorHeapAllocator::~TRenderDescriptorHeapAllocator()
{
    FlushNextFrame();
    while (!DescriptorCaches.empty())
    {
        delete DescriptorCaches.back();
        DescriptorCaches.pop_back();
    }
    VERIFY(FreeIndexes.size() == NextIndex);
}

uint32_t TRenderDescriptorHeapAllocator::Alloc(nri::Descriptor* InDescriptor)
{
    uint32_t Index = 0;
    if (!FreeIndexes.empty())
    {
        Index = FreeIndexes.back();
        FreeIndexes.pop_back();
    }
    else
    {
        R_ASSERT(NextIndex < 2048);
        Index = NextIndex++;
    }
    
    FXRayDescriptorCache* InDescriptorPool = DescriptorCaches.back();
    
    nri::UpdateDescriptorRangeDesc&UpdateDescriptorRangeDescription =  UpdateDescriptorRangesDescriptions.emplace_back();
    UpdateDescriptorRangeDescription.descriptorSet = GRenderResourcesManager->ResourcesDescriptorSet;
    UpdateDescriptorRangeDescription.rangeIndex = 0;
    UpdateDescriptorRangeDescription.baseDescriptor = Index;
    InDescriptorPool->Descriptors[InDescriptorPool->Index] = InDescriptor;
    UpdateDescriptorRangeDescription.descriptors = &InDescriptorPool->Descriptors[InDescriptorPool->Index++];
    UpdateDescriptorRangeDescription.descriptorNum = 1;
    
    if (InDescriptorPool->Index == 256)
    {
        DescriptorCaches.emplace_back(new FXRayDescriptorCache);
    }
    return Index;
}

void TRenderDescriptorHeapAllocator::Free(uint32_t Index)
{
    FreeIndexesForNextFrame.push_back(Index);
}

void TRenderDescriptorHeapAllocator::FlushNextFrame()
{
    FreeIndexes.append_range(FreeIndexesForNextFrame);
    FreeIndexesForNextFrame.clear();
}

void TRenderDescriptorHeapAllocator::UpdateDescriptorRanges()
{
    GRenderDevice.CoreInterface.UpdateDescriptorRanges(UpdateDescriptorRangesDescriptions.data(), UpdateDescriptorRangesDescriptions.size());
    UpdateDescriptorRangesDescriptions.clear();
    
    while (DescriptorCaches.size() > 1)
    {
        delete DescriptorCaches.back();
        DescriptorCaches.pop_back();
    }
    DescriptorCaches[0]->Index = 0;
}

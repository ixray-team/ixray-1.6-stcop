#include "TiramisuRenderDescriptorHeapAllocator.h"
#include "TiramisuRenderResourcesManager.h"

TiramisuRenderDescriptorHeapAllocator::TiramisuRenderDescriptorHeapAllocator()
{
	CheckIsRenderThread();
	DescriptorCaches.emplace_back(new FXRayDescriptorCache);
#ifdef DEBUG
	DebugState.resize(2048);
#endif
}

TiramisuRenderDescriptorHeapAllocator::~TiramisuRenderDescriptorHeapAllocator()
{
	CheckIsRenderThread();
	FlushNextFrame_RenderThread();
	while (!DescriptorCaches.empty())
	{
		delete DescriptorCaches.back();
		DescriptorCaches.pop_back();
	}
	VERIFY(FreeIndexes.size() == NextIndex);
}

u32 TiramisuRenderDescriptorHeapAllocator::Alloc(nri::Descriptor* InDescriptor)
{
	CheckIsRenderThread();
	u32 Index = 0;
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

	nri::UpdateDescriptorRangeDesc& UpdateDescriptorRangeDescription = UpdateDescriptorRangesDescriptions.emplace_back();
	UpdateDescriptorRangeDescription.descriptorSet = GRenderResourcesManager->ResourcesDescriptorSet;
	UpdateDescriptorRangeDescription.rangeIndex = 0;
	UpdateDescriptorRangeDescription.baseDescriptor = Index;
	InDescriptorPool->Descriptors[InDescriptorPool->Index] = InDescriptor;
#ifdef DEBUG
	DebugState[Index] = InDescriptor;
#endif
	UpdateDescriptorRangeDescription.descriptors = &InDescriptorPool->Descriptors[InDescriptorPool->Index++];
	UpdateDescriptorRangeDescription.descriptorNum = 1;

	if (InDescriptorPool->Index == 256)
	{
		DescriptorCaches.emplace_back(new FXRayDescriptorCache);
	}
	return Index;
}

void TiramisuRenderDescriptorHeapAllocator::Free(u32 Index)
{
	CheckIsRenderThread();
#ifdef DEBUG
	DebugState[Index] = nullptr;
#endif
	FreeIndexesForNextFrame.push_back(Index);
}

void TiramisuRenderDescriptorHeapAllocator::FlushNextFrame_RenderThread()
{
	CheckIsRenderThread();
	FreeIndexes.append_range(FreeIndexesForNextFrame);
	FreeIndexesForNextFrame.clear();
}

void TiramisuRenderDescriptorHeapAllocator::UpdateDescriptorRanges()
{
	CheckIsRenderThread();
	GRenderDevice.CoreInterface.UpdateDescriptorRanges(UpdateDescriptorRangesDescriptions.data(), UpdateDescriptorRangesDescriptions.size());
	UpdateDescriptorRangesDescriptions.clear();

	while (DescriptorCaches.size() > 1)
	{
		delete DescriptorCaches.back();
		DescriptorCaches.pop_back();
	}
	DescriptorCaches[0]->Index = 0;
}

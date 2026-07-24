#pragma once

#include "TiramisuRenderTypes.h"

// Кэш descriptor set и его bindless-индекса в глобальном heap.
struct FXRayDescriptorCache
{
	nri::Descriptor* Descriptors[256] = {};
	u32 Index = 0;
};

// Выделяет стабильные bindless descriptor indices для shader indexing.
class TiramisuRenderDescriptorHeapAllocator
{
public:
	TiramisuRenderDescriptorHeapAllocator();
	~TiramisuRenderDescriptorHeapAllocator();
	u32 Alloc(nri::Descriptor* InDescriptor);
	void Free(u32 Index);

	void FlushNextFrame_RenderThread();
	void UpdateDescriptorRanges();

private:
	xr_vector<FXRayDescriptorCache*> DescriptorCaches;

	xr_vector<nri::UpdateDescriptorRangeDesc> UpdateDescriptorRangesDescriptions;
	xr_vector<u32> FreeIndexes;
	xr_vector<u32> FreeIndexesForNextFrame;
	u32 NextIndex = 0;
#ifdef DEBUG
	xr_vector<nri::Descriptor*> DebugState;
#endif
};

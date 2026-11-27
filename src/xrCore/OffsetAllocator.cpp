#include "stdafx.h"
#include "OffsetAllocator.h"

void COffsetAllocator::Init(u32 capacityBytes)
{
	FreeBlocks.clear();
	LiveBlocks.clear();
	CapacityBytes = capacityBytes;
	UsedBytes = 0;
	if (CapacityBytes != 0)
		FreeBlocks[0] = CapacityBytes;
}

void COffsetAllocator::Reset()
{
	const u32 capacity = CapacityBytes;
	Init(capacity);
}

u32 COffsetAllocator::Alloc(u32 sizeBytes, u32 alignment)
{
	if (sizeBytes == 0 || CapacityBytes == 0)
		return InvalidOffset;

	if (alignment == 0)
		alignment = 1;

	// First-fit over the address-ordered free blocks.
	for (auto it = FreeBlocks.begin(); it != FreeBlocks.end(); ++it)
	{
		const u32 blockOffset = it->first;
		const u32 blockSize = it->second;

		const u32 alignedOffset = (blockOffset + alignment - 1) & ~(alignment - 1);
		const u32 headPad = alignedOffset - blockOffset;
		if (headPad + sizeBytes > blockSize)
			continue;

		FreeBlocks.erase(it);

		// Leftover head padding stays free.
		if (headPad != 0)
			FreeBlocks[blockOffset] = headPad;

		// Leftover tail stays free.
		const u32 tailOffset = alignedOffset + sizeBytes;
		const u32 tailSize = blockSize - headPad - sizeBytes;
		if (tailSize != 0)
			FreeBlocks[tailOffset] = tailSize;

		LiveBlocks[alignedOffset] = sizeBytes;
		UsedBytes += sizeBytes;
		return alignedOffset;
	}

	return InvalidOffset;
}

void COffsetAllocator::Free(u32 offset)
{
	if (offset == InvalidOffset)
		return;

	auto live = LiveBlocks.find(offset);
	if (live == LiveBlocks.end())
	{
		Msg("! COffsetAllocator::Free: unknown offset %u (double free or foreign offset)", offset);
		return;
	}

	u32 freeOffset = offset;
	u32 freeSize = live->second;
	LiveBlocks.erase(live);
	UsedBytes -= freeSize;

	// Coalesce with the previous block if adjacent.
	auto next = FreeBlocks.lower_bound(freeOffset);
	if (next != FreeBlocks.begin())
	{
		auto prev = std::prev(next);
		if (prev->first + prev->second == freeOffset)
		{
			freeOffset = prev->first;
			freeSize += prev->second;
			FreeBlocks.erase(prev);
		}
	}

	// Coalesce with the next block if adjacent. (Re-find after a possible erase.)
	next = FreeBlocks.lower_bound(freeOffset);
	if (next != FreeBlocks.end() && freeOffset + freeSize == next->first)
	{
		freeSize += next->second;
		FreeBlocks.erase(next);
	}

	FreeBlocks[freeOffset] = freeSize;
}

bool COffsetAllocator::Owns(u32 offset) const
{
	return LiveBlocks.find(offset) != LiveBlocks.end();
}

COffsetAllocator::SStats COffsetAllocator::GetStats() const
{
	SStats stats = {};
	stats.Capacity = CapacityBytes;
	stats.Used = UsedBytes;
	stats.Free = CapacityBytes - UsedBytes;
	stats.FreeBlockCount = u32(FreeBlocks.size());
	stats.AllocationCount = u32(LiveBlocks.size());
	stats.LargestFreeBlock = 0;
	for (const auto& block : FreeBlocks)
		stats.LargestFreeBlock = std::max(stats.LargestFreeBlock, block.second);
	return stats;
}

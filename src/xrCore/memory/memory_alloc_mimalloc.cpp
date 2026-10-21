#include "stdafx.h"

#ifdef IXRAY_USE_MIMALLOC
#include <mimalloc.h>
#include <mimalloc-stats.h>
#include "memory_alloc_mimalloc.h"

void* CMemAllocMimalloc::alloc(size_t Size)
{
	return mi_malloc(Size);
}

void* CMemAllocMimalloc::realloc(void* Pointer, size_t Size)
{
	if (Pointer && Size == 0)
	{
		mi_free(Pointer);
		return nullptr;
	}
	return mi_realloc(Pointer, Size);
}

void CMemAllocMimalloc::free(void* Pointer)
{
	mi_free(Pointer);
}

CMemAllocMimalloc* CMemAllocMimalloc::Create()
{
	static constinit CMemAllocMimalloc Allocator;
	return &Allocator;
}

void CMemAllocMimalloc::Compact()
{
	mi_collect(true);
}

u32 CMemAllocMimalloc::Usage(u32* BlocksUsed, u32* BlocksFree)
{
	mi_stats_t Stats;
	mi_stats_init(&Stats);
	const bool Valid = mi_stats_get(&Stats);
	s64 Used = 0;
	if (Valid)
	{
		for (const auto& Bin : Stats.malloc_bins)
		{
			Used += Bin.current;
		}
	}
	if (BlocksUsed)
	{
		*BlocksUsed = static_cast<u32>(std::clamp<s64>(Used, 0, UINT32_MAX));
	}
	if (BlocksFree)
	{
		*BlocksFree = 0;
	}
	const s64 Bytes = Valid ? Stats.malloc_normal.current + Stats.malloc_huge.current : 0;
	return static_cast<u32>(std::clamp<s64>(Bytes, 0, UINT32_MAX));
}
#endif

#include "stdafx.h"
#ifdef IXRAY_USE_MIMALLOC
#include "memory_alloc_mimalloc.h"
#else
#include "memory_alloc_pure.h"
#endif

namespace MemoryAllocators
{
static IMemoryAllocator& Get()
{
#ifdef IXRAY_USE_MIMALLOC
	return *CMemAllocMimalloc::Create();
#else
	return *CMemAllocPure::Create();
#endif
}
}

void* xrMemory::mem_alloc(size_t size)
{
	std::atomic_ref(stat_calls).fetch_add(1, std::memory_order_relaxed);
	auto ptr = MemoryAllocators::Get().alloc(size);
	PROF_MEM_ALLOC_CAPTURE(ptr, size);
	return ptr;
}

void xrMemory::mem_free(void* P)
{
	std::atomic_ref(stat_calls).fetch_add(1, std::memory_order_relaxed);
	PROF_MEM_FREE_CAPTURE(P);
	MemoryAllocators::Get().free(P);
}

void* xrMemory::mem_realloc(void* P, size_t size)
{
	std::atomic_ref(stat_calls).fetch_add(1, std::memory_order_relaxed);
	PROF_MEM_FREE_CAPTURE(P);
	auto ptr = MemoryAllocators::Get().realloc(P, size);
	PROF_MEM_ALLOC_CAPTURE(ptr, size);
	return ptr;
}

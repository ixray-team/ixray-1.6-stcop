#include "stdafx.h"
#include "xrMemory_align.h"

void* xrMemory::mem_alloc(size_t size)
{
	stat_calls++;
	if (pAlloc == nullptr)
	{
		auto ptr = malloc(size);
		PROF_MEM_ALLOC_CAPTURE(ptr, size);
    	return ptr;
	}
	auto ptr = pAlloc->alloc(size);
	PROF_MEM_ALLOC_CAPTURE(ptr, size);
	return ptr;
}

void xrMemory::mem_free(void* P)
{
	stat_calls++;
	PROF_MEM_FREE_CAPTURE(P);
	return pAlloc->free(P);
}

void* xrMemory::mem_realloc(void* P, size_t size)
{
	stat_calls++;
	PROF_MEM_FREE_CAPTURE(P);
	auto ptr = pAlloc->realloc(P, size);
	PROF_MEM_ALLOC_CAPTURE(ptr, size);
	return ptr;
}

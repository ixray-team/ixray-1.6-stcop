#include "stdafx.h"
#include "xrMemory_align.h"

void* xrMemory::mem_alloc(size_t size)
{
	stat_calls++;
	return pAlloc->alloc(size);
}

void xrMemory::mem_free(void* P)
{
	stat_calls++;
	return pAlloc->free(P);
}

void* xrMemory::mem_realloc(void* P, size_t size)
{
	stat_calls++;
	return pAlloc->realloc(P, size);
}

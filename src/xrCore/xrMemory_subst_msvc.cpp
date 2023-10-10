#include "stdafx.h"
#pragma hdrstop

#include "xrMemory_align.h"
#include "xrMemory_pure.h"

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

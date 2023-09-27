#include "stdafx.h"
#include "memory_alloc_pure.h"

CMemAllocPure gMemPure;

void* CMemAllocPure::alloc(size_t size)
{
	return malloc(size);
}

void* CMemAllocPure::realloc(void* p, size_t size)
{
	void* result = ::realloc(p, size);
	return (result);
}

void CMemAllocPure::free(void* p)
{
	::free(p);
}

#include <new>
CMemAllocPure* CMemAllocPure::Create()
{
	return new CMemAllocPure;
}

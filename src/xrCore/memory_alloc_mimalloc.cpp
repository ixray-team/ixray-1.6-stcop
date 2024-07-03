#include "stdafx.h"

#include <mimalloc.h>
#include "memory_alloc_mimalloc.h"

void* CMemAllocMimalloc::alloc(size_t size)
{
	return mi_malloc(size);
}

void* CMemAllocMimalloc::realloc(void* p, size_t size)
{
	void* result = ::mi_realloc(p, size);
	return (result);
}

void CMemAllocMimalloc::free(void* p)
{
	::mi_free(p);
}

CMemAllocMimalloc* CMemAllocMimalloc::Create()
{
	static CMemAllocMimalloc gMemPure;
	return &gMemPure;
}

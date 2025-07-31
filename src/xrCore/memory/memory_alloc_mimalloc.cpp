#include "stdafx.h"

#if 0
#include <mimalloc.h>
#include "memory_alloc_mimalloc.h"

void* CMemAllocMimalloc::alloc(size_t size)
{
	return 0;// mi_malloc(size);
}

void* CMemAllocMimalloc::realloc(void* p, size_t size)
{
	void* result = 0;// ::mi_realloc(p, size);
	return (result);
}

void CMemAllocMimalloc::free(void* p)
{
	//::mi_free(p);
}

CMemAllocMimalloc* CMemAllocMimalloc::Create()
{
	static CMemAllocMimalloc gMemPure;
	return &gMemPure;
}
#endif
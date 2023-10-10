#pragma once
#include "xrMEMORY_POOL.h"

class CMemAllocXRay :
	public IMemoryAllocator
{
	MEMPOOL mem_pools[mem_pools_count];

public:
	CMemAllocXRay();

	void* alloc(size_t size) override;
	void* realloc(void* p, size_t size) override;
	void free(void* p) override;

	static CMemAllocXRay* Create();
};
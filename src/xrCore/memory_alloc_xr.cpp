#include "stdafx.h"

#include "memory_alloc_pure.h"
#include "memory_alloc_xr.h"
#include "xrMemory_align.h"

ICF u8* acc_header(void* P) {
	u8* _P = (u8*)P;
	return	_P - 1; 
}

ICF u32 get_header(void* P) {
	return (u32)*acc_header(P); 
}

ICF u32 get_pool(size_t size) {
	u32 pid = u32(size / mem_pools_ebase);
	if (pid >= mem_pools_count) {
		return mem_generic;
	} else {
		return pid;
	}
}

CMemAllocXRay::CMemAllocXRay() {
	// initialize POOLs
	u32	element = mem_pools_ebase;
	u32 sector = mem_pools_ebase * 1024;
	for (u32 pid = 0; pid < mem_pools_count; pid++) {
		mem_pools[pid]._initialize(element, sector, 0x1);
		element += mem_pools_ebase;
	}
}

void* CMemAllocXRay::alloc(size_t size) {
	u32	_footer = 0;
	void* _ptr = 0;

	if (!mem_initialized) {
		// generic
		// Igor: Reserve 1 byte for xrMemory header
		void* _real = xr_aligned_offset_malloc(1 + size + _footer, 16, 0x1);
		_ptr = (void*) (((u8*) _real) + 1);
		*acc_header(_ptr) = mem_generic;
	} else {
		// accelerated
		// Igor: Reserve 1 byte for xrMemory header
		u32	pool = get_pool(1 + size + _footer);
		if (mem_generic == pool) {
			// generic
			//	Igor: Reserve 1 byte for xrMemory header
			void* _real = xr_aligned_offset_malloc(1 + size + _footer, 16, 0x1);
			_ptr = (void*)(((u8*)_real) + 1);
			*acc_header(_ptr) = mem_generic;
		} else {
			// pooled
			//	Igor: Reserve 1 byte for xrMemory header
			//	Already reserved when getting pool id
			void* _real = mem_pools[pool].create();
			_ptr = (void*) (((u8*) _real) + 1);
			*acc_header(_ptr) = (u8) pool;
		}
	}

	return	_ptr;
}

void* CMemAllocXRay::realloc(void* p, size_t size) {
	if (p == nullptr) {
		return alloc(size);
	}

	u32		p_current = get_header(p);
	//	Igor: Reserve 1 byte for xrMemory header
	u32		p_new = get_pool(1 + size);
	u32		p_mode = 1;

	if (mem_generic == p_current) {
		if (p_new < p_current)		p_mode = 2;
		else						p_mode = 0;
	}

	void* _real = (void*) (((u8*) p) - 1);
	void* _ptr = nullptr;
	if (0 == p_mode) {
		u32 _footer = 0;
		//	Igor: Reserve 1 byte for xrMemory header
		void* _real2 = xr_aligned_offset_realloc(_real, 1 + size + _footer, 16, 0x1);
		_ptr = (void*)(((u8*)_real2) + 1);
		*acc_header(_ptr) = mem_generic;
	} else if (1 == p_mode) {
		// pooled realloc
		R_ASSERT2(p_current < mem_pools_count, "Memory corruption");
		u32 s_current = mem_pools[p_current].get_element();
		u32 s_dest = (u32)size;
		void* p_old = p;

		void* p_new_ = alloc(size);

		//	Igor: Reserve 1 byte for xrMemory header
		//	Don't bother in this case?
		Memory.mem_copy(p_new_, p_old, _min(s_current - 1, s_dest));
		free(p_old);
		_ptr = p_new_;
	} else if (2 == p_mode) {
		// relocate into another mmgr(pooled) from real
		void* p_old = p;
		void* p_new_ = alloc(size);
		Memory.mem_copy(p_new_, p_old, (u32)size);
		free(p_old);
		_ptr = p_new_;
	}

	return	_ptr;
}

void CMemAllocXRay::free(void* p) {
	u32	pool = get_header(p);
	void* _real = (void*)(((u8*)p) - 1);
	if (mem_generic == pool) {
		// generic
		xr_aligned_free(_real);
	} else {
		// pooled
		VERIFY2(pool < mem_pools_count, "Memory corruption");
		mem_pools[pool].destroy(_real);
	}
}

#include <new>

CMemAllocXRay* CMemAllocXRay::Create() {
	static CMemAllocXRay Alloc;
	return &Alloc;
}

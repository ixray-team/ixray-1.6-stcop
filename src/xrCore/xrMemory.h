#pragma once
#include "xrMemory_pso.h"

class IMemoryAllocator
{
public:
	virtual void* alloc(size_t size) = 0;
	virtual void* realloc(void* p, size_t size) = 0;
	virtual void free(void* p) = 0;
};

class XRCORE_API xrMemory {
	IMemoryAllocator* pAlloc = nullptr;
public:
	xrMemory			();
	void				_initialize		(BOOL _debug_mode=FALSE);
	void				_destroy		();

	u32					stat_calls = 0;
	s32					stat_counter = 0;
public:

	u32					mem_usage		(u32* pBlocksUsed=NULL, u32* pBlocksFree=NULL);
	void				mem_compact		();
	void				mem_counter_set	(u32 _val)	{ stat_counter = _val;	}
	u32					mem_counter_get	()			{ return stat_counter;	}

	void*				mem_alloc		(size_t	size				);
	void*				mem_realloc		(void*	p, size_t size		);

	void				mem_free		(void*	p					);

	pso_MemCopy*		mem_copy;
	pso_MemFill*		mem_fill;
	pso_MemFill32*		mem_fill32;
};

extern XRCORE_API xrMemory Memory;

// delete
#ifdef __BORLANDC__
	#include "xrMemory_subst_borland.h"
#else
	#include "xrMemory_subst_msvc.h"
#endif

// generic "C"-like allocations/deallocations
template <class T>
IC T*		xr_alloc	(u32 count)				{	return  (T*)Memory.mem_alloc(count*sizeof(T));	}
template <class T>
IC void		xr_free		(T* &P)					{	if (P) { Memory.mem_free((void*)P); P=NULL;	};	}
IC void*	xr_malloc	(size_t size)			{	return	Memory.mem_alloc(size);					}
IC void*	xr_realloc	(void* P, size_t size)	{	return Memory.mem_realloc(P,size);				}

XRCORE_API	char* 	xr_strdup	(const char* string);

#	if !(defined(__BORLANDC__) || defined(NO_XRNEW))
	IC void*	operator new		(size_t size)		{	return Memory.mem_alloc(size?size:1);				}
	IC void		operator delete		(void *p)			{	xr_free(p);											}
	IC void*	operator new[]		(size_t size)		{	return Memory.mem_alloc(size?size:1);				}
	IC void		operator delete[]	(void* p)			{	xr_free(p);											}
#	endif


// POOL-ing
const		u32			mem_pools_count			=	54;
const		u32			mem_pools_ebase			=	16;
const		u32			mem_generic				=	mem_pools_count+1;
extern		BOOL		mem_initialized;

XRCORE_API void vminfo			(size_t *_free, size_t *reserved, size_t *committed);
XRCORE_API void log_vminfo		();
XRCORE_API u32	mem_usage_impl	(u32* pBlocksUsed, u32* pBlocksFree);

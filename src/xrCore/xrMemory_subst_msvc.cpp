#include "stdafx.h"
#pragma hdrstop

#include "xrMemory_align.h"
#include "xrMemory_pure.h"

#ifndef	__BORLANDC__
MEMPOOL		mem_pools			[mem_pools_count];

// MSVC
ICF	u8*		acc_header			(void* P)	{	u8*		_P		= (u8*)P;	return	_P-1;	}
ICF	u32		get_header			(void* P)	{	return	(u32)*acc_header(P);				}
ICF	u32		get_pool			(size_t size)
{
	u32		pid					= u32(size/mem_pools_ebase);
	if (pid>=mem_pools_count)	return mem_generic;
	else						return pid;
}

#ifdef PURE_ALLOC
bool	g_use_pure_alloc		= false;
#endif // PURE_ALLOC

void*	xrMemory::mem_alloc		(size_t size)
{
	stat_calls++;

#ifdef PURE_ALLOC
	static bool g_use_pure_alloc_initialized = false;
	if (!g_use_pure_alloc_initialized) {
		g_use_pure_alloc_initialized	= true;
		g_use_pure_alloc				= 
#	ifdef XRCORE_STATIC
			true
#	else // XRCORE_STATIC
			!!strstr(GetCommandLine(),"-pure_alloc")
#	endif // XRCORE_STATIC
			;
	}

	if (g_use_pure_alloc) {
		void							*result = malloc(size);
		return							(result);
	}
#endif // PURE_ALLOC

	u32		_footer				=	0;
	void*	_ptr				=	0;

	//
	if (!mem_initialized /*|| debug_mode*/)		
	{
		// generic
		//	Igor: Reserve 1 byte for xrMemory header
		void*	_real			=	xr_aligned_offset_malloc	(1 + size + _footer, 16, 0x1);
		//void*	_real			=	xr_aligned_offset_malloc	(size + _footer, 16, 0x1);
		_ptr					=	(void*)(((u8*)_real)+1);
		*acc_header(_ptr)		=	mem_generic;
	} else {
		//	accelerated
		//	Igor: Reserve 1 byte for xrMemory header
		u32	pool				=	get_pool	(1+size+_footer);
		//u32	pool				=	get_pool	(size+_footer);
		if (mem_generic==pool)	
		{
			// generic
			//	Igor: Reserve 1 byte for xrMemory header
			void*	_real		=	xr_aligned_offset_malloc	(1 + size + _footer,16,0x1);
			//void*	_real		=	xr_aligned_offset_malloc	(size + _footer,16,0x1);
			_ptr				=	(void*)(((u8*)_real)+1);
			*acc_header(_ptr)	=	mem_generic;
		} else {
			// pooled
			//	Igor: Reserve 1 byte for xrMemory header
			//	Already reserved when getting pool id
			void*	_real		=	mem_pools[pool].create();
			_ptr				=	(void*)(((u8*)_real)+1);
			*acc_header(_ptr)	=	(u8)pool;
		}
	}

	return	_ptr;
}

void	xrMemory::mem_free		(void* P)
{
	stat_calls++;

#ifdef PURE_ALLOC
	if (g_use_pure_alloc) {
		free					(P);
		return;
	}
#endif // PURE_ALLOC

	u32	pool					= get_header	(P);
	void* _real					= (void*)(((u8*)P)-1);
	if (mem_generic==pool)		
	{
		// generic
		xr_aligned_free			(_real);
	} else {
		// pooled
		VERIFY2					(pool<mem_pools_count,"Memory corruption");
		mem_pools[pool].destroy	(_real);
	}
}

extern BOOL	g_bDbgFillMemory	;

void*	xrMemory::mem_realloc	(void* P, size_t size)
{
	stat_calls++;
#ifdef PURE_ALLOC
	if (g_use_pure_alloc) {
		void							*result = realloc(P,size);
		return							(result);
	}
#endif // PURE_ALLOC
	if (0==P) {
		return mem_alloc	(size);
	}

	u32		p_current			= get_header(P);
	//	Igor: Reserve 1 byte for xrMemory header
	u32		p_new				= get_pool	(1+size+(0));
	u32		p_mode				;

	if (mem_generic==p_current)	{
		if (p_new<p_current)		p_mode	= 2	;
		else						p_mode	= 0	;
	} else 							p_mode	= 1	;

	void*	_real				= (void*)(((u8*)P)-1);
	void*	_ptr				= NULL;
	if		(0==p_mode)
	{
		u32		_footer			=	0;
		//	Igor: Reserve 1 byte for xrMemory header
		void*	_real2			=	xr_aligned_offset_realloc	(_real,1+size+_footer,16,0x1);
		_ptr					= (void*)(((u8*)_real2)+1);
		*acc_header(_ptr)		= mem_generic;
	} 
	else if (1==p_mode)		
	{
		// pooled realloc
		R_ASSERT2				(p_current<mem_pools_count,"Memory corruption");
		u32		s_current		= mem_pools[p_current].get_element();
		u32		s_dest			= (u32)size;
		void*	p_old			= P;

		void*	p_new_			= mem_alloc(size);
		//	Igor: Reserve 1 byte for xrMemory header
		//	Don't bother in this case?
		mem_copy				(p_new_,p_old,_min(s_current-1,s_dest));
		//mem_copy				(p_new_,p_old,_min(s_current,s_dest));
		mem_free				(p_old);
		_ptr					= p_new_;
	} else if (2==p_mode)		{
		// relocate into another mmgr(pooled) from real
		void*	p_old			= P;
		void*	p_new_			= mem_alloc(size);
		mem_copy				(p_new_,p_old,(u32)size);
		mem_free				(p_old);
		_ptr					= p_new_;
	}

	return	_ptr;
}

#endif // __BORLANDC__
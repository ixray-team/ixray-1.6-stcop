////////////////////////////////////////////////////////////////////////////
//	Created		: 14.08.2009
//	Author		: Armen Abroyan
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "doug_lea_allocator.h"

#define USE_DL_PREFIX

#include "DougLea/dlmalloc.h"

doug_lea_allocator::doug_lea_allocator(LPCSTR arena_id)
{
}

void* doug_lea_allocator::malloc_impl(size_t size)
{
	return dlmalloc(size);
}

void* doug_lea_allocator::realloc_impl(void* pointer, size_t new_size)
{
	return dlrealloc(pointer, new_size);
}

void doug_lea_allocator::free_impl(void*& pointer)
{
	dlfree(pointer);
	pointer = 0;
}
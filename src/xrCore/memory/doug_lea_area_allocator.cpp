#include "stdafx.h"
#include "doug_lea_area_allocator.h"
#include "doug_lea_memory_allocator.h"

doug_lea_area_allocator::doug_lea_area_allocator(void* area, const char* arena_id, size_t alloc_size)
{
	area_space = create_mspace_with_base(area, alloc_size, 0);
}

doug_lea_area_allocator::~doug_lea_area_allocator()
{
	destroy_mspace(area_space);
}

void* doug_lea_area_allocator::malloc_impl(size_t size)
{
	return mspace_malloc(area_space, size);
}

void* doug_lea_area_allocator::realloc_impl(void* pointer, size_t new_size)
{
	return mspace_realloc(area_space, pointer, new_size);
}

void doug_lea_area_allocator::free_impl(void*& pointer)
{
	mspace_free(area_space, pointer);
	pointer = nullptr;
}
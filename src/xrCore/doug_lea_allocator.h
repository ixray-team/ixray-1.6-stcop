////////////////////////////////////////////////////////////////////////////
//	Created		: 14.08.2009
//	Author		: Armen Abroyan
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////
#pragma once

class XRCORE_API doug_lea_allocator
{
public:
	doug_lea_allocator(LPCSTR arena_id);
	~doug_lea_allocator() = default;

	void* malloc_impl(size_t size);
	void* realloc_impl(void* pointer, size_t new_size);

	void free_impl(void*& pointer);

	template <typename T>
	inline void free_impl(T*& pointer)
	{
		free_impl(reinterpret_cast<void*&>(pointer));
	}

	template <typename T>
	inline T* alloc_impl(size_t const count)
	{
		return (T*)malloc_impl(count * sizeof(T));
	}
};
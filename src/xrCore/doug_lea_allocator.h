////////////////////////////////////////////////////////////////////////////
//	Created		: 14.08.2009
//	Author		: Armen Abroyan
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////
#pragma once

class XRCORE_API doug_lea_allocator
{
public:
	doug_lea_allocator(void* arena, size_t arena_size, LPCSTR arena_id);
	~doug_lea_allocator();

	void* malloc_impl(size_t size);
	void* realloc_impl(void* pointer, size_t new_size);

	void free_impl(void*& pointer);

	size_t get_allocated_size() const;
	inline	LPCSTR	get_arena_id() const
	{
		return m_arena_id; 
	}

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

private:
	LPCSTR m_arena_id;
	void* m_dl_arena;
};
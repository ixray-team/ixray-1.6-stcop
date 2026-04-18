#pragma once

class XRCORE_API doug_lea_area_allocator
{
	void* area_space = nullptr;
public:
	doug_lea_area_allocator(void* area, const char* arena_id, size_t alloc_size);
	~doug_lea_area_allocator();

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
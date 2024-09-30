#pragma once

template <bool _is_pm, typename T>
struct xr_special_free
{
	IC void operator()(T*& ptr)
	{
		if constexpr (_is_pm)
		{
			void* _real_ptr = dynamic_cast<void*>(ptr);
			ptr->~T();
			Memory.mem_free(_real_ptr);
		}
		else
		{
			ptr->~T();
			Memory.mem_free(ptr);
		}
	}
};

template <class T>
IC void xr_delete(T*& ptr)
{
	if (ptr)
	{
		xr_special_free<std::is_polymorphic<T>::value, T>()(ptr);
		ptr = nullptr;
	}
}
template <class T>
IC void xr_delete(T* const& ptr)
{
	if (ptr)
	{
		xr_special_free<std::is_polymorphic<T>::value, T>()(const_cast<T*&>(ptr));
		const_cast<T*&>(ptr) = nullptr;
	}
}

#include <memory>

template<typename T>
using xr_shared_ptr = std::shared_ptr<T>;

template<typename T>
using xr_unique_ptr = std::unique_ptr<T, xr_special_free<false, T>>;

template <class T, class... Args>
xr_shared_ptr<T> xr_make_shared(Args&&... args)
{
	return xr_shared_ptr<T>(new T(std::forward<Args>(args)...), [](T* ptr)
	       {
	       		xr_special_free<false, T> deleter;
	       		deleter(ptr);
	       });
}

template <typename T, typename... ARGS>
xr_unique_ptr<T> xr_make_unique(ARGS&&... args)
{
	void* TypeMem = Memory.mem_alloc(sizeof(T));
	new (TypeMem)T(std::forward<ARGS>(args)...);
	return xr_unique_ptr<T>(reinterpret_cast<T*>(TypeMem), xr_special_free<false, T>{});
}

#pragma once

template <bool _is_pm, typename T>
struct xr_special_free
{
	IC void operator()(T*& ptr)
	{
		if (ptr == nullptr)
		{
			return;
		}

		if constexpr (_is_pm)
		{
			void* _real_ptr = nullptr;

			if constexpr (std::is_polymorphic_v<T> && !std::is_final_v<T>)
			{
				// FX: ѕроблемы множественного наследовани€
				_real_ptr = dynamic_cast<void*>(ptr);
			}
			else
			{
				_real_ptr = static_cast<void*>(ptr);
			}

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

template <bool _is_pm, typename T>
struct xr_special_free<_is_pm, T[]>
{
	IC void operator()(T* ptr)
	{
		if (ptr == nullptr)
			return;

		Memory.mem_free(ptr);
	}
};

template <class T>
IC void xr_delete(T*& ptr)
{
	static_assert(!std::is_polymorphic_v<T> || std::has_virtual_destructor_v<T>, "Polymorphic delete requires virtual destructor or RTTI!");
	if (ptr)
	{
		xr_special_free<std::is_polymorphic_v<T>, T>()(ptr);
		ptr = nullptr;
	}
}
template <class T>
IC void xr_delete(T* const& ptr)
{
	static_assert(!std::is_polymorphic_v<T> || std::has_virtual_destructor_v<T>, "Polymorphic delete requires virtual destructor or RTTI!");
	if (ptr)
	{
		xr_special_free<std::is_polymorphic_v<T>, T>()(const_cast<T*&>(ptr));
		const_cast<T*&>(ptr) = nullptr;
	}
}

#include <memory>

template<typename T>
using xr_weak_ptr = std::weak_ptr<T>;

template<typename T>
using xr_shared_ptr = std::shared_ptr<T>;

template<typename T>
using xr_unique_ptr = std::unique_ptr<T, xr_special_free<std::is_polymorphic_v<T>, T>>;

template <class T, class... Args>
xr_shared_ptr<T> xr_make_shared(Args&&... args)
{
	return xr_shared_ptr<T>(new T(std::forward<Args>(args)...), [](T* ptr)
	       {
	       		xr_special_free<std::is_polymorphic_v<T>, T> deleter;
	       		deleter(ptr);
	       });
}

template <typename T, typename... ARGS>
xr_unique_ptr<T> xr_make_unique(ARGS&&... args)
{
	void* TypeMem = Memory.mem_alloc(sizeof(T));
	new (TypeMem)T(std::forward<ARGS>(args)...);
	return xr_unique_ptr<T>(reinterpret_cast<T*>(TypeMem), xr_special_free<std::is_polymorphic_v<T>, T>{});
}

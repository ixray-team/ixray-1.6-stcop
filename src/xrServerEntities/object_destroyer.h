#pragma once
#include "object_interfaces.h"

template <typename T>
concept HasContainerOps = requires(T a)
{
	a.begin();
	a.end();
	a.clear();
};

template <typename T>
inline void delete_data(T& data)
{
	if constexpr (std::is_base_of_v<IPureDestroyableObject, std::remove_pointer_t<T>>)
	{
		if constexpr (std::is_pointer_v<T>)
		{
			data->destroy();
		}
		else
		{
			data.destroy();
		}
	}
	else if constexpr (std::is_pointer_v<T>)
	{
		if constexpr (std::is_same_v<std::remove_pointer_t<T>, char>)
		{
			if (data != nullptr)
			{
				xr_free(data);
				data = nullptr;
			}
		}
		else
		{
			xr_delete(data);
		}
	}
	else if constexpr (HasContainerOps<T>)
	{
		for (auto& Item : data)
		{
			if constexpr (std::is_pointer_v<std::remove_reference_t<decltype(Item)>>)
			{
				xr_delete(Item);
			}
			else
			{
				delete_data(Item);
			}
		}
		data.clear();
	}
	else if constexpr (std::is_same_v<T, std::remove_cvref_t<T>> && requires { typename T::first_type; typename T::second_type; })
	{
		auto& [First, Second] = data;

		if constexpr (std::is_pointer_v<std::remove_reference_t<decltype(First)>>)
		{
			xr_delete(First);
		}
		else
		{
			delete_data(First);
		}

		if constexpr (std::is_pointer_v<std::remove_reference_t<decltype(Second)>>)
		{
			xr_delete(Second);
		}
		else
		{
			delete_data(Second);
		}
	}
	else if constexpr (std::is_same_v<T, char*>)
	{
		xr_free(data);
	}
	else
	{
	}
}


template <typename T, size_t N>
inline void delete_data(T(&Array)[N])
{
	for (size_t i = 0; i < N; ++i)
	{
		if constexpr (std::is_pointer_v<T>)
		{
			if (Array[i] != nullptr)
			{
				delete_data(Array[i]);
				Array[i] = nullptr;
			}
		}
		else
		{
			delete_data(Array[i]);
		}
	}
}
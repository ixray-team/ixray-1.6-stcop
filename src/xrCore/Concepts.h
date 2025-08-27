#pragma once

#include <type_traits>

namespace XRay::Concepts
{
	template <typename T>
	concept CharPtr = std::is_pointer_v<T> && std::same_as<std::remove_pointer_t<T>, char>;

	template <typename T>
	concept CharArray = std::is_array_v<T> && std::same_as<std::remove_extent_t<T>, char>;

	template <typename T>
	concept XRayString = std::same_as<std::remove_cvref_t<T>, xr_string> || std::same_as<std::remove_cvref_t<T>, shared_str>;

	template <typename T>
	concept Arithmetic = std::integral<T> || std::floating_point<T>;

	template <typename T>
	concept FixedType = std::is_arithmetic_v<T> || std::is_enum_v<T>;
}
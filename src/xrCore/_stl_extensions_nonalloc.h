#pragma once
// XRay typedefs, which don't use xalloc
#include <span>
#include <array>

template <class T, size_t Ext = std::dynamic_extent>
using xr_span = std::span<T, Ext>;

template <typename K, typename V>
using xr_pair = std::pair<K, V>;

template<typename... Args>
using xr_tuple = std::tuple<Args...>;

using xr_string_view = std::string_view;
using xr_wstring_view = std::wstring_view;

template<typename Type, size_t Size>
using xr_array = std::array<Type, Size>;
/***************************************************************************************
* Copyright (C) Anton Kovalev (vertver), 2023. All rights reserved.
* GRR - "Games Require Reflection", library for integrating reflection into games
* MIT License
***************************************************************************************/
#ifndef GRR_TYPES_HPP_INCLUDED
#define GRR_TYPES_HPP_INCLUDED
#include <system_error>
#include <cstdint>
#include <utility>
#include <array>
#include <algorithm>

namespace grr
{
    struct type_context;
    class context;
    class type_declaration;

    template<typename T>
    using allocator = std::allocator<T>;

    template<typename T>
    using optional = GRR_OPTIONAL<T>;

    template<typename T>
    using vector = GRR_VECTOR<T>;

    template<typename K, typename V>
    using hash_map = GRR_HASH_MAP<K, V>;

    template<typename K>
    using hash_set = GRR_HASH_SET<K>;

#ifdef _MSC_VER
    using wstring = GRR_WIDESTRING;
    using wstring_view = GRR_WIDESTRING_VIEW;
#endif

    using string = GRR_STRING;
    using string_view = GRR_STRING_VIEW;
    using type_id = GRR_TYPE_ID;

    using ptr_pair = std::pair<std::uint64_t, std::pair<void*, type_id>>;
    using const_ptr_pair = std::pair<std::uint64_t, std::pair<const void*, type_id>>;
}

#ifndef GRR_MSVC_TYPES
#ifdef _MSC_VER
#define GRR_MSVC_TYPES \
    wchar_t, \
    __int32, \
    unsigned __int32, \
    grr::wstring, \
    grr::wstring_view, 
#else
#define GRR_MSVC_TYPES 
#endif
#endif

#ifndef GRR_CLANG_TYPES
#ifdef __clang__
#define GRR_CLANG_TYPES 
#else
#define GRR_CLANG_TYPES 
#endif
#endif

#ifndef GRR_GCC_TYPES
#ifdef __GNUC__
#define GRR_GCC_TYPES 
#else
#define GRR_GCC_TYPES 
#endif
#endif

#ifndef GRR_TYPES
#define GRR_TYPES \
    void, \
    bool, \
    char, \
    short, \
    long, \
    long long, \
    unsigned char, \
    unsigned short, \
    unsigned long, \
    unsigned long long, \
    GRR_MSVC_TYPES \
    GRR_CLANG_TYPES \
    GRR_GCC_TYPES \
    \
    float, \
    double, \
    \
    grr::ptr_pair, \
    grr::const_ptr_pair, \
    grr::string, \
    grr::string_view \
    GRR_USER_TYPES
#endif

namespace grr
{
    template<typename T>
    struct is_fallback_type : std::false_type {};
    template<>
    struct is_fallback_type<ptr_pair> : std::true_type {};
    template<>
    struct is_fallback_type<const_ptr_pair> : std::true_type {};

    template<typename T>
    constexpr bool is_fallback_type_v = is_fallback_type<T>::value;

    namespace detail
    {
        template<typename T, typename U = void>
        struct is_key_value_map : std::false_type { };

        template<typename T>
        struct is_key_value_map<T, std::void_t<
            typename T::key_type,
            typename T::mapped_type,
            decltype(std::declval<T&>()[std::declval<const typename T::key_type&>()])>> 
                : std::true_type { };	
        
        template<typename T, typename U = void>
        struct is_key_map : std::false_type { };

        template<typename T>
        struct is_key_map<T, std::void_t<typename T::key_type>> : std::true_type { };

        template<typename T, typename U = void>
        struct is_container : std::false_type { };

        template<typename T>
        struct is_container<T, std::void_t<typename T::value_type>> : std::true_type { };
    }

    template<typename T>
    struct is_key_value_map : detail::is_key_value_map<T>::type { };
    template<typename T>
    constexpr bool is_key_value_map_v = is_key_value_map<T>::value;	
    
    template<typename T>
    struct is_key_map : detail::is_key_map<T>::type { };
    template<typename T>
    constexpr bool is_key_map_v = is_key_map<T>::value;

    template<typename T>
    struct is_container : detail::is_container<T>::type { };
    template<typename T>
    constexpr bool is_container_v = is_container<T>::value;	

    template<class T>
    using clean_type = std::remove_reference_t<std::remove_cv_t<T>>;

    template<typename T>
    constexpr bool is_container_not_string_v = is_container<T>::value && !std::is_same_v<grr::clean_type<T>, grr::string>;

    template <typename T, typename... Ts>
    struct pack_index : std::integral_constant<std::size_t, static_cast<std::size_t>(-1)> {};

    template <typename T, typename... Ts>
    struct pack_index<T, T, Ts...> : std::integral_constant<std::size_t, 0> {};

    template <typename T, typename U, typename... Ts>
    struct pack_index<T, U, Ts...> : std::integral_constant<std::size_t, 1 + pack_index<T, Ts...>::value> {};
}

#endif
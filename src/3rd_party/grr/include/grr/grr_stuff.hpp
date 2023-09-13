/***************************************************************************************
* Copyright (C) Anton Kovalev (vertver), 2023. All rights reserved.
* GRR - "Games Require Reflection", library for integrating reflection into games
* MIT License
***************************************************************************************/
#ifndef GRR_STUFF_HPP_INCLUDED
#define GRR_STUFF_HPP_INCLUDED

#if defined(GRR_TS_REFLECT) && !defined(__cpp_lib_reflection)
#error Unsupported compiler for C++ reflection feature
#endif

#ifndef GRR_USER_TYPES
#define GRR_USER_TYPES
#endif

#if defined(_MSC_VER) && __cplusplus == 199711L
#define GRR_CXX _MSVC_LANG 
#else
#define GRR_CXX __cplusplus 
#endif

#if GRR_CXX >= 202002L
#define GRR_CXX20_SUPPORT 1
#elif GRR_CXX < 201703L
#error Incompatible version of C++
#endif

#ifndef GRR_CONSTEXPR
#ifdef GRR_CXX20_SUPPORT
#define GRR_CONSTEXPR constexpr
#else
#define GRR_CONSTEXPR
#endif
#endif

#if GRR_CXX20_SUPPORT
#define GRR_LIKELY [[likely]]
#else
#define GRR_LIKELY
#endif

namespace grr
{
    template<typename T>
    constexpr bool is_reflectable_v =
        visit_struct::traits::is_visitable<T>::value ||
        pfr::is_implicitly_reflectable<T, T>::value;
}

#endif
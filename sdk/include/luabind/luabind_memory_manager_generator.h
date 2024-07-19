////////////////////////////////////////////////////////////////////////////
//	Module 		: luabind_memory_manager_generator.h
//	Created 	: 05.01.2006
//  Modified 	: 23.04.2008
//	Author		: Dmitriy Iassenev
//	Description : memory manager generator
////////////////////////////////////////////////////////////////////////////

#pragma once

#include <type_traits>

#ifndef LUABIND_MEMORY_MANAGER_GENERATOR
#    define LUABIND_MEMORY_MANAGER_GENERATOR
#endif // #ifndef LUABIND_MEMORY_MANAGER_GENERATOR

namespace luabind {

template <bool>
struct luabind_new_detail {
    template <typename T, typename... Args>
    static T* initialize(T *result, Args&&... args)
    {
        return	(new (result) T(std::forward<Args>(args)...));
    }
};

template <>
struct luabind_new_detail<true> {
    template <typename T, typename... Args>
    static T* initialize(T *result, Args&&...)
    {
        return	(result);
    }
};

struct luabind_new_detail_copy_constructor {
    template <typename T>
    static T* initialize(T *result, const T &value)
    {
        return	(new (result) T(value));
    }

    template <typename T>
    static T* initialize(T *result, T&& value)
    {
        return	(new (result) T(std::move(value)));
    }

    template <typename T, typename... Args>
    static T* initialize(T *result, Args&&... args)
    {
        return	(luabind_new_detail<std::is_pod_v<T>>::initialize(result, std::forward<Args>(args)...));
    }
};

template <typename T, typename... Args>
inline T* luabind_new(Args&&... args)
{
    T *pResult = static_cast<T*>(call_allocator(nullptr, sizeof(T)));
    return (luabind_new_detail_copy_constructor::initialize(pResult, std::forward<Args>(args)...));
}

}

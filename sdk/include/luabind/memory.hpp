////////////////////////////////////////////////////////////////////////////
//	Module 		: luabind_memory.h
//	Created 	: 24.06.2005
//  Modified 	: 23.04.2008
//	Author		: Dmitriy Iassenev
//	Description : luabind memory allocator template class
////////////////////////////////////////////////////////////////////////////

#ifndef LUABIND_MEMORY_HPP_INCLUDED
#define LUABIND_MEMORY_HPP_INCLUDED

#include <luabind/config.hpp>
#include <luabind/types.hpp>

#if defined(DEBUG) && defined(NDEBUG)
static_assert(false, "Do not define NDEBUG macros in DEBUG configuration since luabind classes are sensisitve to it");
#endif

namespace luabind
{
    using allocator_func = void* (__cdecl*)(void* context, void const* ptr, size_t);
    extern LUABIND_API allocator_func allocator;
    extern LUABIND_API void* allocator_context;

    namespace detail
    {
        inline void* call_allocator(void const* ptr, size_t size)
        {
            return allocator(allocator_context, ptr, size);
        }
    }

    template <typename T, typename... Args>
    inline T* luabind_new(Args&&... args)
    {
        auto result = reinterpret_cast<T*>(detail::call_allocator(nullptr, sizeof(T)));
        return new (result) T(std::forward<Args>(args)...);
    }

    namespace detail
    {
        template<typename T>
        inline void delete_helper2(T*& pointer, void* top_pointer)
        {
            pointer->~T();
            call_allocator(top_pointer, 0);
            pointer = nullptr;
        }

        template<typename T, bool polymorphic>
        struct delete_helper
        {
            static void apply(T*& pointer)
            {
                delete_helper2(pointer, dynamic_cast<void*>(pointer));
            }
        };

        template<typename T>
        struct delete_helper<T, false>
        {
            static void apply(T*& pointer)
            {
                delete_helper2(pointer, pointer);
            }
        };
    }

    template<typename T>
    inline void luabind_delete(T*& pointer)
    {
        if (!pointer)
            return;
        detail::delete_helper<T, std::is_polymorphic<T>::value>::apply(pointer);
    }

    template<typename T>
    struct luabind_deleter
    {
        void operator()(T* ptr) const
        {
            luabind_delete(ptr);
        }
    };
}

#endif // LUABIND_MEMORY_HPP_INCLUDED

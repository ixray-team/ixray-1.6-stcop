////////////////////////////////////////////////////////////////////////////
//	Module 		: luabind_memory_allocator.h
//	Created 	: 24.06.2005
//  Modified 	: 23.04.2008
//	Author		: Dmitriy Iassenev
//	Description : luabind memory allocator template class
////////////////////////////////////////////////////////////////////////////
#pragma once
#include <luabind/memory.hpp>

namespace luabind
{
    template<class T>
    class memory_allocator
    {
    private:
        typedef memory_allocator<T>	self_type;

    public:
        typedef	size_t size_type;
        typedef ptrdiff_t difference_type;
        typedef T* pointer;
        typedef T const* const_pointer;
        typedef T& reference;
        typedef T const& const_reference;
        typedef T value_type;

        template<class _1>
        struct rebind
        {
            typedef memory_allocator<_1> other;
        };

        memory_allocator() = default;
        memory_allocator(memory_allocator<T> const&) = default;

        template<class _1>
        inline memory_allocator(memory_allocator<_1> const&);

        template<class _1>
        inline memory_allocator<T> &operator=(memory_allocator<_1> const&);

        inline pointer address(reference value) const;
        inline const_pointer address(const_reference value) const;
        inline pointer allocate(size_type n, void const* p = 0) const;
        inline char* __charalloc(size_type n);
        inline void deallocate(pointer p, size_type n) const;
        inline void deallocate(void* p, size_type n) const;
        inline void construct(pointer p, T const& value);
        inline void destroy(pointer p);
        inline size_type max_size() const;
    };

    template<class _0, class _1>
    inline bool operator==(memory_allocator<_0> const&, memory_allocator<_1> const&);

    template<class _0, class _1>
    inline bool operator!=(memory_allocator<_0> const&, memory_allocator<_1> const&);
}

template<class T>
template<class other>
luabind::memory_allocator<T>::memory_allocator(memory_allocator<other> const&)
{}

template<class T>
template<class other>
luabind::memory_allocator<T>&luabind::memory_allocator<T>::operator=(memory_allocator<other> const&)
{
    return *this;
}

template<class T>
typename luabind::memory_allocator<T>::pointer luabind::memory_allocator<T>::address(reference value) const
{
    return &value;
}

template<class T>
typename luabind::memory_allocator<T>::const_pointer luabind::memory_allocator<T>::address(const_reference value) const
{
    return &value;
}

template<class T>
typename luabind::memory_allocator<T>::pointer luabind::memory_allocator<T>::allocate(size_type n, void const* p) const
{
    pointer result = (pointer)detail::call_allocator(p, n * sizeof(T));
    if (!n)
        result = (pointer)detail::call_allocator(p, sizeof(T));
    return result;
}

template<class T>
char* luabind::memory_allocator<T>::__charalloc(size_type const n)
{
    return (char*) allocate(n);
}

template<class T>
void luabind::memory_allocator<T>::deallocate(pointer const p, size_type const n) const
{
    detail::call_allocator(p, 0);
}

template<class T>
void luabind::memory_allocator<T>::deallocate(void* p, size_type n) const
{
    detail::call_allocator(p, 0);
}

template<class T>
void luabind::memory_allocator<T>::construct(pointer const p, T const& value)
{
    new(p) T(value);
}

template<class T>
void luabind::memory_allocator<T>::destroy(pointer const p)
{
    p->~T();
}

template<class T>
typename luabind::memory_allocator<T>::size_type luabind::memory_allocator<T>::max_size() const
{
    size_type count = (size_type)(-1) / sizeof(T);
    if (count)
        return count;
    return 1;
}

namespace luabind
{
    template<class _0, class _1>
    inline bool operator==(memory_allocator<_0> const&, memory_allocator<_1> const&)
    {
        return true;
    }

    template<class _0, class _1>
    inline bool operator!=(memory_allocator<_0> const&, memory_allocator<_1> const&)
    {
        return false;
    }
}
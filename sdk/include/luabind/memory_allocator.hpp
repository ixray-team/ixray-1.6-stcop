////////////////////////////////////////////////////////////////////////////
//	Module 		: luabind_memory_allocator.h
//	Created 	: 24.06.2005
//  Modified 	: 23.04.2008
//	Author		: Dmitriy Iassenev
//	Description : luabind memory allocator template class
////////////////////////////////////////////////////////////////////////////

#ifndef LUABIND_MEMORY_ALLOCATOR_HPP_INCLUDED
#define LUABIND_MEMORY_ALLOCATOR_HPP_INCLUDED

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

        memory_allocator();
        memory_allocator(memory_allocator<T> const&);

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

#define TEMPLATE_SPECIALIZATION	template<class T>
#define MEMORY_ALLOCATOR luabind::memory_allocator<T>

TEMPLATE_SPECIALIZATION
MEMORY_ALLOCATOR::memory_allocator()
{}

TEMPLATE_SPECIALIZATION
MEMORY_ALLOCATOR::memory_allocator(self_type const&)
{}

TEMPLATE_SPECIALIZATION
template<class other>
MEMORY_ALLOCATOR::memory_allocator(memory_allocator<other> const&)
{}

TEMPLATE_SPECIALIZATION
template<class other>
MEMORY_ALLOCATOR&MEMORY_ALLOCATOR::operator=(memory_allocator<other> const&)
{
    return *this;
}

TEMPLATE_SPECIALIZATION
typename MEMORY_ALLOCATOR::pointer MEMORY_ALLOCATOR::address(reference value) const
{
    return &value;
}

TEMPLATE_SPECIALIZATION
typename MEMORY_ALLOCATOR::const_pointer MEMORY_ALLOCATOR::address(const_reference value) const
{
    return &value;
}

TEMPLATE_SPECIALIZATION
typename MEMORY_ALLOCATOR::pointer MEMORY_ALLOCATOR::allocate(size_type n, void const* p) const
{
    pointer result = (pointer)detail::call_allocator(p, n * sizeof(T));
    if (!n)
        result = (pointer)detail::call_allocator(p, sizeof(T));
    return result;
}

TEMPLATE_SPECIALIZATION
char* MEMORY_ALLOCATOR::__charalloc(size_type const n)
{
    return (char _FARQ*)allocate(n);
}

TEMPLATE_SPECIALIZATION
void MEMORY_ALLOCATOR::deallocate(pointer const p, size_type const n) const
{
    detail::call_allocator(p, 0);
}

TEMPLATE_SPECIALIZATION
void MEMORY_ALLOCATOR::deallocate(void* p, size_type n) const
{
    detail::call_allocator(p, 0);
}

TEMPLATE_SPECIALIZATION
void MEMORY_ALLOCATOR::construct(pointer const p, T const& value)
{
    new(p) T(value);
}

TEMPLATE_SPECIALIZATION
void MEMORY_ALLOCATOR::destroy(pointer const p)
{
    p->~T();
}

TEMPLATE_SPECIALIZATION
typename MEMORY_ALLOCATOR::size_type MEMORY_ALLOCATOR::max_size() const
{
    size_type count = (size_type)(-1) / sizeof(T);
    if (count)
        return count;
    return 1;
}

#undef TEMPLATE_SPECIALIZATION
#undef MEMORY_ALLOCATOR

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

#endif // LUABIND_MEMORY_ALLOCATOR_HPP_INCLUDED

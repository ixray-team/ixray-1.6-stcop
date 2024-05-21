#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_FIXED_VECTOR_H__
#define __XR_FIXED_VECTOR_H__

#include "xr_types.h"

namespace xray_re {

// FIXME: the implementation is not complete
template<typename T, uint32_t N> struct _svector {
	typedef T value_type;
	typedef T* iterator;
	typedef const T* const_iterator;
	typedef T& reference;
	typedef const T& const_reference;
	typedef T* pointer;
	typedef const T* const_pointer;

				_svector();
	iterator		begin();
	const_iterator		begin() const;
	iterator		end();
	const_iterator		end() const;
	size_t			size() const;
	size_t			capacity() const;
	void			clear();
	void			resize(size_t _size);
	void			reserve(size_t _size);
	void			push_back(const_reference value);
	void			pop_back();
	reference		operator[](size_t at);
	const_reference		operator[](size_t at) const;
	reference		front();
	const_reference		front() const;
	reference		back();
	const_reference		back() const;
	bool			empty() const;
	bool			full() const;
	void			erase(iterator at);
	void			erase(size_t at);
	void			append(const _svector<T, N>& that);
	iterator		insert(iterator at, const_reference value);
	template<class I> void	assign(I first, I last);

	int			compare(const _svector<T, N>& right) const;
	bool			operator<(const _svector<T, N>& right) const;
	bool			operator==(const _svector<T, N>& right) const;

	T			array[N];
	uint32_t		count;
};

template<typename T, uint32_t N> inline _svector<T, N>::_svector(): count(0) {}

template<typename T, uint32_t N> inline
typename _svector<T, N>::iterator _svector<T, N>::begin() { return array; }

template<typename T, uint32_t N> inline
typename _svector<T, N>::const_iterator _svector<T, N>::begin() const { return array; }

template<typename T, uint32_t N> inline
typename _svector<T, N>::iterator _svector<T, N>::end() { return &array[count]; }

template<typename T, uint32_t N> inline
typename _svector<T, N>::const_iterator _svector<T, N>::end() const { return &array[count]; }

template<typename T, uint32_t N> inline size_t _svector<T, N>::size() const { return count; }

template<typename T, uint32_t N> inline size_t _svector<T, N>::capacity() const { return N; }

template<typename T, uint32_t N> inline void _svector<T, N>::clear()
{
	while (count)
		array[--count].~T();
}

template<typename T, uint32_t N> inline void _svector<T, N>::resize(size_t _size)
{
	if (size() < _size)
		;
	else if (_size < size())
		erase(_size);
}

template<typename T, uint32_t N> inline void _svector<T, N>::reserve(size_t _size) { assert(_size <= N); }

template<typename T, uint32_t N> inline void _svector<T, N>::push_back(const T& value)
{
	assert(count < N);
	array[count++] = value;
}

template<typename T, uint32_t N> inline void _svector<T, N>::pop_back()
{
	assert(!empty());
	--count;
	array[count].~T();
}

template<typename T, uint32_t N> inline typename _svector<T, N>::reference
_svector<T, N>::operator[](size_t at) { assert(at < count); return array[at]; }

template<typename T, uint32_t N> inline typename _svector<T, N>::const_reference
_svector<T, N>::operator[](size_t at) const { assert(at < count); return array[at]; }

template<typename T, uint32_t N> inline typename _svector<T, N>::reference
_svector<T, N>::front() { assert(!empty()); return array[0]; }

template<typename T, uint32_t N> inline typename _svector<T, N>::const_reference
_svector<T, N>::front() const { assert(!empty()); return array[0]; }

template<typename T, uint32_t N> inline typename _svector<T, N>::reference
_svector<T, N>::back() { assert(!empty()); return array[count - 1]; }

template<typename T, uint32_t N> inline typename _svector<T, N>::const_reference
_svector<T, N>::back() const { assert(!empty()); return array[count - 1]; }

template<typename T, uint32_t N> inline bool _svector<T, N>::empty() const { return count == 0; }

template<typename T, uint32_t N> inline bool _svector<T, N>::full() const { return count == N; }

#if 0
template<typename T, uint32_t N> inline void erase(iterator at)
{
	assert(at >= begin() && at 
}

template<typename T, uint32_t N> inline void erase(size_t at)
{
	count = uint32_t(at & UINT32_MAX);
	for (; at < count; ++at)
}
#endif

template<typename T, uint32_t N> inline typename _svector<T, N>::iterator
_svector<T, N>::insert(iterator at, const_reference value)
{
	xr_not_implemented();
}

template<typename T, uint32_t N> inline void _svector<T, N>::append(const _svector<T, N>& that)
{
	assert(count + that.size() <= N);
	for (const_iterator it = that.begin(), end = that.end(); it != end; ++it)
		array[count++] = *it;
}

template<typename T, uint32_t N> template<class I> inline void _svector<T, N>::assign(I first, I last)
{
	clear();
	for (; first != last; ++first) {
		assert(count < N);
		array[count++] = *first;
	}
}

template<typename T, uint32_t N> inline int _svector<T, N>::compare(const _svector<T, N>& right) const
{
	const_iterator it1 = begin(), end1 = end();
	const_iterator it2 = right.begin(), end2 = right.end();
	for (; it1 != end1 && it2 != end2; ++it1, ++it2) {
		if (!(*it1 == *it2))
			return (*it1 < *it2) ? -1 : 1;
	}
	return (it1 == end1) ? (it2 == end2 ? 0 : -1) : 1;
}

template<typename T, uint32_t N> inline bool
_svector<T, N>::operator<(const _svector<T, N>& right) const { return compare(right) < 0; }

template<typename T, uint32_t N> inline bool
_svector<T, N>::operator==(const _svector<T, N>& right) const { return compare(right) == 0; }

} // end of namespace xray_re

#endif

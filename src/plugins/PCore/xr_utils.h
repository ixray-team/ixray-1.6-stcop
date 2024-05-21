#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_UTILS_H__
#define __XR_UTILS_H__

#include <memory>
#include <string>

namespace xray_re {

template<typename T> static inline
typename T::value_type find_by_name(T& container, const char* name)
{
	for (typename T::iterator it = container.begin(),
			end = container.end(); it != end; ++it) {
		if ((*it)->name() == name)
			return *it;
	}
	return 0;
}

template<typename T> static inline
typename T::value_type find_by_name(T& container, const std::string& name)
{
	return find_by_name<T>(container, name.c_str());
}

template<typename T> void delete_elements(T* container[], size_t n)
{
	for (T **p = container, **end = p + n; p != end; ++p)
		delete *p;
}

template<typename T> void delete_elements(T& container)
{
	for (typename T::iterator it = container.begin(), end = container.end(); it != end; ++it)
		delete *it;
}

template<typename T> void clear_container(T& container)
{
	for (typename T::iterator it = container.begin(), end = container.end(); it != end; ++it)
		delete *it;
	container.clear();
}

template<typename T> void trim_container(T& container)
{
	for (typename T::iterator it = container.begin(), end = container.end(); it != end; ++it)
		delete *it;
	T().swap(container);
}

template<typename T> inline int __compare(const T& left, const T& right)
{
	return (left < right) ? -1 : (left == right ? 0 : +1);
}

template<typename I, typename P> inline I lower_bound_if(I first, I last, P pred)
{
	for (ptrdiff_t count = last - first; 0 < count; ) {
		ptrdiff_t count2 = count / 2;
		I it = first + count2;
		if (pred(*it)) {
			first = ++it;
			count -= count2 + 1;
		} else {
			count = count2;
		}
	}
	return first;
}

template<typename T> inline void delete_safe(const T*& p) { delete p; p = 0; }

template<typename T> struct ptr_less {
	inline bool operator()(const T* l, const T* r) const { return *l < *r; }
};

template<typename T> struct ptr_greater {
	inline bool operator()(const T* l, const T* r) const { return *l > *r; }
};

#if defined(_MSC_VER) && _MSC_VER >= 1400 && _MSC_VER < 1600
#define xr_uninitialized_fill_n(p, n, v)	stdext::unchecked_uninitialized_fill_n((p), (n), (v))
#else
#define xr_uninitialized_fill_n(p, n, v)	std::uninitialized_fill_n((p), (n), (v))
#endif

} // end of namespace xray_re

#endif

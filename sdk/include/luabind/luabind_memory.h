////////////////////////////////////////////////////////////////////////////
//	Module 		: luabind_memory.h
//	Created 	: 24.06.2005
//  Modified 	: 23.04.2008
//	Author		: Dmitriy Iassenev
//	Description : luabind memory
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "luabind_api.h"
#include <vector>
#include <list>
#include <map>
#include <set>
#include <string>

#ifdef DEBUG
#	ifdef NDEBUG
        static_assert(false, "Do not define NDEBUG macros in DEBUG configuration since luabind classes are sensisitve to it")
#	endif // #ifdef NDEBUG
#endif // #ifdef DEBUG

namespace luabind 
{
	static void* __cdecl luabind_allocator(const void* pointer, size_t const size) 
	{
		if (!size)
		{
			void* non_const_pointer = const_cast<void*>(pointer);
			free(non_const_pointer);
			return nullptr;
		}

		if (!pointer) 
		{
			return malloc(size);
		}

		void* non_const_pointer = const_cast<void*>(pointer);
		return realloc(non_const_pointer, size);
	}

	inline void* call_allocator	(void const* buffer, size_t const size)
	{
		return (luabind_allocator(buffer, size));
	}
} // namespace luabind

#include <luabind/luabind_delete.h>
#include <luabind/luabind_memory_manager_generator.h>

#include <luabind/luabind_memory_allocator.h>

namespace luabind {
	template	<typename T>									class	internal_vector : public std::vector<T, memory_allocator<T> > { public: inline unsigned int size() const { return (unsigned int)(std::vector<T, memory_allocator<T> >::size()); } };
	template	<typename T>									class	internal_list : public std::list<T, memory_allocator<T> > { public: };
	template	<typename K, class P = std::less<K> >				class	internal_set : public std::set<K, P, memory_allocator<K> > { public: };
	template	<typename K, class P = std::less<K> >				class	internal_multiset : public std::multiset<K, P, memory_allocator<K> > { public: };
	template	<typename K, class V, class P = std::less<K> >	class	internal_map : public std::map<K, V, P, memory_allocator<std::pair<const K, V> > > { public: };
	template	<typename K, class V, class P = std::less<K> >	class	internal_multimap : public std::multimap<K, V, P, memory_allocator<std::pair<const K, V> > > { public: };

	typedef		std::basic_string<char, std::char_traits<char>, memory_allocator<char> >		internal_string;
} // namespace luabind


#pragma once

#include <memory>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <queue>
#include <luabind/memory.hpp>
#include <luabind/memory_allocator.hpp>

namespace luabind
{
    template<typename T>
    using unique_ptr = std::unique_ptr<T, luabind_deleter<T>>;

    using string = std::basic_string<char, std::char_traits<char>, memory_allocator<char>>;
    using stringstream = std::basic_stringstream<char, std::char_traits<char>, memory_allocator<char>>;

    template<typename T>
    using vector = std::vector<T, memory_allocator<T>>;

    template<typename T>
    using list = std::list<T, memory_allocator<T>>;

    template<typename K, class P = std::less<K>>
    using set = std::set<K, P, memory_allocator<K>>;

    template<typename K, class P = std::less<K>>
    using multiset = std::multiset<K, P, memory_allocator<K>>;

    template<typename K, class V, class P = std::less<K>>
    using map = std::map<K, V, P, memory_allocator<std::pair<const K, V>>>;

    template<typename K, class V, class P = std::less<K>>
    using multimap = std::multimap<K, V, P, memory_allocator<std::pair<const K, V>>>;

    template<typename T>
    using dequeue = std::deque<T, memory_allocator<T>>;

    template<typename T>
    using queue = std::queue<T, dequeue<T>>;
}
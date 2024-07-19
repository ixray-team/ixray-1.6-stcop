// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#pragma once
#include <sm_boost/boost_legacy.h>
#include <luabind/config.hpp>
#include <luabind/detail/policy.hpp>

namespace luabind { namespace detail
{
	template<class T>
	void convert_to_lua(lua_State* L, const T& v)
	{
		using value_type = typename std::unwrap_reference<T>::type;
		typename default_policy::generate_converter<value_type, Direction::cpp_to_lua>::type converter;

		if constexpr (boost_legacy::is_reference_wrapper_v<T>)
		{
			converter.apply(L, v.get());
		}
		else
		{
			converter.apply(L, v);
		}
	}

	template<int Index, class T, typename... Policies>
	void convert_to_lua_p(lua_State* L, const T& v, const policy_cons<Policies...>)
	{
		using value_type = typename std::unwrap_reference<T>::type;
	    using converter_policy = typename find_conversion_policy<Index, Policies...>::type;
		typename converter_policy::template generate_converter<value_type, Direction::cpp_to_lua>::type converter;

		if constexpr (boost_legacy::is_reference_wrapper_v<T>)
		{
			converter.apply(L, v.get());
		}
		else
		{
			converter.apply(L, v);
		}
	}
}}
///////////////////////////////////////////////////////////////////////////////////
/// OpenGL Image (gli.g-truc.net)
///
/// Copyright (c) 2008 - 2014 G-Truc Creation (www.g-truc.net)
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to deal
/// in the Software without restriction, including without limitation the rights
/// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
/// copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
/// 
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
/// 
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
/// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
/// THE SOFTWARE.
///
/// @ref core
/// @file gli/type.hpp
/// @date 2014-07-28 / 2015-08-29
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

// STD
#include <cstddef>

// GLM
#define GLM_FORCE_EXPLICIT_CTOR
#include <glm/glm.hpp>
#include <glm/gtc/vec1.hpp>
#include <glm/gtx/std_based_type.hpp>

#if GLM_COMPILER & GLM_COMPILER_VC
#	define GLI_FORCE_INLINE __forceinline
#elif GLM_COMPILER & (GLM_COMPILER_GCC | GLM_COMPILER_APPLE_CLANG | GLM_COMPILER_LLVM)
#	define GLI_FORCE_INLINE inline __attribute__((__always_inline__))
#else
#	define GLI_FORCE_INLINE inline
#endif//GLM_COMPILER

#define GLI_DISABLE_ASSERT 0

#if defined(NDEBUG) || GLI_DISABLE_ASSERT
#	define GLI_ASSERT(test)
#else
#	define GLI_ASSERT(test) assert((test))
#endif

namespace gli
{
	using namespace glm;

	using std::size_t;

	typedef tvec1<size_t> dim1_t;
	typedef tvec2<size_t> dim2_t;
	typedef tvec3<size_t> dim3_t;
	typedef tvec4<size_t> dim4_t;

	template <typename T, precision P>
	inline tvec4<T, P> make_vec4(tvec1<T, P> const & v)
	{
		return tvec4<T, P>(v.x, static_cast<T>(0), static_cast<T>(0), static_cast<T>(1));
	}

	template <typename T, precision P>
	inline tvec4<T, P> make_vec4(tvec2<T, P> const & v)
	{
		return tvec4<T, P>(v.x, v.y, static_cast<T>(0), static_cast<T>(1));
	}

	template <typename T, precision P>
	inline tvec4<T, P> make_vec4(tvec3<T, P> const & v)
	{
		return tvec4<T, P>(v.x, v.y, v.z, static_cast<T>(1));
	}

	template <typename T, precision P>
	inline tvec4<T, P> make_vec4(tvec4<T, P> const & v)
	{
		return v;
	}
}//namespace gli

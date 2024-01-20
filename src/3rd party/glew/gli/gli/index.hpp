///////////////////////////////////////////////////////////////////////////////////
/// OpenGL Image (gli.g-truc.net)
///
/// Copyright (c) 2008 - 2015 G-Truc Creation (www.g-truc.net)
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
/// @file gli/index.hpp
/// @date 2015-09-27 / 2015-09-27
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "type.hpp"

namespace gli
{
	template <typename T, precision P>
	inline size_t linear_index(tvec1<T, P> const & Coord, tvec1<T, P> const & Dimensions)
	{
		GLI_ASSERT(glm::all(glm::lessThan(Coord, Dimensions)));
		return static_cast<size_t>(Coord.x);
	}

	template <typename T, precision P>
	inline size_t linear_index(tvec2<T, P> const & Coord, tvec2<T, P> const & Dimensions)
	{
		GLI_ASSERT(glm::all(glm::lessThan(Coord, Dimensions)));
		return static_cast<size_t>(Coord.x + Coord.y * Dimensions.x);
	}

	template <typename T, precision P>
	inline size_t linear_index(tvec3<T, P> const & Coord, tvec3<T, P> const & Dimensions)
	{
		GLI_ASSERT(glm::all(glm::lessThan(Coord, Dimensions)));
		return static_cast<size_t>(Coord.x + Coord.y * Dimensions.x + Coord.z * Dimensions.x * Dimensions.y);
	}
}//namespace gli


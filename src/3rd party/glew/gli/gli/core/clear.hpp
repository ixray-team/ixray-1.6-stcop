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
/// @file gli/core/clear.hpp
/// @date 2015-10-14 / 2015-10-14
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "convert.hpp"

namespace gli{
namespace detail
{
	template <typename textureType, typename T, precision P>
	struct clear
	{
		static void call(textureType & Texture, typename convert<textureType, T, P>::writeFunc Write, tvec4<T, P> const & Color)
		{
			assert(Write);

			texture const ConvertTexel(Texture.target(), Texture.format(), texture::texelcoord_type(1), 1, 1, 1);
			textureType Texel(ConvertTexel);
			Write(Texel, typename textureType::texelcoord_type(0), 0, 0, 0, Color);

			size_t const BlockSize(block_size(Texture.format()));
			for(size_t BlockIndex = 0, BlockCount = Texture.size() / BlockSize; BlockIndex < BlockCount; ++BlockIndex)
				memcpy(static_cast<std::uint8_t*>(Texture.data()) + BlockSize * BlockIndex, Texel.data(), BlockSize);
		}
	};
}//namespace detail
}//namespace gli

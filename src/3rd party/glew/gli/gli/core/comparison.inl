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
/// @file gli/core/comparison.inl
/// @date 2013-02-04 / 2013-02-04
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <cstring>

namespace gli{
namespace detail
{
	inline bool equalData(texture const & TextureA, texture const & TextureB)
	{
		GLI_ASSERT(TextureA.size() == TextureB.size());

		if(TextureA.data() == TextureB.data())
			return true;

		for(texture::size_type LayerIndex = 0, LayerCount = TextureA.layers(); LayerIndex < LayerCount; ++LayerIndex)
		for(texture::size_type FaceIndex = 0, FaceCount = TextureA.faces(); FaceIndex < FaceCount; ++FaceIndex)
		for(texture::size_type LevelIndex = 0, LevelCount = TextureA.levels(); LevelIndex < LevelCount; ++LevelIndex)
		{
			void const* PointerA = TextureA.data(LayerIndex, FaceIndex, LevelIndex);
			void const* PointerB = TextureB.data(LayerIndex, FaceIndex, LevelIndex);
			if(std::memcmp(PointerA, PointerB, TextureA.size(LevelIndex)) != 0)
				return false;
		}

		return true;
	}
}//namespace detail

	inline bool operator==(image const & ImageA, image const & ImageB)
	{
		if(!glm::all(glm::equal(ImageA.dimensions(), ImageB.dimensions())))
			return false;
		if(ImageA.size() != ImageB.size())
			return false;

		return std::memcmp(ImageA.data(), ImageB.data(), ImageA.size()) == 0;
	}

	inline bool operator!=(image const & ImageA, image const & ImageB)
	{
		if(!glm::all(glm::equal(ImageA.dimensions(), ImageB.dimensions())))
			return true;
		if(ImageA.size() != ImageB.size())
			return true;

		return std::memcmp(ImageA.data(), ImageB.data(), ImageA.size()) != 0;
	}

	inline bool equal(texture const & TextureA, texture const & TextureB)
	{
		if(TextureA.empty() && TextureB.empty())
			return true;
		if(TextureA.empty() != TextureB.empty())
			return false;
		if(TextureA.target() != TextureB.target())
			return false;
		if(TextureA.layers() != TextureB.layers())
			return false;
		if(TextureA.faces() != TextureB.faces())
			return false;
		if(TextureA.levels() != TextureB.levels())
			return false;
		if(TextureA.format() != TextureB.format())
			return false;
		if(TextureA.size() != TextureB.size())
			return false;

		return detail::equalData(TextureA, TextureB);
	}

	inline bool notEqual(texture const & TextureA, texture const & TextureB)
	{
		if(TextureA.empty() && TextureB.empty())
			return false;
		if(TextureA.empty() != TextureB.empty())
			return true;
		if(TextureA.target() != TextureB.target())
			return true;
		if(TextureA.layers() != TextureB.layers())
			return true;
		if(TextureA.faces() != TextureB.faces())
			return true;
		if(TextureA.levels() != TextureB.levels())
			return true;
		if(TextureA.format() != TextureB.format())
			return true;
		if(TextureA.size() != TextureB.size())
			return true;

		return !detail::equalData(TextureA, TextureB);
	}

	inline bool operator==(texture const & A, texture const & B)
	{
		return gli::equal(A, B);
	}

	inline bool operator!=(texture const & A, texture const & B)
	{
		return gli::notEqual(A, B);
	}
}//namespace gli

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
/// @file gli/core/texture2d_array.inl
/// @date 2013-01-12 / 2013-01-12
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include "../levels.hpp"

namespace gli
{
	inline texture2DArray::texture2DArray()
	{}

	inline texture2DArray::texture2DArray(format_type Format, texelcoord_type const & Dimensions, size_type Layers)
		: texture(TARGET_2D_ARRAY, Format, texture::texelcoord_type(Dimensions, 1), Layers, 1, gli::levels(Dimensions))
	{
		this->build_cache();
	}

	inline texture2DArray::texture2DArray(format_type Format, texelcoord_type const & Dimensions, size_type Layers, size_type Levels)
		: texture(TARGET_2D_ARRAY, Format, texture::texelcoord_type(Dimensions, 1), Layers, 1, Levels)
	{
		this->build_cache();
	}

	inline texture2DArray::texture2DArray(texture const & Texture)
		: texture(Texture, gli::TARGET_2D_ARRAY, Texture.format())
	{
		this->build_cache();
	}

	inline texture2DArray::texture2DArray
	(
		texture const & Texture,
		format_type Format,
		size_type BaseLayer, size_type MaxLayer,
		size_type BaseFace, size_type MaxFace,
		size_type BaseLevel, size_type MaxLevel
	)
		: texture(
			Texture, TARGET_2D_ARRAY,
			Format,
			BaseLayer, MaxLayer,
			BaseFace, MaxFace,
			BaseLevel, MaxLevel)
	{
		this->build_cache();
	}

	inline texture2DArray::texture2DArray
	(
		texture2DArray const & Texture,
		size_type BaseLayer, size_type MaxLayer,
		size_type BaseLevel, size_type MaxLevel
	)
		: texture(
			Texture, gli::TARGET_2D_ARRAY,
			Texture.format(),
			Texture.base_layer() + BaseLayer, Texture.base_layer() + MaxLayer,
			Texture.base_face(), Texture.max_face(),
			Texture.base_level() + BaseLevel, Texture.base_level() + MaxLevel)
	{
		this->build_cache();
	}

	inline texture2D texture2DArray::operator[](size_type Layer) const
	{
		GLI_ASSERT(Layer < this->layers());

		return texture2D(
			*this, this->format(),
			this->base_layer() + Layer, this->base_layer() + Layer,
			this->base_face(), this->max_face(),
			this->base_level(), this->max_level());
	}

	inline texture2DArray::texelcoord_type texture2DArray::dimensions(size_type Level) const
	{
		GLI_ASSERT(!this->empty());

		return this->Caches[this->index_cache(0, Level)].Dim;
	}

	template <typename genType>
	inline genType texture2DArray::load(texelcoord_type const & TexelCoord, size_type Layer, size_type Level) const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(!is_compressed(this->format()));
		GLI_ASSERT(block_size(this->format()) == sizeof(genType));

		cache const & Cache = this->Caches[this->index_cache(Layer, Level)];

		std::size_t const Index = linear_index(TexelCoord, Cache.Dim);
		GLI_ASSERT(Index < Cache.Size / sizeof(genType));

		return reinterpret_cast<genType const * const>(Cache.Data)[Index];
	}

	template <typename genType>
	inline void texture2DArray::store(texelcoord_type const & TexelCoord, size_type Layer, size_type Level, genType const & Texel)
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(!is_compressed(this->format()));
		GLI_ASSERT(block_size(this->format()) == sizeof(genType));

		cache& Cache = this->Caches[this->index_cache(Layer, Level)];
		GLI_ASSERT(glm::all(glm::lessThan(TexelCoord, Cache.Dim)));

		std::size_t const Index = linear_index(TexelCoord, Cache.Dim);
		GLI_ASSERT(Index < Cache.Size / sizeof(genType));

		reinterpret_cast<genType*>(Cache.Data)[Index] = Texel;
	}

	inline void texture2DArray::clear()
	{
		this->texture::clear();
	}

	template <typename genType>
	inline void texture2DArray::clear(genType const & Texel)
	{
		this->texture::clear<genType>(Texel);
	}

	template <typename genType>
	inline void texture2DArray::clear(size_type Layer, size_type Level, genType const & Texel)
	{
		this->texture::clear<genType>(Layer, 0, Level, Texel);
	}

	inline texture2DArray::size_type texture2DArray::index_cache(size_type Layer, size_type Level) const
	{
		return Layer * this->levels() + Level;
	}

	inline void texture2DArray::build_cache()
	{
		this->Caches.resize(this->layers() * this->levels());

		for(size_type Layer = 0; Layer < this->layers(); ++Layer)
		for(size_type Level = 0; Level < this->levels(); ++Level)
		{
			cache& Cache = this->Caches[this->index_cache(Layer, Level)];
			Cache.Data = this->data<std::uint8_t>(Layer, 0, Level);
			Cache.Dim = glm::max(texture2D::texelcoord_type(this->texture::dimensions(Level)), texture2D::texelcoord_type(1));
#			ifndef NDEBUG
				Cache.Size = this->size(Level);
#			endif
		}
	}
}//namespace gli

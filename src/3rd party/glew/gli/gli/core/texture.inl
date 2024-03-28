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
/// @file gli/core/texture.inl
/// @date 2015-08-20 / 2015-08-20
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

namespace gli
{
	inline texture::texture()
		: Storage(nullptr)
		, Target(static_cast<gli::target>(TARGET_INVALID))
		, Format(static_cast<gli::format>(FORMAT_INVALID))
		, BaseLayer(0), MaxLayer(0)
		, BaseFace(0), MaxFace(0)
		, BaseLevel(0), MaxLevel(0)
		, Swizzles(SWIZZLE_ZERO)
	{}

	inline texture::texture
	(
		target_type Target,
		format_type Format,
		texelcoord_type const & Dimensions,
		size_type Layers,
		size_type Faces,
		size_type Levels,
		swizzles_type const & Swizzles
	)
		: Storage(std::make_shared<storage>(Format, Dimensions, Layers, Faces, Levels))
		, Target(Target)
		, Format(Format)
		, BaseLayer(0), MaxLayer(Layers - 1)
		, BaseFace(0), MaxFace(Faces - 1)
		, BaseLevel(0), MaxLevel(Levels - 1)
		, Swizzles(Swizzles)
	{
		assert(Target != TARGET_CUBE || (Target == TARGET_CUBE && Dimensions.x == Dimensions.y));
		assert(Target != TARGET_CUBE_ARRAY || (Target == TARGET_CUBE_ARRAY && Dimensions.x == Dimensions.y));

		this->build_cache();
	}

	inline texture::texture
	(
		texture const & Texture,
		target_type Target,
		format_type Format,
		size_type BaseLayer, size_type MaxLayer,
		size_type BaseFace, size_type MaxFace,
		size_type BaseLevel, size_type MaxLevel,
		swizzles_type const & Swizzles
	)
		: Storage(Texture.Storage)
		, Target(Target)
		, Format(Format)
		, BaseLayer(BaseLayer), MaxLayer(MaxLayer)
		, BaseFace(BaseFace), MaxFace(MaxFace)
		, BaseLevel(BaseLevel), MaxLevel(MaxLevel)
		, Swizzles(Swizzles)
	{
		GLI_ASSERT(block_size(Format) == block_size(Texture.format()));
		GLI_ASSERT(Target != TARGET_1D || (Target == TARGET_1D && this->layers() == 1 && this->faces() == 1 && this->dimensions().y == 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_1D_ARRAY || (Target == TARGET_1D_ARRAY && this->layers() >= 1 && this->faces() == 1 && this->dimensions().y == 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_2D || (Target == TARGET_2D && this->layers() == 1 && this->faces() == 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_2D_ARRAY || (Target == TARGET_2D_ARRAY && this->layers() >= 1 && this->faces() == 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_3D || (Target == TARGET_3D && this->layers() == 1 && this->faces() == 1 && this->dimensions().y >= 1 && this->dimensions().z >= 1));
		GLI_ASSERT(Target != TARGET_CUBE || (Target == TARGET_CUBE && this->layers() == 1 && this->faces() >= 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_CUBE_ARRAY || (Target == TARGET_CUBE_ARRAY && this->layers() >= 1 && this->faces() >= 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));

		this->build_cache();
	}

	inline texture::texture
	(
		texture const & Texture,
		target_type Target,
		format_type Format,
		swizzles_type const & Swizzles
	)
		: Storage(Texture.Storage)
		, Target(Target)
		, Format(Format)
		, BaseLayer(Texture.base_layer()), MaxLayer(Texture.max_layer())
		, BaseFace(Texture.base_face()), MaxFace(Texture.max_face())
		, BaseLevel(Texture.base_level()), MaxLevel(Texture.max_level())
		, Swizzles(Swizzles)
	{
		if(this->empty())
			return;

		GLI_ASSERT(Target != TARGET_1D || (Target == TARGET_1D && this->layers() == 1 && this->faces() == 1 && this->dimensions().y == 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_1D_ARRAY || (Target == TARGET_1D_ARRAY && this->layers() >= 1 && this->faces() == 1 && this->dimensions().y == 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_2D || (Target == TARGET_2D && this->layers() == 1 && this->faces() == 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_2D_ARRAY || (Target == TARGET_2D_ARRAY && this->layers() >= 1 && this->faces() == 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_3D || (Target == TARGET_3D && this->layers() == 1 && this->faces() == 1 && this->dimensions().y >= 1 && this->dimensions().z >= 1));
		GLI_ASSERT(Target != TARGET_CUBE || (Target == TARGET_CUBE && this->layers() == 1 && this->faces() >= 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));
		GLI_ASSERT(Target != TARGET_CUBE_ARRAY || (Target == TARGET_CUBE_ARRAY && this->layers() >= 1 && this->faces() >= 1 && this->dimensions().y >= 1 && this->dimensions().z == 1));

		this->build_cache();
	}

	inline bool texture::empty() const
	{
		if(this->Storage.get() == nullptr)
			return true;

		return this->Storage->empty();
	}

	inline texture::format_type texture::format() const
	{
		return this->Format;
	}

	inline texture::swizzles_type texture::swizzles() const
	{
		swizzles_type const FormatSwizzle = detail::get_format_info(this->format()).Swizzles;
		swizzles_type const CustomSwizzle = this->Swizzles;
		return swizzles_type(
			is_channel(CustomSwizzle.r) ? FormatSwizzle[CustomSwizzle.r] : CustomSwizzle.r,
			is_channel(CustomSwizzle.g) ? FormatSwizzle[CustomSwizzle.g] : CustomSwizzle.g,
			is_channel(CustomSwizzle.b) ? FormatSwizzle[CustomSwizzle.b] : CustomSwizzle.b,
			is_channel(CustomSwizzle.a) ? FormatSwizzle[CustomSwizzle.a] : CustomSwizzle.a);
	}

	inline texture::size_type texture::base_layer() const
	{
		return this->BaseLayer;
	}

	inline texture::size_type texture::max_layer() const
	{
		return this->MaxLayer;
	}

	inline texture::size_type texture::layers() const
	{
		if (this->empty())
			return 0;
		return this->max_layer() - this->base_layer() + 1;
	}

	inline texture::size_type texture::base_face() const
	{
		return this->BaseFace;
	}

	inline texture::size_type texture::max_face() const
	{
		return this->MaxFace;
	}

	inline texture::size_type texture::faces() const
	{
		if (this->empty())
			return 0;
		return this->max_face() - this->base_face() + 1;
	}

	inline texture::size_type texture::base_level() const
	{
		return this->BaseLevel;
	}

	inline texture::size_type texture::max_level() const
	{
		return this->MaxLevel;
	}

	inline texture::size_type texture::levels() const
	{
		if (this->empty())
			return 0;
		return this->max_level() - this->base_level() + 1;
	}

	inline texture::size_type texture::size() const
	{
		GLI_ASSERT(!this->empty());

		return this->Cache.Size;
	}

	template <typename genType>
	inline texture::size_type texture::size() const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) == sizeof(genType));

		return this->size() / sizeof(genType);
	}

	inline texture::size_type texture::size(size_type Level) const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(Level >= 0 && Level < this->levels());

		return this->Storage->level_size(this->BaseLevel + Level);
	}

	template <typename genType>
	inline texture::size_type texture::size(size_type Level) const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(Level >= 0 && Level < this->levels());
		GLI_ASSERT(block_size(this->format()) == sizeof(genType));

		return this->Storage->level_size(Level) / sizeof(genType);
	}

	inline void * texture::data()
	{
		return this->Cache.Data;
	}

	inline void const * texture::data() const
	{
		return this->Cache.Data;
	}

	template <typename genType>
	inline genType * texture::data()
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) >= sizeof(genType));

		return reinterpret_cast<genType *>(this->data());
	}

	template <typename genType>
	inline genType const * texture::data() const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) >= sizeof(genType));

		return reinterpret_cast<genType const *>(this->data());
	}

	inline void * texture::data(size_type Layer, size_type Face, size_type Level)
	{
		GLI_ASSERT(Layer >= 0 && Layer < this->layers() && Face >= 0 && Face < this->faces() && Level >= 0 && Level < this->levels());

		size_type const Offset = this->Storage->offset(
			this->base_layer() + Layer, this->base_face() + Face, this->base_level() + Level);

		return this->Storage->data() + Offset;
	}

	inline void const * texture::data(size_type Layer, size_type Face, size_type Level) const
	{
		GLI_ASSERT(Layer >= 0 && Layer < this->layers() && Face >= 0 && Face < this->faces() && Level >= 0 && Level < this->levels());

		size_type const Offset = this->Storage->offset(
			this->base_layer() + Layer, this->base_face() + Face, this->base_level() + Level);

		return this->Storage->data() + Offset;
	}

	template <typename genType>
	inline genType * texture::data(size_type Layer, size_type Face, size_type Level)
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) >= sizeof(genType));
		GLI_ASSERT(Layer >= 0 && Layer < this->layers() && Face >= 0 && Face < this->faces() && Level >= 0 && Level < this->levels());

		return reinterpret_cast<genType *>(this->data(Layer, Face, Level));
	}

	template <typename genType>
	inline genType const * texture::data(size_type Layer, size_type Face, size_type Level) const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) >= sizeof(genType));
		GLI_ASSERT(Layer >= 0 && Layer < this->layers() && Face >= 0 && Face < this->faces() && Level >= 0 && Level < this->levels());

		return reinterpret_cast<genType const *>(this->data(Layer, Face, Level));
	}

	inline texture::texelcoord_type texture::dimensions(size_type Level) const
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(Level >= 0 && Level < this->levels());

		storage::texelcoord_type const & SrcDimensions = this->Storage->dimensions(this->base_level() + Level);
		storage::texelcoord_type const & DstDimensions = SrcDimensions * block_dimensions(this->format()) / this->Storage->block_dimensions();

		return glm::max(DstDimensions, texture::texelcoord_type(1));
	}

	inline void texture::clear()
	{
		GLI_ASSERT(!this->empty());

		memset(this->data(), 0, this->size());
	}

	template <typename genType>
	inline void texture::clear(genType const & Texel)
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) == sizeof(genType));

		genType* Data = this->data<genType>();
		size_type const TexelCount = this->size<genType>();

		for(size_type TexelIndex = 0; TexelIndex < TexelCount; ++TexelIndex)
			*(Data + TexelIndex) = Texel;
	}

	template <typename genType>
	inline void texture::clear(size_type Layer, size_type Face, size_type Level, genType const & Texel)
	{
		GLI_ASSERT(!this->empty());
		GLI_ASSERT(block_size(this->format()) == sizeof(genType));
		GLI_ASSERT(Layer >= 0 && Layer < this->layers() && Face >= 0 && Face < this->faces() && Level >= 0 && Level < this->levels());

		size_type const TexelCount = this->Storage->level_size(Level) / sizeof(genType);
		genType* Data = this->data<genType>(Layer, Face, Level);
		for(size_type TexelIndex = 0; TexelIndex < TexelCount; ++TexelIndex)
			*(Data + TexelIndex) = Texel;
	}

	template <typename genType>
	void texture::swizzle(gli::swizzles const & Swizzles)
	{
		for(size_type TexelIndex = 0, TexelCount = this->size<genType>(); TexelIndex < TexelCount; ++TexelIndex)
		{
			genType & TexelDst = *(this->data<genType>() + TexelIndex);
			genType const TexelSrc = TexelDst;
			for(typename genType::length_type Component = 0; Component < TexelDst.length(); ++Component)
			{
				assert(static_cast<typename genType::length_type>(Swizzles[Component]) < TexelDst.length());
				TexelDst[Component] = TexelSrc[Swizzles[Component]];
			}
		}
	}

	inline void texture::build_cache()
	{
		size_type const Offset = this->Storage->offset(
			this->base_layer(), this->base_face(), this->base_level());

		size_type const Size = this->Storage->layer_size(
			this->base_face(), this->max_face(),
			this->base_level(), this->max_level()) * this->layers();

		this->Cache.Data = this->Storage->data() + Offset;
		this->Cache.Size = Size;
	}

	inline texture::size_type texture::offset(size_type Layer, size_type Face, size_type Level) const
	{
		GLI_ASSERT(Layer >= 0 && Layer < this->layers() && Face >= 0 && Face < this->faces() && Level >= 0 && Level < this->levels());

		return this->Storage->offset(
			this->base_layer() + Layer,
			this->base_face() + Face,
			this->base_level() + Level);
	}
}//namespace gli


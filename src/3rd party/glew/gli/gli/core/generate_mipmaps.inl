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
/// @file gli/generate_mipmaps.inl
/// @date 2015-10-06 / 2015-10-22
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/sampler1d.hpp>
#include <gli/sampler1d_array.hpp>
#include <gli/sampler2d.hpp>
#include <gli/sampler2d_array.hpp>
#include <gli/sampler3d.hpp>
#include <gli/sampler_cube.hpp>
#include <gli/sampler_cube_array.hpp>

namespace gli
{
	inline texture1D generate_mipmaps(
		texture1D const & Texture,
		texture1D::size_type BaseLevel, texture1D::size_type MaxLevel,
		filter Minification)
	{
		fsampler1D Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	inline texture1DArray generate_mipmaps(
		texture1DArray const & Texture,
		texture1DArray::size_type BaseLayer, texture1DArray::size_type MaxLayer,
		texture1DArray::size_type BaseLevel, texture1DArray::size_type MaxLevel,
		filter Minification)
	{
		fsampler1DArray Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseLayer, MaxLayer, BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	inline texture2D generate_mipmaps(
		texture2D const & Texture,
		texture2D::size_type BaseLevel, texture2D::size_type MaxLevel,
		filter Minification)
	{
		fsampler2D Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	inline texture2DArray generate_mipmaps(
		texture2DArray const & Texture,
		texture2DArray::size_type BaseLayer, texture2DArray::size_type MaxLayer,
		texture2DArray::size_type BaseLevel, texture2DArray::size_type MaxLevel,
		filter Minification)
	{
		fsampler2DArray Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseLayer, MaxLayer, BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	inline texture3D generate_mipmaps(
		texture3D const & Texture,
		texture3D::size_type BaseLevel, texture3D::size_type MaxLevel,
		filter Minification)
	{
		fsampler3D Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	inline textureCube generate_mipmaps(
		textureCube const & Texture,
		textureCube::size_type BaseFace, textureCube::size_type MaxFace,
		textureCube::size_type BaseLevel, textureCube::size_type MaxLevel,
		filter Minification)
	{
		fsamplerCube Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseFace, MaxFace, BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	inline textureCubeArray generate_mipmaps(
		textureCubeArray const & Texture,
		textureCubeArray::size_type BaseLayer, textureCubeArray::size_type MaxLayer,
		textureCubeArray::size_type BaseFace, textureCubeArray::size_type MaxFace,
		textureCubeArray::size_type BaseLevel, textureCubeArray::size_type MaxLevel,
		filter Minification)
	{
		fsamplerCubeArray Sampler(Texture, WRAP_CLAMP_TO_EDGE);
		Sampler.generate_mipmaps(BaseLayer, MaxLayer, BaseFace, MaxFace, BaseLevel, MaxLevel, Minification);
		return Sampler();
	}

	template <>
	inline texture1D generate_mipmaps<texture1D>(texture1D const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_level(), Texture.max_level(), Minification);
	}

	template <>
	inline texture1DArray generate_mipmaps<texture1DArray>(texture1DArray const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_layer(), Texture.max_layer(), Texture.base_level(), Texture.max_level(), Minification);
	}

	template <>
	inline texture2D generate_mipmaps<texture2D>(texture2D const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_level(), Texture.max_level(), Minification);
	}

	template <>
	inline texture2DArray generate_mipmaps<texture2DArray>(texture2DArray const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_layer(), Texture.max_layer(), Texture.base_level(), Texture.max_level(), Minification);
	}

	template <>
	inline texture3D generate_mipmaps<texture3D>(texture3D const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_level(), Texture.max_level(), Minification);
	}

	template <>
	inline textureCube generate_mipmaps<textureCube>(textureCube const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_face(), Texture.max_face(), Texture.base_level(), Texture.max_level(), Minification);
	}

	template <>
	inline textureCubeArray generate_mipmaps<textureCubeArray>(textureCubeArray const & Texture, filter Minification)
	{
		return generate_mipmaps(Texture, Texture.base_layer(), Texture.max_layer(), Texture.base_face(), Texture.max_face(), Texture.base_level(), Texture.max_level(), Minification);
	}
}//namespace gli

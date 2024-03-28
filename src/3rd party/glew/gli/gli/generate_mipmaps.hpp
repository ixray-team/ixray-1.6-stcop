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
/// @file gli/generate_mipmaps.hpp
/// @date 2015-10-06 / 2015-10-06
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "texture1d.hpp"
#include "texture1d_array.hpp"
#include "texture2d.hpp"
#include "texture2d_array.hpp"
#include "texture3d.hpp"
#include "texture_cube.hpp"
#include "texture_cube_array.hpp"
#include "filter.hpp"

namespace gli
{
	/// Allocate a texture and generate all the mipmaps of the texture using the Minification filter.
	template <typename texture_type>
	texture_type generate_mipmaps(texture_type const & Texture, filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLevel to the MaxLevel included using the Minification filter.
	texture1D generate_mipmaps(
		texture1D const & Texture,
		texture1D::size_type BaseLevel, texture1D::size_type MaxLevel,
		filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLayer to the MaxLayer and from the BaseLevel to the MaxLevel included levels using the Minification filter.
	texture1DArray generate_mipmaps(
		texture1DArray const & Texture,
		texture1DArray::size_type BaseLayer, texture1DArray::size_type MaxLayer,
		texture1DArray::size_type BaseLevel, texture1DArray::size_type MaxLevel,
		filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLevel to the MaxLevel included using the Minification filter.
	texture2D generate_mipmaps(
		texture2D const & Texture,
		texture2D::size_type BaseLevel, texture2D::size_type MaxLevel,
		filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLayer to the MaxLayer and from the BaseLevel to the MaxLevel included levels using the Minification filter.
	texture2DArray generate_mipmaps(
		texture2DArray const & Texture,
		texture2DArray::size_type BaseLayer, texture2DArray::size_type MaxLayer,
		texture2DArray::size_type BaseLevel, texture2DArray::size_type MaxLevel,
		filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLevel to the MaxLevel included using the Minification filter.
	texture3D generate_mipmaps(
		texture3D const & Texture,
		texture3D::size_type BaseLevel, texture3D::size_type MaxLevel,
		filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLayer to the MaxLayer, from the BaseFace to the MaxFace and from the BaseLevel to the MaxLevel included levels using the Minification filter.
	textureCube generate_mipmaps(
		textureCube const & Texture,
		textureCube::size_type BaseFace, textureCube::size_type MaxFace,
		textureCube::size_type BaseLevel, textureCube::size_type MaxLevel,
		filter Minification);

	/// Allocate a texture and generate the mipmaps of the texture from the BaseLayer to the MaxLayer and from the BaseLevel to the MaxLevel included levels using the Minification filter.
	textureCubeArray generate_mipmaps(
		textureCubeArray const & Texture,
		textureCubeArray::size_type BaseLayer, textureCubeArray::size_type MaxLayer,
		textureCubeArray::size_type BaseFace, textureCubeArray::size_type MaxFace,
		textureCubeArray::size_type BaseLevel, textureCubeArray::size_type MaxLevel,
		filter Minification);
}//namespace gli

#include "./core/generate_mipmaps.inl"

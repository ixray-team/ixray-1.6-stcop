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
/// @file gli/sampler_cube.hpp
/// @date 2015-10-26 / 2015-10-26
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "sampler.hpp"
#include "texture_cube.hpp"
#include "core/mipmaps_compute.hpp"
#include "core/convert.hpp"

namespace gli
{
	template <typename T, precision P = defaultp>
	class samplerCube : public sampler
	{
	private:
		typedef typename detail::interpolate<T>::type interpolate_type;

	public:
		typedef textureCube texture_type;
		typedef typename texture_type::size_type size_type;
		typedef typename texture_type::texelcoord_type texelcoord_type;
		typedef interpolate_type level_type;
		typedef tvec2<interpolate_type, P> samplecoord_type;
		typedef tvec4<T, P> texel_type;

		samplerCube(texture_type const & Texture, wrap Wrap, filter Mip = FILTER_NEAREST, filter Min = FILTER_NEAREST, texel_type const & BorderColor = texel_type(0, 0, 0, 1));

		/// Access the sampler texture object
		texture_type const & operator()() const;

		/// Fetch a texel from the sampler texture
		texel_type texel_fetch(texelcoord_type const & TexelCoord, size_type Face, size_type Level) const;

		/// Write a texel in the sampler texture
		void texel_write(texelcoord_type const & TexelCoord, size_type Face, size_type Level, texel_type const & Texel);

		/// Clear the sampler texture with a uniform texel
		void clear(texel_type const & Texel);

		/// Sample the sampler texture at a specific level
		texel_type texture_lod(samplecoord_type const & SampleCoord, size_type Face, level_type Level) const;

		/// Generate all the mipmaps of the sampler texture from the texture base level
		void generate_mipmaps(filter Minification);

		/// Generate the mipmaps of the sampler texture from the texture base level to the texture max level included
		void generate_mipmaps(size_type BaseFace, size_type MaxFace, size_type BaseLevel, size_type MaxLevel, filter Minification);

	private:
		typedef typename detail::convert<texture_type, T, P>::func convert_type;
		typedef typename detail::convert<texture_type, T, P>::fetchFunc fetch_type;
		typedef typename detail::convert<texture_type, T, P>::writeFunc write_type;
		typedef typename detail::filterBase<detail::DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>::filterFunc filter_type;

		texture_type Texture;
		convert_type Convert;
		texel_type BorderColor;
		filter_type Filter;
	};

	typedef samplerCube<float> fsamplerCube;
	typedef samplerCube<double> dsamplerCube;
	typedef samplerCube<unsigned int> usamplerCube;
	typedef samplerCube<int> isamplerCube;

}//namespace gli

#include "./core/sampler_cube.inl"

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
/// @file gli/core/filter.hpp
/// @date 2015-10-18 / 2015-10-18
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../filter.hpp"
#include "coord.hpp"

namespace gli{
namespace detail
{
	template <typename T>
	struct interpolate
	{
		typedef float type;
	};

	template <>
	struct interpolate<double>
	{
		typedef double type;
	};

	template <>
	struct interpolate<long double>
	{
		typedef long double type;
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct filterBase
	{
		typedef typename texture_type::size_type size_type;
		typedef typename texture_type::texelcoord_type texelcoord_type;

		typedef texel_type(*filterFunc)(
			texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap,
			size_type Layer, size_type Face, interpolate_type Level,
			texel_type const & BorderColor);
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, bool is_float = true, bool support_border = true>
	struct nearest : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_nearest<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			texelcoord_type const TexelDim(Texture.dimensions(LevelIndex));
			samplecoord_type const TexelLast(samplecoord_type(TexelDim) - samplecoord_type(1));
			texelcoord_type const TexelCoord = texelcoord_type(round(SampleCoordWrap * TexelLast));
			typename texelcoord_type::bool_type const UseTexelCoord = in_interval(TexelCoord, texelcoord_type(0), TexelDim - 1);

			texel_type Texel(BorderColor);
			if(all(UseTexelCoord))
				Texel = Fetch(Texture, TexelCoord, Layer, Face, LevelIndex);

			return Texel;
		}
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct nearest<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, false> : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_nearest<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			samplecoord_type const TexelLast(samplecoord_type(Texture.dimensions(LevelIndex)) - samplecoord_type(1));
			texelcoord_type const TexelCoord = texelcoord_type(round(SampleCoordWrap * TexelLast));

			return Fetch(Texture, TexelCoord, Layer, Face, LevelIndex);
		}
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, bool is_float = true, bool support_border = true>
	struct linear : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			return texel_type(0);
		}
	};

	template <typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct linear<DIMENSION_1D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, true> : public filterBase<DIMENSION_1D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<DIMENSION_1D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			coord_type const & Coord = make_coord_linear_border(Texture.dimensions(LevelIndex), SampleCoordWrap);

			texel_type Texel0(BorderColor);
			if(Coord.UseTexelFloor.s)
				Texel0 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s), Layer, Face, LevelIndex);

			texel_type Texel1(BorderColor);
			if(Coord.UseTexelCeil.s)
				Texel1 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s), Layer, Face, LevelIndex);

			return mix(Texel0, Texel1, Coord.Blend.s);
		}
	};

	template <typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct linear<DIMENSION_1D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, false> : public filterBase<DIMENSION_1D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<DIMENSION_1D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			coord_type const & Coord = make_coord_linear(Texture.dimensions(LevelIndex), SampleCoordWrap);

			texel_type const Texel0 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s), Layer, Face, LevelIndex);
			texel_type const Texel1 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s), Layer, Face, LevelIndex);

			return mix(Texel0, Texel1, Coord.Blend.s);
		}
	};

	template <typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct linear<DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, true> : public filterBase<DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<typename texture_type::size_type>(Level);
			coord_type const & Coord = make_coord_linear_border(Texture.dimensions(LevelIndex), SampleCoordWrap);

			texel_type Texel00(BorderColor);
			if(Coord.UseTexelFloor.s && Coord.UseTexelFloor.t)
				Texel00 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t), Layer, Face, LevelIndex);

			texel_type Texel10(BorderColor);
			if(Coord.UseTexelCeil.s && Coord.UseTexelFloor.t)
				Texel10 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t), Layer, Face, LevelIndex);

			texel_type Texel11(BorderColor);
			if(Coord.UseTexelCeil.s && Coord.UseTexelCeil.t)
				Texel11 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t), Layer, Face, LevelIndex);

			texel_type Texel01(BorderColor);
			if(Coord.UseTexelFloor.s && Coord.UseTexelCeil.t)
				Texel01 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t), Layer, Face, LevelIndex);

			texel_type const ValueA(mix(Texel00, Texel10, Coord.Blend.s));
			texel_type const ValueB(mix(Texel01, Texel11, Coord.Blend.s));
			return mix(ValueA, ValueB, Coord.Blend.t);
		}
	};

	template <typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct linear<DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, false> : public filterBase<DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<DIMENSION_2D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			coord_type const & Coord = make_coord_linear_border(Texture.dimensions(LevelIndex), SampleCoordWrap);

			texel_type const & Texel00 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t), Layer, Face, LevelIndex);
			texel_type const & Texel10 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t), Layer, Face, LevelIndex);
			texel_type const & Texel11 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t), Layer, Face, LevelIndex);
			texel_type const & Texel01 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t), Layer, Face, LevelIndex);
/*
			texel_type const & Texel00 = Texture.load<vec4>(typename texture_type::texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t), LevelIndex);
			texel_type const & Texel10 = Texture.load<vec4>(typename texture_type::texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t), LevelIndex);
			texel_type const & Texel11 = Texture.load<vec4>(typename texture_type::texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t), LevelIndex);
			texel_type const & Texel01 = Texture.load<vec4>(typename texture_type::texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t), LevelIndex);
*/
			texel_type const ValueA(mix(Texel00, Texel10, Coord.Blend.s));
			texel_type const ValueB(mix(Texel01, Texel11, Coord.Blend.s));
			return mix(ValueA, ValueB, Coord.Blend.t);
		}
	};

	template <typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct linear<DIMENSION_3D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, true> : public filterBase<DIMENSION_3D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<DIMENSION_3D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			coord_type const & Coord = make_coord_linear_border(Texture.dimensions(LevelIndex), SampleCoordWrap);

			texel_type Texel000(BorderColor);
			if(Coord.UseTexelFloor.s && Coord.UseTexelFloor.t && Coord.UseTexelFloor.p)
				Texel000 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);

			texel_type Texel100(BorderColor);
			if(Coord.UseTexelCeil.s && Coord.UseTexelFloor.t && Coord.UseTexelFloor.p)
				Texel100 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);

			texel_type Texel110(BorderColor);
			if(Coord.UseTexelCeil.s && Coord.UseTexelCeil.t && Coord.UseTexelFloor.p)
				Texel110 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);

			texel_type Texel010(BorderColor);
			if(Coord.UseTexelFloor.s && Coord.UseTexelCeil.t && Coord.UseTexelFloor.p)
				Texel010 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);

			texel_type Texel001(BorderColor);
			if (Coord.UseTexelFloor.s && Coord.UseTexelFloor.t && Coord.UseTexelCeil.p)
				Texel001 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);

			texel_type Texel101(BorderColor);
			if(Coord.UseTexelCeil.s && Coord.UseTexelFloor.t && Coord.UseTexelCeil.p)
				Texel101 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);

			texel_type Texel111(BorderColor);
			if(Coord.UseTexelCeil.s && Coord.UseTexelCeil.t && Coord.UseTexelCeil.p)
				Texel111 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);

			texel_type Texel011(BorderColor);
			if(Coord.UseTexelFloor.s && Coord.UseTexelCeil.t && Coord.UseTexelCeil.p)
				Texel011 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);

			texel_type const ValueA(mix(Texel000, Texel100, Coord.Blend.s));
			texel_type const ValueB(mix(Texel010, Texel110, Coord.Blend.s));

			texel_type const ValueC(mix(Texel001, Texel101, Coord.Blend.s));
			texel_type const ValueD(mix(Texel011, Texel111, Coord.Blend.s));

			texel_type const ValueE(mix(ValueA, ValueB, Coord.Blend.t));
			texel_type const ValueF(mix(ValueC, ValueD, Coord.Blend.t));

			return mix(ValueE, ValueF, Coord.Blend.p);
		}
	};

	template <typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type>
	struct linear<DIMENSION_3D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, true, false> : public filterBase<DIMENSION_3D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<DIMENSION_3D, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			size_type const LevelIndex = static_cast<size_type>(Level);
			coord_type const & Coord = make_coord_linear(Texture.dimensions(LevelIndex), SampleCoordWrap);

			texel_type const & Texel000 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);
			texel_type const & Texel100 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);
			texel_type const & Texel110 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);
			texel_type const & Texel010 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t, Coord.TexelFloor.p), Layer, Face, LevelIndex);
			texel_type const & Texel001 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelFloor.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);
			texel_type const & Texel101 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelFloor.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);
			texel_type const & Texel111 = Fetch(Texture, texelcoord_type(Coord.TexelCeil.s, Coord.TexelCeil.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);
			texel_type const & Texel011 = Fetch(Texture, texelcoord_type(Coord.TexelFloor.s, Coord.TexelCeil.t, Coord.TexelCeil.p), Layer, Face, LevelIndex);

			texel_type const ValueA(mix(Texel000, Texel100, Coord.Blend.s));
			texel_type const ValueB(mix(Texel010, Texel110, Coord.Blend.s));

			texel_type const ValueC(mix(Texel001, Texel101, Coord.Blend.s));
			texel_type const ValueD(mix(Texel011, Texel111, Coord.Blend.s));

			texel_type const ValueE(mix(ValueA, ValueB, Coord.Blend.t));
			texel_type const ValueF(mix(ValueC, ValueD, Coord.Blend.t));

			return mix(ValueE, ValueF, Coord.Blend.p);
		}
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, bool is_float, bool support_border>
	struct nearest_mipmap_nearest : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			return nearest<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, is_float, support_border>::call(Texture, Fetch, SampleCoordWrap, Layer, Face, round(Level), BorderColor);
		}
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, bool is_float, bool support_border>
	struct nearest_mipmap_linear : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			texel_type const MinTexel = nearest<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, is_float, support_border>::call(Texture, Fetch, SampleCoordWrap, Layer, Face, floor(Level), BorderColor);
			texel_type const MaxTexel = nearest<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, is_float, support_border>::call(Texture, Fetch, SampleCoordWrap, Layer, Face, ceil(Level), BorderColor);
			return mix(MinTexel, MaxTexel, fract(Level));
		}
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, bool is_float, bool support_border>
	struct linear_mipmap_nearest : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			return linear<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, is_float, support_border>::call(Texture, Fetch, SampleCoordWrap, Layer, Face, round(Level), BorderColor);
		}
	};

	template <dimension Dimension, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, bool is_float, bool support_border>
	struct linear_mipmap_linear : public filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type>
	{
		typedef filterBase<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type> base_type;
		typedef typename base_type::size_type size_type;
		typedef typename base_type::texelcoord_type texelcoord_type;
		typedef coord_linear_border<texelcoord_type, samplecoord_type> coord_type;

		static texel_type call(texture_type const & Texture, fetch_type Fetch, samplecoord_type const & SampleCoordWrap, size_type Layer, size_type Face, interpolate_type Level, texel_type const & BorderColor)
		{
			texel_type const MinTexel = linear<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, is_float, support_border>::call(Texture, Fetch, SampleCoordWrap, Layer, Face, floor(Level), BorderColor);
			texel_type const MaxTexel = linear<Dimension, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, is_float, support_border>::call(Texture, Fetch, SampleCoordWrap, Layer, Face, ceil(Level), BorderColor);
			return mix(MinTexel, MaxTexel, fract(Level));
		}
	};

	template <typename filter_type, dimension Dimensions, typename texture_type, typename interpolate_type, typename samplecoord_type, typename fetch_type, typename texel_type, typename T>
	inline filter_type get_filter(filter Mip, filter Min, bool Border)
	{
		static filter_type Table[][FILTER_COUNT][2] =
		{
			{
				{
					nearest_mipmap_nearest<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, false>::call,
					nearest_mipmap_nearest<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, true>::call
				},
				{
					linear_mipmap_nearest<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, false>::call,
					linear_mipmap_nearest<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, true>::call
				}
			},
			{
				{
					nearest_mipmap_linear<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, false>::call,
					nearest_mipmap_linear<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, true>::call
				},
				{
					linear_mipmap_linear<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, false>::call,
					linear_mipmap_linear<Dimensions, texture_type, interpolate_type, samplecoord_type, fetch_type, texel_type, std::numeric_limits<T>::is_iec559, true>::call
				}
			}
		};
		static_assert(sizeof(Table) / sizeof(Table[0]) == FILTER_COUNT, "GLI ERROR: 'Table' doesn't match the number of supported filters");

		GLI_ASSERT(Table[Mip - FILTER_FIRST][Min - FILTER_FIRST][Border ? 1 : 0]);

		return Table[Mip - FILTER_FIRST][Min - FILTER_FIRST][Border ? 1 : 0];
	}
}//namespace detail
}//namespace gli


//////////////////////////////////////////////////////////////////////////////////
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
/// @file gli/core/convert_sampler2d_array.cpp
/// @date 2015-10-05 / 2015-10-05
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/sampler2d_array.hpp>
#include <glm/gtc/epsilon.hpp>

namespace
{
	template <typename sampler_value_type, typename texture_value_type, template <typename, gli::precision> class vec_type, gli::detail::convertMode mode>
	struct norm
	{
		typedef gli::detail::convertFunc<gli::texture2DArray, sampler_value_type, texture_value_type, gli::defaultp, vec_type, mode, std::numeric_limits<sampler_value_type>::is_iec559> convert;

		static int run(gli::format Format, gli::tvec4<sampler_value_type, gli::defaultp> const & Color)
		{
			gli::texture2DArray Texture(Format, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::tvec4<sampler_value_type, gli::defaultp> const & Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			return gli::all(gli::epsilonEqual(Texel, Color, static_cast<sampler_value_type>(0.01))) ? 0 : 1;
		}
	};
}//namespace

namespace rgba16sf
{
	int test()
	{
		int Error = 0;

		gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, gli::u16, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_HALF, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_R16_SFLOAT_PACK16, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2D::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2D::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, gli::u16, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_HALF, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RG16_SFLOAT_PACK16, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, gli::u16, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_HALF, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGB16_SFLOAT_PACK16, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2D::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2D::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, gli::u16, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_HALF, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGBA16_SFLOAT_PACK16, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba16sf

namespace rgba32sf
{
	int test()
	{
		int Error = 0;

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, float, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::vec4 const Color(1.0f, 0.0f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, float, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::dvec4 const Color(1.0f, 0.0f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, float, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RG32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, float, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RG32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, float, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGB32_SFLOAT_PACK32, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, float, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGB32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, float, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGBA32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, float, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGBA32_SFLOAT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01)) ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba32sf

namespace rgba64sf
{
	int test()
	{
		int Error = 0;

		gli::vec4 const ColorF32(1.0f, 0.5f, 0.0f, 1.0f);
		gli::dvec4 const ColorF64(1.0f, 0.5f, 0.0f, 1.0f);

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, double, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::vec4 const Color(1.0f, 0.0f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R64_SFLOAT_PACK64, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, double, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::dvec4 const Color(1.0f, 0.0f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R64_SFLOAT_PACK64, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2D::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, Color, 0.01)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, double, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RG64_SFLOAT_PACK64, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorF32);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, ColorF32, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, double, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RG64_SFLOAT_PACK64, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorF64);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, ColorF64, 0.01)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, double, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGB64_SFLOAT_PACK64, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorF32);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, ColorF32, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, double, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGB64_SFLOAT_PACK64, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorF64);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, ColorF64, 0.01)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, float, double, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGBA64_SFLOAT_PACK64, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorF32);
			gli::vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, ColorF32, 0.01f)) ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, double, double, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_CAST, true> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGBA64_SFLOAT_PACK64, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorF64);
			gli::dvec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::all(gli::epsilonEqual(Texel, ColorF64, 0.01)) ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba64sf

namespace rgba8_int
{
	int test()
	{
		int Error = 0;

		gli::u8vec4 const ColorU8(255, 127, 0, 255);
		gli::i8vec4 const ColorI8(127, 63, 0,-128);

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, gli::u32, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_R8_UINT_PACK8, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorU8);
			gli::u8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel.x == ColorU8.x ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::i32, glm::i8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_R8_SINT_PACK8, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorI8);
			gli::i8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel.x == ColorI8.x ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::u8, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_RG8_UINT_PACK8, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorU8);
			glm::u8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel.x == ColorU8.x ? 0 : 1;
			Error += Texel.y == ColorU8.y ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::i8, glm::i8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_RG8_SINT_PACK8, gli::texture2D::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorI8);
			glm::i8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel.x == ColorI8.x ? 0 : 1;
			Error += Texel.y == ColorI8.y ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::u8, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGB8_UINT_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorU8);
			glm::u8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel.x == ColorU8.x ? 0 : 1;
			Error += Texel.y == ColorU8.y ? 0 : 1;
			Error += Texel.z == ColorU8.z ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::i8, glm::i8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGB8_SINT_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorI8);
			glm::i8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel.x == ColorI8.x ? 0 : 1;
			Error += Texel.y == ColorI8.y ? 0 : 1;
			Error += Texel.z == ColorI8.z ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::u8, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGBA8_UINT_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorU8);
			glm::u8vec4 Texel =convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel == ColorU8 ? 0 : 1;
		}

		{
			typedef gli::detail::convertFunc<gli::texture2DArray, glm::i8, glm::i8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_CAST, false> convert;

			gli::texture2DArray Texture(gli::FORMAT_RGBA8_SINT_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			convert::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, ColorI8);
			glm::i8vec4 Texel = convert::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel == ColorI8 ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba8_int

namespace rgba_16packed
{
	int test()
	{
		int Error = 0;

		gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);

		{
			gli::texture2DArray Texture(gli::FORMAT_RG3B2_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_332UNORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_332UNORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 1.f / 7.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 1.f / 7.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 1.f / 3.f) ? 0 : 1;
		}

		{
			gli::texture2DArray Texture(gli::FORMAT_RG4_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_44UNORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_44UNORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 1.f / 15.f) ? 0 : 1;
		}

		{
			gli::texture2DArray Texture(gli::FORMAT_RGBA4_UNORM_PACK16, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_4444UNORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_4444UNORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.w, Color.w, 1.f / 15.f) ? 0 : 1;
		}

		{
			gli::texture2DArray Texture(gli::FORMAT_R5G6B5_UNORM_PACK16, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_565UNORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_565UNORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 1.f / 31.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 1.f / 31.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 1.f / 31.f) ? 0 : 1;
		}

		{
			gli::texture2DArray Texture(gli::FORMAT_RGB5A1_UNORM_PACK16, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_5551UNORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_5551UNORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 1.f / 15.f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.w, Color.w, 0.01f) ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba_16packed

namespace rgba8_norm
{
	int test()
	{
		int Error = 0;

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
		}

		{
			gli::vec4 const Color(255.f, 127.f, 0.f, 255.f);
			gli::texture2DArray Texture(gli::FORMAT_R8_USCALED_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(255.f, 127.f, 0.f, 255.f);
			gli::texture2DArray Texture(gli::FORMAT_R8_USCALED_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_CAST, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R8_SNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R8_SNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::i8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::i8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RG8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RG8_SNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGB8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01f) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGB8_SNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01f) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.w, Color.w, 0.01f) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGBA8_SNORM_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_NORM, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			gli::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::i8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_NORM, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.w, Color.w, 0.01f) ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba8_norm

namespace rgba8_srgb
{
	int test()
	{
		int Error = 0;

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_R8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec1, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RG8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RG8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec2, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGB8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01f) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGB8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec3, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01) ? 0 : 1;
		}

		{
			gli::vec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGBA8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::vec4 Texel = gli::detail::convertFunc<gli::texture2DArray, float, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01f) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.w, Color.w, 0.01f) ? 0 : 1;
		}

		{
			gli::dvec4 const Color(1.0f, 0.5f, 0.0f, 1.0f);
			gli::texture2DArray Texture(gli::FORMAT_RGBA8_SRGB_PACK8, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_SRGB, true>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::dvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, double, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_SRGB, true>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += gli::epsilonEqual(Texel.x, Color.x, 0.01) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.y, Color.y, 0.01) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.z, Color.z, 0.01) ? 0 : 1;
			Error += gli::epsilonEqual(Texel.w, Color.w, 0.01) ? 0 : 1;
		}

		return Error;
	}
}//namespace rgba8_srgb

namespace rgb9e5
{
	int test()
	{
		int Error = 0;

		Error += norm<float, glm::u8, gli::tvec3, gli::detail::CONVERT_MODE_RGB9E5>::run(gli::FORMAT_RGB9E5_UFLOAT_PACK32, gli::vec4(1.0f, 0.5f, 0.0f, 1.0f));
		Error += norm<double, glm::u8, gli::tvec3, gli::detail::CONVERT_MODE_RGB9E5>::run(gli::FORMAT_RGB9E5_UFLOAT_PACK32, gli::dvec4(1.0f, 0.5f, 0.0f, 1.0f));

		return Error;
	}
}//namespace rgb9e5

namespace rg11b10f
{
	int test()
	{
		int Error = 0;

		Error += norm<float, glm::u32, gli::tvec3, gli::detail::CONVERT_MODE_RG11B10F>::run(gli::FORMAT_RG11B10_UFLOAT_PACK32, gli::vec4(1.0f, 0.5f, 0.0f, 1.0f));
		Error += norm<double, glm::u32, gli::tvec3, gli::detail::CONVERT_MODE_RG11B10F>::run(gli::FORMAT_RG11B10_UFLOAT_PACK32, gli::dvec4(1.0f, 0.5f, 0.0f, 1.0f));

		return Error;
	}
}//namespace rg11b10f

namespace rgb10a2norm
{
	int test()
	{
		int Error = 0;

		Error += norm<float, glm::u8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2UNORM>::run(gli::FORMAT_RGB10A2_UNORM_PACK32, gli::vec4(1.0f, 0.5f, 0.0f, 1.0f));
		Error += norm<float, glm::i8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2SNORM>::run(gli::FORMAT_RGB10A2_SNORM_PACK32, gli::vec4(1.0f, 0.5f, 0.0f, 1.0f));
		Error += norm<float, glm::u8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2USCALE>::run(gli::FORMAT_RGB10A2_USCALED_PACK32, gli::vec4(1023.f, 511.f, 0.f, 3.0f));
		Error += norm<float, glm::i8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2SSCALE>::run(gli::FORMAT_RGB10A2_SSCALED_PACK32, gli::vec4(511.f, 255.f, 0.f, 1.0f));
		Error += norm<double, glm::u8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2UNORM>::run(gli::FORMAT_RGB10A2_UNORM_PACK32, gli::dvec4(1.0f, 0.5f, 0.0f, 1.0f));
		Error += norm<double, glm::i8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2SNORM>::run(gli::FORMAT_RGB10A2_SNORM_PACK32, gli::dvec4(1.0f, 0.5f, 0.0f, 1.0f));
		Error += norm<double, glm::u8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2USCALE>::run(gli::FORMAT_RGB10A2_USCALED_PACK32, gli::dvec4(1023.f, 511.f, 0.f, 3.0f));
		Error += norm<double, glm::i8, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2SSCALE>::run(gli::FORMAT_RGB10A2_SSCALED_PACK32, gli::dvec4(511.f, 255.f, 0.f, 1.0f));

		return Error;
	}
}//namespace rgb10a2norm

namespace rgb10a2int
{
	int test()
	{
		int Error = 0;

		{
			gli::uvec4 const Color(255, 127, 0, 3);
			gli::texture2DArray Texture(gli::FORMAT_RGB10A2_UINT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, gli::uint, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2UINT, false>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::uvec4 Texel = gli::detail::convertFunc<gli::texture2DArray, gli::uint, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2UINT, false>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel == Color ? 0 : 1;
		}

		{
			gli::ivec4 const Color(127, 63, 0, 1);
			gli::texture2DArray Texture(gli::FORMAT_RGB10A2_SINT_PACK32, gli::texture2DArray::texelcoord_type(1), 1, 1);
			gli::detail::convertFunc<gli::texture2DArray, gli::sint, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2SINT, false>::write(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0, Color);
			glm::ivec4 Texel = gli::detail::convertFunc<gli::texture2DArray, gli::sint, glm::u8, gli::defaultp, gli::tvec4, gli::detail::CONVERT_MODE_RGB10A2SINT, false>::fetch(Texture, gli::texture2DArray::texelcoord_type(0), 0, 0, 0);

			Error += Texel == Color ? 0 : 1;
		}

		return Error;
	}
}//namespace rgb10a2int

int main()
{
	int Error = 0;

	Error += rgba32sf::test();
	Error += rgba64sf::test();
	Error += rgba8_int::test();
	Error += rgba8_norm::test();
	Error += rgba8_srgb::test();
	Error += rgb9e5::test();
	Error += rg11b10f::test();
	Error += rgb10a2norm::test();
	Error += rgb10a2int::test();
	Error += rgba_16packed::test();

	return Error;
}


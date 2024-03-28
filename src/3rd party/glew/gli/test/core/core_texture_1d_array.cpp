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
/// @file test/core/texture_1d_array.cpp
/// @date 2013-01-12 / 2013-01-12
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/gli.hpp>

int test_alloc()
{
	int Error(0);

	std::vector<gli::format> Formats;
	Formats.push_back(gli::FORMAT_RGBA8_UNORM_PACK8);
	Formats.push_back(gli::FORMAT_RGB8_UNORM_PACK8);
	Formats.push_back(gli::FORMAT_R8_SNORM_PACK8);
	Formats.push_back(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8);
	Formats.push_back(gli::FORMAT_RGBA_BP_UNORM_BLOCK16);
	Formats.push_back(gli::FORMAT_RGBA32_SFLOAT_PACK32);

	std::vector<gli::texture1DArray::texelcoord_type> Sizes;
	Sizes.push_back(gli::texture1DArray::texelcoord_type(16));
	Sizes.push_back(gli::texture1DArray::texelcoord_type(32));
	Sizes.push_back(gli::texture1DArray::texelcoord_type(15));
	Sizes.push_back(gli::texture1DArray::texelcoord_type(17));
	Sizes.push_back(gli::texture1DArray::texelcoord_type(1));

	for(gli::size_t FormatIndex = 0; FormatIndex < Formats.size(); ++FormatIndex)
	for(gli::size_t SizeIndex = 0; SizeIndex < Sizes.size(); ++SizeIndex)
	{
		gli::texture1DArray::texelcoord_type Size(Sizes[SizeIndex]);

		gli::texture1DArray TextureA(Formats[FormatIndex], Size, 1);
		gli::texture1DArray TextureB(Formats[FormatIndex], Size, 1);

		Error += TextureA == TextureB ? 0 : 1;
	}

	return Error;
}

int test_texture1DArray_clear()
{
	int Error(0);

	gli::u8vec4 const Orange(255, 127, 0, 255);

	gli::texture1DArray::texelcoord_type const Size(16);

	gli::texture1DArray Texture(gli::FORMAT_RGBA8_UINT_PACK8, Size, 2);

	Texture.clear<glm::u8vec4>(Orange);

	return Error;
}

int test_texture1DArray_query()
{
	int Error(0);

	gli::texture1DArray::size_type Layers(2);
	gli::texture1DArray::size_type Levels(2);

	gli::texture1DArray Texture(gli::FORMAT_RGBA8_UINT_PACK8, gli::texture1DArray::texelcoord_type(2), Layers, Levels);

	gli::texture1DArray::size_type Size = Texture.size();

	Error += Size == sizeof(glm::u8vec4) * 3 * Layers ? 0 : 1;
	Error += Texture.format() == gli::FORMAT_RGBA8_UINT_PACK8 ? 0 : 1;
	Error += Texture.layers() == Layers ? 0 : 1;
	Error += Texture.levels() == Levels ? 0 : 1;
	Error += !Texture.empty() ? 0 : 1;
	Error += Texture.dimensions() == gli::texture1DArray::texelcoord_type(2) ? 0 : 1;

	return Error;
}

int test_texture1DArray_access()
{
	int Error(0);

	{
		gli::texture1DArray Texture1DArray(gli::FORMAT_RGBA8_UINT_PACK8, gli::texture1DArray::texelcoord_type(2), 2, 1);
		assert(!Texture1DArray.empty());

		gli::texture1D Texture0 = Texture1DArray[0];
		gli::texture1D Texture1 = Texture1DArray[1];
		
		std::size_t Size0 = Texture0.size();
		std::size_t Size1 = Texture1.size();

		Error += Size0 == sizeof(glm::u8vec4) * 2 ? 0 : 1;
		Error += Size1 == sizeof(glm::u8vec4) * 2 ? 0 : 1;

		for(std::size_t i = 0; i < 2; ++i)
		{
			*(Texture0.data<glm::u8vec4>() + i) = glm::u8vec4(255, 127, 0, 255);
			*(Texture1.data<glm::u8vec4>() + i) = glm::u8vec4(0, 127, 255, 255);
		}

		glm::u8vec4 * PointerA = Texture0.data<glm::u8vec4>();
		glm::u8vec4 * PointerB = Texture1.data<glm::u8vec4>();

		glm::u8vec4 * Pointer0 = Texture1DArray.data<glm::u8vec4>() + 0;
		glm::u8vec4 * Pointer1 = Texture1DArray.data<glm::u8vec4>() + 2;

		Error += PointerA == Pointer0 ? 0 : 1;
		Error += PointerB == Pointer1 ? 0 : 1;

		glm::u8vec4 ColorA = *Texture0.data<glm::u8vec4>();
		glm::u8vec4 ColorB = *Texture1.data<glm::u8vec4>();

		glm::u8vec4 Color0 = *Pointer0;
		glm::u8vec4 Color1 = *Pointer1;

		Error += ColorA == Color0 ? 0 : 1;
		Error += ColorB == Color1 ? 0 : 1;

		Error += glm::all(glm::equal(Color0, glm::u8vec4(255, 127, 0, 255))) ? 0 : 1;
		Error += glm::all(glm::equal(Color1, glm::u8vec4(0, 127, 255, 255))) ? 0 : 1;
	}

	return Error;
}

struct test
{
	test(
		gli::format const & Format,
		gli::texture1DArray::texelcoord_type const & Dimensions,
		gli::texture1DArray::size_type const & Size) :
		Format(Format),
		Dimensions(Dimensions),
		Size(Size)
	{}

	gli::format Format;
	gli::texture1DArray::texelcoord_type Dimensions;
	gli::texture1DArray::size_type Size;
};

int test_texture1DArray_size()
{
	int Error(0);

	std::vector<test> Tests;
	Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, gli::texture1DArray::texelcoord_type(4), 32));
	Tests.push_back(test(gli::FORMAT_R8_UINT_PACK8, gli::texture1DArray::texelcoord_type(4), 8));

	for(std::size_t i = 0; i < Tests.size(); ++i)
	{
		gli::texture1DArray Texture1DArray(Tests[i].Format, gli::texture1DArray::texelcoord_type(4), 2, 1);

		Error += Texture1DArray.size() == Tests[i].Size ? 0 : 1;
		assert(!Error);
	}

	for(std::size_t i = 0; i < Tests.size(); ++i)
	{
		gli::texture1DArray Texture1DArray(Tests[i].Format, gli::texture1DArray::texelcoord_type(4), 2, 1);

		gli::texture1D Texture1D = Texture1DArray[0];

		Error += Texture1DArray.size() == Tests[i].Size ? 0 : 1;
		assert(!Error);
	}

	return Error;
}

namespace load_store
{
	template <typename genType>
	int run(gli::format Format, std::array<genType, 8> const & TestSamples)
	{
		int Error = 0;

		gli::texture1DArray::texelcoord_type const Dimensions(16);
		std::array<gli::texture1D::texelcoord_type, 8> TexelCoord;
		for (gli::size_t i = 0, n = 8; i < n; ++i)
			TexelCoord[i] = gli::texture1D::texelcoord_type(static_cast<int>(i));

		gli::texture1DArray TextureA(Format, Dimensions, 3);
		TextureA.clear();
		for(gli::size_t i = 0, n = 8; i < n; ++i)
			*(TextureA.data<genType>(2, 0, 1) + i) = TestSamples[i];

		gli::texture1DArray TextureB(Format, Dimensions, 3);
		TextureB.clear();
		for(gli::size_t i = 0, n = 8; i < n; ++i)
			TextureB.store(TexelCoord[i], 2, 1, TestSamples[i]);

		std::array<genType, 8> LoadedSamplesA;
		for(gli::size_t i = 0, n = 8; i < n; ++i)
			LoadedSamplesA[i] = TextureA.load<genType>(TexelCoord[i], 2, 1);

		std::array<genType, 8> LoadedSamplesB;
		for(gli::size_t i = 0, n = 8; i < n; ++i)
			LoadedSamplesB[i] = TextureB.load<genType>(TexelCoord[i], 2, 1);

		for(gli::size_t i = 0, n = 8; i < n; ++i)
			Error += LoadedSamplesA[i] == TestSamples[i] ? 0 : 1;

		for(gli::size_t i = 0, n = 8; i < n; ++i)
			Error += LoadedSamplesB[i] == TestSamples[i] ? 0 : 1;

		Error += TextureA == TextureB ? 0 : 1;

		gli::texture1DArray TextureC(TextureA, 2, 2, 1, 1);
		gli::texture1DArray TextureD(TextureB, 2, 2, 1, 1);

		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}

	int test()
	{
		int Error = 0;

		{
			std::array<glm::f32vec1, 8> TestSamples{
			{
				glm::f32vec1(0.0f),
				glm::f32vec1(1.0f),
				glm::f32vec1(-1.0f),
				glm::f32vec1(0.5f),
				glm::f32vec1(-0.5f),
				glm::f32vec1(0.2f),
				glm::f32vec1(-0.2f),
				glm::f32vec1(0.9f)
			}};

			Error += run(gli::FORMAT_R32_SFLOAT_PACK32, TestSamples);
		}

		{
			std::array<glm::f32vec2, 8> TestSamples{
			{
				glm::f32vec2(-1.0f,-1.0f),
				glm::f32vec2(-0.5f,-0.5f),
				glm::f32vec2(0.0f, 0.0f),
				glm::f32vec2(0.5f, 0.5f),
				glm::f32vec2(1.0f, 1.0f),
				glm::f32vec2(-1.0f, 1.0f),
				glm::f32vec2(-0.5f, 0.5f),
				glm::f32vec2(0.0f, 0.0f)
			}};

			Error += run(gli::FORMAT_RG32_SFLOAT_PACK32, TestSamples);
		}

		{
			std::array<glm::f32vec3, 8> TestSamples{
			{
				glm::f32vec3(-1.0f, 0.0f, 1.0f),
				glm::f32vec3(-0.5f, 0.0f, 0.5f),
				glm::f32vec3(-0.2f, 0.0f, 0.2f),
				glm::f32vec3(-0.0f, 0.0f, 0.0f),
				glm::f32vec3(0.1f, 0.2f, 0.3f),
				glm::f32vec3(-0.1f,-0.2f,-0.3f),
				glm::f32vec3(0.7f, 0.8f, 0.9f),
				glm::f32vec3(-0.7f,-0.8f,-0.9f)
			}};

			Error += run(gli::FORMAT_RGB32_SFLOAT_PACK32, TestSamples);
		}

		{
			std::array<glm::f32vec4, 8> TestSamples{
			{
				glm::f32vec4(-1.0f, 0.0f, 1.0f, 1.0f),
				glm::f32vec4(-0.5f, 0.0f, 0.5f, 1.0f),
				glm::f32vec4(-0.2f, 0.0f, 0.2f, 1.0f),
				glm::f32vec4(-0.0f, 0.0f, 0.0f, 1.0f),
				glm::f32vec4(0.1f, 0.2f, 0.3f, 1.0f),
				glm::f32vec4(-0.1f,-0.2f,-0.3f, 1.0f),
				glm::f32vec4(0.7f, 0.8f, 0.9f, 1.0f),
				glm::f32vec4(-0.7f,-0.8f,-0.9f, 1.0f)
			}};

			Error += run(gli::FORMAT_RGBA32_SFLOAT_PACK32, TestSamples);
		}

		{
			std::array<glm::i8vec1, 8> TestSamples{
			{
				glm::i8vec1(-128),
				glm::i8vec1(-127),
				glm::i8vec1(127),
				glm::i8vec1(64),
				glm::i8vec1(-64),
				glm::i8vec1(1),
				glm::i8vec1(-1),
				glm::i8vec1(0)
			}};

			Error += run(gli::FORMAT_R8_SINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_R8_SNORM_PACK8, TestSamples);
		}

		{
			std::array<glm::i8vec2, 8> TestSamples{
			{
				glm::i8vec2(-128, -96),
				glm::i8vec2(-64,  96),
				glm::i8vec2(-128,  64),
				glm::i8vec2(127,  32),
				glm::i8vec2(0, 126),
				glm::i8vec2(-48,  48),
				glm::i8vec2(-127, 127),
				glm::i8vec2(64,   0)
			}};

			Error += run(gli::FORMAT_RG8_UINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_RG8_UNORM_PACK8, TestSamples);
		}

		{
			std::array<glm::i8vec3, 8> TestSamples{
			{
				glm::i8vec3(-128,   0,   0),
				glm::i8vec3(-128, 127,   0),
				glm::i8vec3(-128, -96,   0),
				glm::i8vec3(127,-128,   0),
				glm::i8vec3(0, 127,   0),
				glm::i8vec3(0, 127,-127),
				glm::i8vec3(0,  64, -64),
				glm::i8vec3(-32,  32,  96)
			}};

			Error += run(gli::FORMAT_RGB8_SINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_RGB8_SNORM_PACK8, TestSamples);
		}

		{
			std::array<glm::i8vec4, 8> TestSamples{
			{
				glm::i8vec4(-127,   0,   0, 127),
				glm::i8vec4(-128,  96,   0,-128),
				glm::i8vec4(127,  64,   0,   1),
				glm::i8vec4(0, -64,   0,   2),
				glm::i8vec4(-95,  32,   0,   3),
				glm::i8vec4(95, -32, 127,   4),
				glm::i8vec4(-63,  16,-128,  -1),
				glm::i8vec4(63, -16,-127,  -2)
			}};

			Error += run(gli::FORMAT_RGBA8_SINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_RGBA8_SNORM_PACK8, TestSamples);
		}

		{
			std::array<glm::u8vec1, 8> TestSamples{
			{
				glm::u8vec1(255),
				glm::u8vec1(224),
				glm::u8vec1(192),
				glm::u8vec1(128),
				glm::u8vec1(64),
				glm::u8vec1(32),
				glm::u8vec1(16),
				glm::u8vec1(0)
			}};

			Error += run(gli::FORMAT_R8_UINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_R8_UNORM_PACK8, TestSamples);
			Error += run(gli::FORMAT_R8_SRGB_PACK8, TestSamples);
		}

		{
			std::array<glm::u8vec2, 8> TestSamples{
			{
				glm::u8vec2(255,   0),
				glm::u8vec2(255, 128),
				glm::u8vec2(255, 255),
				glm::u8vec2(128, 255),
				glm::u8vec2(0, 255),
				glm::u8vec2(0, 255),
				glm::u8vec2(0,   0),
				glm::u8vec2(255,   0)
			}};

			Error += run(gli::FORMAT_RG8_UINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_RG8_UNORM_PACK8, TestSamples);
			Error += run(gli::FORMAT_RG8_SRGB_PACK8, TestSamples);
		}

		{
			std::array<glm::u8vec3, 8> TestSamples{
			{
				glm::u8vec3(255,   0,   0),
				glm::u8vec3(255, 128,   0),
				glm::u8vec3(255, 255,   0),
				glm::u8vec3(128, 255,   0),
				glm::u8vec3(0, 255,   0),
				glm::u8vec3(0, 255, 255),
				glm::u8vec3(0,   0, 255),
				glm::u8vec3(255,   0, 255)
			}};

			Error += run(gli::FORMAT_RGB8_UINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_RGB8_UNORM_PACK8, TestSamples);
			Error += run(gli::FORMAT_RGB8_SRGB_PACK8, TestSamples);
		}

		{
			std::array<glm::u8vec4, 8> TestSamples{
			{
				glm::u8vec4(255,   0,   0, 255),
				glm::u8vec4(255, 128,   0, 255),
				glm::u8vec4(255, 255,   0, 255),
				glm::u8vec4(128, 255,   0, 255),
				glm::u8vec4(0, 255,   0, 255),
				glm::u8vec4(0, 255, 255, 255),
				glm::u8vec4(0,   0, 255, 255),
				glm::u8vec4(255,   0, 255, 255)
			}};

			Error += run(gli::FORMAT_RGBA8_UINT_PACK8, TestSamples);
			Error += run(gli::FORMAT_RGBA8_UNORM_PACK8, TestSamples);
			Error += run(gli::FORMAT_RGBA8_SRGB_PACK8, TestSamples);
		}

		{
			std::array<glm::u16vec1, 8> TestSamples{
			{
				glm::u16vec1(65535),
				glm::u16vec1(32767),
				glm::u16vec1(192),
				glm::u16vec1(128),
				glm::u16vec1(64),
				glm::u16vec1(32),
				glm::u16vec1(16),
				glm::u16vec1(0)
			}};

			Error += run(gli::FORMAT_R16_UINT_PACK16, TestSamples);
			Error += run(gli::FORMAT_R16_UNORM_PACK16, TestSamples);
		}

		{
			std::array<glm::u16vec2, 8> TestSamples{
			{
				glm::u16vec2(255,   0),
				glm::u16vec2(255, 128),
				glm::u16vec2(255, 255),
				glm::u16vec2(128, 255),
				glm::u16vec2(0, 255),
				glm::u16vec2(0, 255),
				glm::u16vec2(0,   0),
				glm::u16vec2(255,   0)
			}};

			Error += run(gli::FORMAT_RG16_UINT_PACK16, TestSamples);
			Error += run(gli::FORMAT_RG16_UNORM_PACK16, TestSamples);
		}

		{
			std::array<glm::u16vec3, 8> TestSamples{
			{
				glm::u16vec3(255,   0,   0),
				glm::u16vec3(255, 128,   0),
				glm::u16vec3(255, 255,   0),
				glm::u16vec3(128, 255,   0),
				glm::u16vec3(0, 255,   0),
				glm::u16vec3(0, 255, 255),
				glm::u16vec3(0,   0, 255),
				glm::u16vec3(255,   0, 255)
			}};

			Error += run(gli::FORMAT_RGB16_UINT_PACK16, TestSamples);
			Error += run(gli::FORMAT_RGB16_UNORM_PACK16, TestSamples);
		}

		{
			std::array<glm::u16vec4, 8> TestSamples{
			{
				glm::u16vec4(255,   0,   0, 255),
				glm::u16vec4(255, 128,   0, 255),
				glm::u16vec4(255, 255,   0, 255),
				glm::u16vec4(128, 255,   0, 255),
				glm::u16vec4(0, 255,   0, 255),
				glm::u16vec4(0, 255, 255, 255),
				glm::u16vec4(0,   0, 255, 255),
				glm::u16vec4(255,   0, 255, 255)
			}};

			Error += run(gli::FORMAT_RGBA16_UINT_PACK16, TestSamples);
			Error += run(gli::FORMAT_RGBA16_UNORM_PACK16, TestSamples);
		}

		{
			std::array<glm::u32vec1, 8> TestSamples{
			{
				glm::u32vec1(65535),
				glm::u32vec1(32767),
				glm::u32vec1(192),
				glm::u32vec1(128),
				glm::u32vec1(64),
				glm::u32vec1(32),
				glm::u32vec1(16),
				glm::u32vec1(0)
			}};

			Error += run(gli::FORMAT_R32_UINT_PACK32, TestSamples);
		}

		{
			std::array<glm::u32vec2, 8> TestSamples{
			{
				glm::u32vec2(255,   0),
				glm::u32vec2(255, 128),
				glm::u32vec2(255, 255),
				glm::u32vec2(128, 255),
				glm::u32vec2(0, 255),
				glm::u32vec2(0, 255),
				glm::u32vec2(0,   0),
				glm::u32vec2(255,   0)
			}};

			Error += run(gli::FORMAT_RG32_UINT_PACK32, TestSamples);
		}

		{
			std::array<glm::u32vec3, 8> TestSamples{
			{
				glm::u32vec3(255,   0,   0),
				glm::u32vec3(255, 128,   0),
				glm::u32vec3(255, 255,   0),
				glm::u32vec3(128, 255,   0),
				glm::u32vec3(0, 255,   0),
				glm::u32vec3(0, 255, 255),
				glm::u32vec3(0,   0, 255),
				glm::u32vec3(255,   0, 255)
			}};

			Error += run(gli::FORMAT_RGB32_UINT_PACK32, TestSamples);
		}

		{
			std::array<glm::u32vec4, 8> TestSamples{
			{
				glm::u32vec4(255,   0,   0, 255),
				glm::u32vec4(255, 128,   0, 255),
				glm::u32vec4(255, 255,   0, 255),
				glm::u32vec4(128, 255,   0, 255),
				glm::u32vec4(0, 255,   0, 255),
				glm::u32vec4(0, 255, 255, 255),
				glm::u32vec4(0,   0, 255, 255),
				glm::u32vec4(255,   0, 255, 255)
			}};

			Error += run(gli::FORMAT_RGBA32_UINT_PACK32, TestSamples);
		}

		return Error;
	}
}//namespace load_store

namespace clear
{
	int test()
	{
		int Error = 0;

		glm::u8vec4 const Black(0, 0, 0, 255);
		glm::u8vec4 const Color(255, 127, 0, 255);

		gli::texture1DArray Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1DArray::texelcoord_type(8), 1, 5);
		Texture.clear(Black);

		glm::u8vec4 const TexelA = Texture.load<glm::u8vec4>(gli::texture1DArray::texelcoord_type(0), 0, 0);
		glm::u8vec4 const TexelB = Texture.load<glm::u8vec4>(gli::texture1DArray::texelcoord_type(0), 0, 1);
		glm::u8vec4 const TexelC = Texture.load<glm::u8vec4>(gli::texture1DArray::texelcoord_type(0), 0, 2);

		Error += TexelA == Black ? 0 : 1;
		Error += TexelB == Black ? 0 : 1;
		Error += TexelC == Black ? 0 : 1;

		Texture.clear<glm::u8vec4>(0, 1, glm::u8vec4(255, 127, 0, 255));

		gli::texture1DArray::texelcoord_type Coords(0);
		for(; Coords.x < Texture.dimensions(1).x; ++Coords.x)
		{
			glm::u8vec4 const TexelD = Texture.load<glm::u8vec4>(Coords, 0, 1);
			Error += TexelD == Color ? 0 : 1;
		}

		gli::texture1DArray TextureView(Texture, 0, 0, 1, 1);

		gli::texture1DArray TextureImage(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1DArray::texelcoord_type(4), 1, 1);
		TextureImage.clear(Color);

		Error += TextureView == TextureImage ? 0 : 1;

		return Error;
	}
}//namespace clear

int main()
{
	int Error(0);

	Error += test_alloc();
	Error += test_texture1DArray_size();
	Error += test_texture1DArray_query();
	Error += test_texture1DArray_clear();
	Error += test_texture1DArray_access();
	Error += load_store::test();
	Error += clear::test();

	return Error;
}

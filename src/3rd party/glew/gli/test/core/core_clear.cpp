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
/// @file gli/core/clear.cpp
/// @date 2013-02-07 / 2013-11-25
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/gli.hpp>
#include <ctime>

namespace clear
{
	template <typename texture, typename genType>
	int test_texture(typename texture::texelcoord_type Size, gli::format const & Format, std::vector<genType> const & Colors)
	{
		int Error(0);

		texture TextureA(Format, Size);

		for(typename texture::size_type Level = 0; Level < TextureA.levels(); ++Level)
			TextureA[Level].clear(Colors[Level]);

		texture TextureB = TextureA;
		Error += TextureB == TextureA ? 0 : 1;

		texture TextureC(gli::copy(TextureA));
		Error += TextureC == TextureA ? 0 : 1;

		texture TextureD(gli::copy(TextureB));
		Error += TextureD == TextureB ? 0 : 1;

		texture TextureE(gli::copy(TextureC, 1, 2));
		texture TextureF(TextureC, 1, 2);
		Error += TextureE == TextureF ? 0 : 1;

		texture TextureG(gli::copy(TextureD, 1, 2));
		texture TextureH(TextureD, 1, 2);
		Error += TextureG == TextureH ? 0 : 1;

		TextureG.clear(Colors[Colors.size() - 1]);
		TextureH.clear(Colors[Colors.size() - 1]);
		Error += TextureG == TextureH ? 0 : 1;

		TextureG.clear();
		TextureH.clear();
		Error += TextureG == TextureH ? 0 : 1;

		return Error;
	}

	int test()
	{
		std::vector<glm::u64> ColorDXT1;
		ColorDXT1.push_back(255);
		ColorDXT1.push_back(127);
		ColorDXT1.push_back(64);
		ColorDXT1.push_back(32);
		ColorDXT1.push_back(16);
		ColorDXT1.push_back(8);
		ColorDXT1.push_back(4);

		std::vector<glm::u8> ColorR8_UNORM;
		ColorR8_UNORM.push_back(255);
		ColorR8_UNORM.push_back(127);
		ColorR8_UNORM.push_back(64);
		ColorR8_UNORM.push_back(32);
		ColorR8_UNORM.push_back(16);
		ColorR8_UNORM.push_back(8);
		ColorR8_UNORM.push_back(4);

		std::vector<glm::u8vec3> ColorRGB8_UNORM;
		ColorRGB8_UNORM.push_back(glm::u8vec3(255,   0,   0));
		ColorRGB8_UNORM.push_back(glm::u8vec3(255, 127,   0));
		ColorRGB8_UNORM.push_back(glm::u8vec3(255, 255,   0));
		ColorRGB8_UNORM.push_back(glm::u8vec3(  0, 255,   0));
		ColorRGB8_UNORM.push_back(glm::u8vec3(  0, 255, 255));
		ColorRGB8_UNORM.push_back(glm::u8vec3(  0,   0, 255));
		ColorRGB8_UNORM.push_back(glm::u8vec3(255,   0, 255));

		std::vector<glm::u8vec4> ColorRGBA8_UNORM;
		ColorRGBA8_UNORM.push_back(glm::u8vec4(255,   0,   0, 255));
		ColorRGBA8_UNORM.push_back(glm::u8vec4(255, 127,   0, 255));
		ColorRGBA8_UNORM.push_back(glm::u8vec4(255, 255,   0, 255));
		ColorRGBA8_UNORM.push_back(glm::u8vec4(  0, 255,   0, 255));
		ColorRGBA8_UNORM.push_back(glm::u8vec4(  0, 255, 255, 255));
		ColorRGBA8_UNORM.push_back(glm::u8vec4(  0,   0, 255, 255));
		ColorRGBA8_UNORM.push_back(glm::u8vec4(255,   0, 255, 255));

		std::vector<glm::vec4> ColorRGBA32F;
		ColorRGBA32F.push_back(glm::vec4(1.0,   0,   0, 1.0));
		ColorRGBA32F.push_back(glm::vec4(1.0, 0.5,   0, 1.0));
		ColorRGBA32F.push_back(glm::vec4(1.0, 1.0,   0, 1.0));
		ColorRGBA32F.push_back(glm::vec4(  0, 1.0,   0, 1.0));
		ColorRGBA32F.push_back(glm::vec4(  0, 1.0, 1.0, 1.0));
		ColorRGBA32F.push_back(glm::vec4(  0,   0, 1.0, 1.0));
		ColorRGBA32F.push_back(glm::vec4(1.0,   0, 1.0, 1.0));

		std::vector<int> Sizes;
		Sizes.push_back(32);
		Sizes.push_back(16);
		Sizes.push_back(17);
		Sizes.push_back(15);
		Sizes.push_back(12);
		Sizes.push_back(5);

		int Error(0);

		for(std::size_t i = 0; i < Sizes.size(); ++i)
		{
			//Error += test_texture<gli::texture1D>(gli::texture1D::dim_type(Sizes[i]), gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, ColorDXT1);
			Error += test_texture<gli::texture2D>(gli::texture2D::texelcoord_type(Sizes[i]), gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, ColorDXT1);
			Error += test_texture<gli::texture3D>(gli::texture3D::texelcoord_type(Sizes[i]), gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, ColorDXT1);
			Error += test_texture<gli::texture1D>(gli::texture1D::texelcoord_type(Sizes[i]), gli::FORMAT_R8_UNORM_PACK8, ColorR8_UNORM);
			Error += test_texture<gli::texture2D>(gli::texture2D::texelcoord_type(Sizes[i]), gli::FORMAT_R8_UNORM_PACK8, ColorR8_UNORM);
			Error += test_texture<gli::texture3D>(gli::texture3D::texelcoord_type(Sizes[i]), gli::FORMAT_R8_UNORM_PACK8, ColorR8_UNORM);
			Error += test_texture<gli::texture1D>(gli::texture1D::texelcoord_type(Sizes[i]), gli::FORMAT_RGB8_UNORM_PACK8, ColorRGB8_UNORM);
			Error += test_texture<gli::texture2D>(gli::texture2D::texelcoord_type(Sizes[i]), gli::FORMAT_RGB8_UNORM_PACK8, ColorRGB8_UNORM);
			Error += test_texture<gli::texture3D>(gli::texture3D::texelcoord_type(Sizes[i]), gli::FORMAT_RGB8_UNORM_PACK8, ColorRGB8_UNORM);
			Error += test_texture<gli::texture1D>(gli::texture1D::texelcoord_type(Sizes[i]), gli::FORMAT_RGBA8_UNORM_PACK8, ColorRGBA8_UNORM);
			Error += test_texture<gli::texture2D>(gli::texture2D::texelcoord_type(Sizes[i]), gli::FORMAT_RGBA8_UNORM_PACK8, ColorRGBA8_UNORM);
			Error += test_texture<gli::texture3D>(gli::texture3D::texelcoord_type(Sizes[i]), gli::FORMAT_RGBA8_UNORM_PACK8, ColorRGBA8_UNORM);
			Error += test_texture<gli::texture1D>(gli::texture1D::texelcoord_type(Sizes[i]), gli::FORMAT_RGBA32_SFLOAT_PACK32, ColorRGBA32F);
			Error += test_texture<gli::texture2D>(gli::texture2D::texelcoord_type(Sizes[i]), gli::FORMAT_RGBA32_SFLOAT_PACK32, ColorRGBA32F);
			Error += test_texture<gli::texture3D>(gli::texture3D::texelcoord_type(Sizes[i]), gli::FORMAT_RGBA32_SFLOAT_PACK32, ColorRGBA32F);
		}

		return Error;
	}
}//namespace clear

int main()
{
	int Error(0);

	Error += clear::test();

	return Error;
}

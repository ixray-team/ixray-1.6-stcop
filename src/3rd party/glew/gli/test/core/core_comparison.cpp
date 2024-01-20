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
/// @file gli/core/core_comparison.cpp
/// @date 2013-02-04 / 2013-02-04
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/gli.hpp>

#include <map>

int test_texture1D()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::texture1D TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(32), gli::levels(32));

	{
		gli::texture1D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(32), gli::levels(32));

		Error += TextureA == TextureB ? 0 : 1;
		assert(!Error);
		Error += TextureA != TextureB ? 1 : 0;
		assert(!Error);
	}

	{
		gli::texture1D TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		assert(!Error);
		Error += TextureA != TextureC ? 1 : 0;
		assert(!Error);
	}

	{
		gli::texture1D TextureD(TextureA, TextureA.base_level(), TextureA.max_level());

		Error += TextureA == TextureD ? 0 : 1;
		assert(!Error);
		Error += TextureA != TextureD ? 1 : 0;
		assert(!Error);
	}

	{
		gli::texture1D TextureE(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(32));

		*TextureE[TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		assert(!Error);
		Error += TextureA == TextureE ? 1 : 0;
		assert(!Error);
	}

	{
		gli::texture1D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(32), 1);

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		assert(!Error);
		Error += TextureA == TextureB ? 1 : 0;
		assert(!Error);
	}

	{
		gli::texture1D TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, gli::texture1D::texelcoord_type(32));

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		assert(!Error);
		Error += TextureA == TextureB ? 1 : 0;
		assert(!Error);
	}

	{
		gli::texture1D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(64));

		gli::texture1D TextureC(TextureB, TextureB.base_level() + 1, TextureB.max_level());

		Error += TextureA == TextureC ? 0 : 1;
		assert(!Error);
		Error += TextureA != TextureC ? 1 : 0;
		assert(!Error);
	}

	return Error;
}

int test_texture1DArray()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::texture1DArray TextureA(
		gli::FORMAT_RGBA8_UNORM_PACK8,
		gli::texture1D::texelcoord_type(32),
		gli::texture1D::size_type(1));

	{
		gli::texture1DArray TextureB(
			gli::FORMAT_RGBA8_UNORM_PACK8,
			gli::texture1D::texelcoord_type(32),
			gli::texture1D::size_type(1));

		Error += TextureA == TextureB ? 0 : 1;
		Error += TextureA != TextureB ? 1 : 0;
	}

	{
		gli::texture1DArray TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	{
		gli::texture1DArray TextureD(TextureA, 
			TextureA.base_layer(),
			TextureA.max_layer(),
			TextureA.base_level(),
			TextureA.max_level());

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureA != TextureD ? 1 : 0;
	}

	{
		gli::texture1DArray TextureE(
			gli::FORMAT_RGBA8_UNORM_PACK8,
			gli::texture1DArray::texelcoord_type(32),
			gli::texture1D::size_type(1));

		*TextureE[0][TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		Error += TextureA == TextureE ? 1 : 0;
	}

	{
		gli::texture1DArray TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1DArray::texelcoord_type(32), 1, 1);

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture1DArray TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, gli::texture1DArray::texelcoord_type(32), 1);

		*TextureB[0][TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture1DArray TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1DArray::texelcoord_type(64), 1);

		gli::texture1DArray TextureC(TextureB,
			TextureB.base_layer(), TextureB.max_layer(),
			TextureB.base_level() + 1, TextureB.max_level());

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	return Error;
}

int test_texture2D()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::texture2D TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32));

	{
		gli::texture2D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32));

		Error += TextureA == TextureB ? 0 : 1;
		Error += TextureA != TextureB ? 1 : 0;
	}

	{
		gli::texture2D TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	{
		gli::texture2D TextureD(gli::view(
			TextureA,
			TextureA.base_level(), TextureA.max_level()));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureA != TextureD ? 1 : 0;
	}

	{
		gli::texture2D TextureE(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32));

		*TextureE[TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		Error += TextureA == TextureE ? 1 : 0;
	}

	{
		gli::texture2D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32), 1);

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture2D TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, gli::texture2D::texelcoord_type(32));

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture2D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(64));

		gli::texture2D TextureC(gli::view(
			TextureB,
			TextureB.base_level() + 1, TextureB.max_level()));

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	return Error;
}

int test_texture2DArray()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::texture2DArray const TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(32), 1);

	{
		gli::texture2DArray const TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(32), 1);

		Error += TextureA == TextureB ? 0 : 1;
		Error += TextureA != TextureB ? 1 : 0;
	}

	{
		gli::texture2DArray const TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	{
		gli::texture2DArray const TextureD(TextureA,
			TextureA.base_layer(), TextureA.max_layer(),
			TextureA.base_level(), TextureA.max_level());

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureA != TextureD ? 1 : 0;
	}

	{
		gli::texture2DArray const TextureE(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(32), 1);

		*TextureE[0][TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		Error += TextureA == TextureE ? 1 : 0;
	}

	{
		gli::texture2DArray const TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(32), 1, 1);

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture2DArray const TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, gli::texture2DArray::texelcoord_type(32), 1);

		*TextureB[0][TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture2DArray const TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2DArray::texelcoord_type(64), 1);

		gli::texture2DArray const TextureC(TextureB,
			TextureB.base_layer(), TextureB.max_layer(),
			TextureB.base_level() + 1, TextureB.max_level());

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	return Error;
}

int test_texture3D()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::texture3D TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(32));

	{
		gli::texture3D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(32));

		Error += TextureA == TextureB ? 0 : 1;
		Error += TextureA != TextureB ? 1 : 0;
	}

	{
		gli::texture3D TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	{
		gli::texture3D TextureD(TextureA, TextureA.base_level(), TextureA.max_level());

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureA != TextureD ? 1 : 0;
	}

	{
		gli::texture3D TextureE(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(32));

		*TextureE[TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		Error += TextureA == TextureE ? 1 : 0;
	}

	{
		gli::texture3D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(32), 1);

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture3D TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, gli::texture3D::texelcoord_type(32));

		*TextureB[TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::texture3D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(64));

		gli::texture3D TextureC(TextureB, TextureB.base_level() + 1, TextureB.max_level());

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	return Error;
}

int test_textureCube()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::textureCube::texelcoord_type const Size(16);

	gli::textureCube TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, Size);

	{
		gli::textureCube TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, Size);

		Error += TextureA == TextureB ? 0 : 1;
		Error += TextureA != TextureB ? 1 : 0;
	}

	{
		gli::textureCube TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	{
		gli::textureCube TextureD(TextureA, 
			TextureA.base_face(), TextureA.max_face(),
			TextureA.base_level(), TextureA.max_level());

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureA != TextureD ? 1 : 0;
	}

	{
		gli::textureCube TextureE(gli::FORMAT_RGBA8_UNORM_PACK8, Size);

		*TextureE[TextureE.faces() - 1][TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		Error += TextureA == TextureE ? 1 : 0;
	}

	{
		gli::textureCube TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, Size, 1);

		*TextureB[TextureB.faces() - 1][TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::textureCube TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, Size);

		*TextureB[TextureB.faces() - 1][TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::textureCube TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, Size << gli::textureCube::texelcoord_type(1));

		gli::textureCube TextureC(TextureB, 
			TextureB.base_face(), TextureB.max_face(),
			TextureB.base_level() + 1, TextureB.max_level());

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	return Error;
}

int test_textureCubeArray()
{
	int Error(0);

	std::vector<glm::u8vec4> Color(6);
	Color.push_back(glm::u8vec4(255,   0,   0, 255));
	Color.push_back(glm::u8vec4(255, 127,   0, 255));
	Color.push_back(glm::u8vec4(255, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255,   0, 255));
	Color.push_back(glm::u8vec4(  0, 255, 255, 255));
	Color.push_back(glm::u8vec4(  0,   0, 255, 255));

	gli::textureCubeArray TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCubeArray::texelcoord_type(32), 1);

	{
		gli::textureCubeArray TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCubeArray::texelcoord_type(32), 1);

		Error += TextureA == TextureB ? 0 : 1;
		Error += TextureA != TextureB ? 1 : 0;
	}

	{
		gli::textureCubeArray TextureC(TextureA);

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	{
		gli::textureCubeArray TextureD(TextureA, 
			TextureA.base_layer(), TextureA.max_layer(),
			TextureA.base_face(), TextureA.max_face(),
			TextureA.base_level(), TextureA.max_level());

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureA != TextureD ? 1 : 0;
	}

	{
		gli::textureCubeArray TextureE(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCubeArray::texelcoord_type(32), 1);

		*TextureE[0][TextureE.faces() - 1][TextureE.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureE ? 0 : 1;
		Error += TextureA == TextureE ? 1 : 0;
	}

	{
		gli::textureCubeArray TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCubeArray::texelcoord_type(32), 1, 1);

		*TextureB[0][TextureB.faces() - 1][TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::textureCubeArray TextureB(gli::FORMAT_RGBA8_SNORM_PACK8, gli::textureCubeArray::texelcoord_type(32), 1);

		*TextureB[0][TextureB.faces() - 1][TextureB.levels() - 1].data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

		Error += TextureA != TextureB ? 0 : 1;
		Error += TextureA == TextureB ? 1 : 0;
	}

	{
		gli::textureCubeArray TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCubeArray::texelcoord_type(64), 1);

		gli::textureCubeArray TextureC(TextureB, 
			TextureB.base_layer(), TextureB.max_layer(),
			TextureB.base_face(), TextureB.max_face(),
			TextureB.base_level() + 1, TextureB.max_level());

		Error += TextureA == TextureC ? 0 : 1;
		Error += TextureA != TextureC ? 1 : 0;
	}

	return Error;
}

class C
{};

int test_map()
{
	int Error(0);

	gli::texture2D TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32));
	gli::texture2D TextureB(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(64));
	
	std::map<int, gli::texture2D> Map;

	Map.insert(std::make_pair(0, TextureA));
	Map.insert(std::make_pair(0, TextureB));
	
	std::map<int, C> MapC;
	MapC.insert(std::make_pair(0, C()));
	MapC.insert(std::make_pair(0, C()));

	return Error;
}

int main()
{
	int Error(0);

	Error += test_texture1D();
	Error += test_texture1DArray();
	Error += test_texture2D();
	Error += test_texture2DArray();
	Error += test_texture3D();
	Error += test_textureCube();
	Error += test_textureCubeArray();
	Error += test_map();
		
	return Error;
}

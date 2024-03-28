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
/// @file gli/core/texture_lod_sampler_cube.cpp
/// @date 2015-10-26 / 2015-10-26
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/sampler_cube.hpp>
#include <gli/comparison.hpp>
#include <glm/gtx/component_wise.hpp>
#include <glm/gtc/epsilon.hpp>
#include <ctime>
#include <limits>
#include <array>

namespace load
{
	int test()
	{
		int Error(0);

		gli::textureCube Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(1), 1);
		*(Texture.data<glm::u8vec4>() + 0) = glm::u8vec4(255,   0,   0, 255);
		*(Texture.data<glm::u8vec4>() + 1) = glm::u8vec4(255, 128,   0, 255);
		*(Texture.data<glm::u8vec4>() + 2) = glm::u8vec4(255, 255,   0, 255);
		*(Texture.data<glm::u8vec4>() + 3) = glm::u8vec4(  0, 255,   0, 255);
		*(Texture.data<glm::u8vec4>() + 4) = glm::u8vec4(  0,   0, 255, 255);
		*(Texture.data<glm::u8vec4>() + 5) = glm::u8vec4(255,   0, 255, 255);

		glm::u8vec4 Data0 = Texture.load<glm::u8vec4>(gli::texture2D::texelcoord_type(0), 0, 0);
		glm::u8vec4 Data1 = Texture.load<glm::u8vec4>(gli::texture2D::texelcoord_type(0), 1, 0);
		glm::u8vec4 Data2 = Texture.load<glm::u8vec4>(gli::texture2D::texelcoord_type(0), 2, 0);
		glm::u8vec4 Data3 = Texture.load<glm::u8vec4>(gli::texture2D::texelcoord_type(0), 3, 0);
		glm::u8vec4 Data4 = Texture.load<glm::u8vec4>(gli::texture2D::texelcoord_type(0), 4, 0);
		glm::u8vec4 Data5 = Texture.load<glm::u8vec4>(gli::texture2D::texelcoord_type(0), 5, 0);

		Error += Data0 == glm::u8vec4(255,   0,   0, 255) ? 0 : 1;
		Error += Data1 == glm::u8vec4(255, 128,   0, 255) ? 0 : 1;
		Error += Data2 == glm::u8vec4(255, 255,   0, 255) ? 0 : 1;
		Error += Data3 == glm::u8vec4(  0, 255,   0, 255) ? 0 : 1;
		Error += Data4 == glm::u8vec4(  0,   0, 255, 255) ? 0 : 1;
		Error += Data5 == glm::u8vec4(255,   0, 255, 255) ? 0 : 1;

		return Error;
	}
}//namespace load

namespace texture_lod
{
	int test()
	{
		int Error = 0;

		{
			gli::textureCube Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(2));
			Texture.clear(gli::u8vec4(0, 0, 0, 255));
			Texture.store(gli::textureCube::texelcoord_type(0), 0, 1, gli::u8vec4(255,   0,   0, 255));
			Texture.store(gli::textureCube::texelcoord_type(0), 1, 1, gli::u8vec4(255, 127,   0, 255));
			Texture.store(gli::textureCube::texelcoord_type(0), 2, 1, gli::u8vec4(255, 255,   0, 255));
			Texture.store(gli::textureCube::texelcoord_type(0), 3, 1, gli::u8vec4(  0, 255,   0, 255));
			Texture.store(gli::textureCube::texelcoord_type(0), 4, 1, gli::u8vec4(  0,   0, 255, 255));
			Texture.store(gli::textureCube::texelcoord_type(0), 5, 1, gli::u8vec4(255,   0, 255, 255));

			gli::fsamplerCube Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_NEAREST, gli::FILTER_NEAREST, gli::vec4(1.0f, 0.5f, 0.0f, 1.0f));

			gli::vec4 Sample0 = Sampler.texture_lod(gli::fsamplerCube::samplecoord_type(0.5f), 0, 1.0f);
			gli::vec4 Sample1 = Sampler.texture_lod(gli::fsamplerCube::samplecoord_type(0.5f), 1, 1.0f);
			gli::vec4 Sample2 = Sampler.texture_lod(gli::fsamplerCube::samplecoord_type(0.5f), 2, 1.0f);
			gli::vec4 Sample3 = Sampler.texture_lod(gli::fsamplerCube::samplecoord_type(0.5f), 3, 1.0f);
			gli::vec4 Sample4 = Sampler.texture_lod(gli::fsamplerCube::samplecoord_type(0.5f), 4, 1.0f);
			gli::vec4 Sample5 = Sampler.texture_lod(gli::fsamplerCube::samplecoord_type(0.5f), 5, 1.0f);

			Error += gli::all(gli::epsilonEqual(Sample0, gli::vec4(1.0f, 0.0f, 0.0f, 1.0f), 0.01f)) ? 0 : 1;
			Error += gli::all(gli::epsilonEqual(Sample1, gli::vec4(1.0f, 0.5f, 0.0f, 1.0f), 0.01f)) ? 0 : 1;
			Error += gli::all(gli::epsilonEqual(Sample2, gli::vec4(1.0f, 1.0f, 0.0f, 1.0f), 0.01f)) ? 0 : 1;
			Error += gli::all(gli::epsilonEqual(Sample3, gli::vec4(0.0f, 1.0f, 0.0f, 1.0f), 0.01f)) ? 0 : 1;
			Error += gli::all(gli::epsilonEqual(Sample4, gli::vec4(0.0f, 0.0f, 1.0f, 1.0f), 0.01f)) ? 0 : 1;
			Error += gli::all(gli::epsilonEqual(Sample5, gli::vec4(1.0f, 0.0f, 1.0f, 1.0f), 0.01f)) ? 0 : 1;
		}

		return Error;
	}
}//namespace texture_lod

namespace sampler_type
{
	int test()
	{
		int Error = 0;

		{
			gli::textureCube Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4), 1);
			gli::fsamplerCube Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_LINEAR, gli::FILTER_LINEAR);
		}

		{
			gli::textureCube Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4), 1);
			gli::dsamplerCube Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_LINEAR, gli::FILTER_LINEAR);
		}

		{
			gli::textureCube Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4), 1);
			gli::isamplerCube Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_NEAREST, gli::FILTER_NEAREST);
		}

		return Error;
	}
}//namespace sampler_type

int main()
{
	int Error(0);

	Error += texture_lod::test();
	Error += load::test();
	Error += sampler_type::test();

	return Error;
}


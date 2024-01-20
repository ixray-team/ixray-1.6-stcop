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
/// @file gli/core/core_sampler_clear.cpp
/// @date 2015-10-13 / 2015-10-13
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/sampler2d.hpp>
#include <gli/comparison.hpp>
#include <glm/gtc/epsilon.hpp>

namespace rgba8unorm
{
	int test()
	{
		int Error = 0;

		glm::vec4 const Orange(1.0f, 0.5f, 0.0f, 1.0f);

		gli::texture2D Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32), 1);
		gli::fsampler2D Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_LINEAR, gli::FILTER_LINEAR);
		Sampler.clear(Orange);

		glm::u8vec4 const Texel = Texture.load<glm::u8vec4>(gli::dim2_t(7), 0);

		Error += Texel == glm::u8vec4(255, 127, 0, 255) ? 0 : 1;

		return Error;
	}
}//namespace rgba8unorm

namespace rgba32sf
{
	int test()
	{
		int Error = 0;

		glm::f32vec4 const Orange(1.0f, 0.5f, 0.0f, 1.0f);

		gli::texture2D Texture(gli::FORMAT_RGBA32_SFLOAT_PACK32, gli::texture2D::texelcoord_type(4), 1);
		gli::fsampler2D Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_LINEAR, gli::FILTER_LINEAR);
		Sampler.clear(Orange);

		gli::f32vec4 const Texel0 = Texture.load<gli::f32vec4>(gli::dim2_t(0), 0);
		gli::f32vec4 const Texel1 = Texture.load<gli::f32vec4>(gli::dim2_t(2), 0);

		Error += glm::all(glm::epsilonEqual(Texel0, Orange, 0.01f)) ? 0 : 1;
		Error += glm::all(glm::epsilonEqual(Texel1, Orange, 0.01f)) ? 0 : 1;

		return Error;
	}
}//namespace rgba32sf

int main()
{
	int Error(0);

	Error += rgba32sf::test();
	Error += rgba8unorm::test();

	return Error;
}


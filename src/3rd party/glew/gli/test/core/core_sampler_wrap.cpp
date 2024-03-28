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
/// @file gli/core/core_sampler_wrap.cpp
/// @date 2015-10-10 / 2015-10-15
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/sampler2d.hpp>
#include <gli/comparison.hpp>
#include <glm/gtx/component_wise.hpp>
#include <glm/gtc/epsilon.hpp>
#include <ctime>
#include <limits>
#include <array>

namespace wrap_clamp_to_border
{
	int test()
	{
		int Error(0);

		glm::vec4 const Orange(1.0f, 0.5f, 0.0f, 1.0f);
		glm::vec4 const Blue(0.0f, 0.5f, 1.0f, 1.0f);

		gli::texture2D Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32), 1);
		Texture.clear(glm::packUnorm4x8(Orange));

		gli::fsampler2D SamplerA(Texture, gli::WRAP_CLAMP_TO_BORDER, gli::FILTER_LINEAR, gli::FILTER_LINEAR, Blue);

		{
			std::array<gli::fsampler2D::samplecoord_type, 8> SampleCoord{
			{
				gli::fsampler2D::samplecoord_type( 0.5f, 0.5f),
				gli::fsampler2D::samplecoord_type( 0.1f, 0.1f),
				gli::fsampler2D::samplecoord_type( 0.5f, 0.1f),
				gli::fsampler2D::samplecoord_type( 0.1f, 0.5f),
				gli::fsampler2D::samplecoord_type( 0.5f, 0.9f),
				gli::fsampler2D::samplecoord_type( 0.9f, 0.9f),
				gli::fsampler2D::samplecoord_type( 0.9f, 0.5f),
				gli::fsampler2D::samplecoord_type( 0.1f, 0.9f)
			}};

			for(std::size_t i = 0, n = SampleCoord.size(); i < n; ++i)
			{
				gli::vec4 const Texel = SamplerA.texture_lod(SampleCoord[i], 0.0f);
				Error += glm::all(glm::epsilonEqual(Texel, Orange, 0.01f)) ? 0 : 1;
			}
		}
		{
			std::array<gli::fsampler2D::samplecoord_type, 8> SampleCoord{
			{
				gli::fsampler2D::samplecoord_type( 0.5f,-0.5f),
				gli::fsampler2D::samplecoord_type(-0.5f,-0.5f),
				gli::fsampler2D::samplecoord_type(-0.5f, 0.5f),
				gli::fsampler2D::samplecoord_type( 1.5f, 0.5f),
				gli::fsampler2D::samplecoord_type( 1.5f, 1.5f),
				gli::fsampler2D::samplecoord_type( 0.5f, 1.5f),
				gli::fsampler2D::samplecoord_type( 1.5f,-0.5f),
				gli::fsampler2D::samplecoord_type(-0.5f, 1.5f)
			}};

			for(std::size_t i = 0, n = SampleCoord.size(); i < n; ++i)
			{
				gli::vec4 const Texel = SamplerA.texture_lod(SampleCoord[i], 0.0f);
				Error += glm::all(glm::epsilonEqual(Texel, Blue, 0.01f)) ? 0 : 1;
			}
		}

		return Error;
	}
}//namespace wrap_clamp_to_border

namespace wrap_mirror
{
	int test()
	{
		int Error(0);

		glm::vec4 const Orange(1.0f, 0.5f, 0.0f, 1.0f);
		glm::vec4 const Blue(0.0f, 0.5f, 1.0f, 1.0f);

		gli::texture2D Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(32), 1);
		Texture.clear(glm::packUnorm4x8(Orange));

		gli::fsampler2D Sampler(Texture, gli::WRAP_REPEAT, gli::FILTER_LINEAR, gli::FILTER_LINEAR, Blue);

		{
			std::array<gli::fsampler2D::samplecoord_type, 16> SampleCoord{
			{
				gli::fsampler2D::samplecoord_type( 0.5f, 0.5f),
				gli::fsampler2D::samplecoord_type( 0.1f, 0.1f),
				gli::fsampler2D::samplecoord_type( 0.5f, 0.1f),
				gli::fsampler2D::samplecoord_type( 0.1f, 0.5f),
				gli::fsampler2D::samplecoord_type( 0.5f, 0.9f),
				gli::fsampler2D::samplecoord_type( 0.9f, 0.9f),
				gli::fsampler2D::samplecoord_type( 0.9f, 0.5f),
				gli::fsampler2D::samplecoord_type( 0.1f, 0.9f),
				gli::fsampler2D::samplecoord_type( 0.5f,-0.5f),
				gli::fsampler2D::samplecoord_type(-0.5f,-0.5f),
				gli::fsampler2D::samplecoord_type(-0.5f, 0.5f),
				gli::fsampler2D::samplecoord_type( 1.5f, 0.5f),
				gli::fsampler2D::samplecoord_type( 1.5f, 1.5f),
				gli::fsampler2D::samplecoord_type( 0.5f, 1.5f),
				gli::fsampler2D::samplecoord_type( 1.5f,-0.5f),
				gli::fsampler2D::samplecoord_type(-0.5f, 1.5f)
			}};

			for(std::size_t i = 0, n = SampleCoord.size(); i < n; ++i)
			{
				gli::vec4 const Texel = Sampler.texture_lod(SampleCoord[i], 0.0f);
				Error += glm::all(glm::epsilonEqual(Texel, Orange, 0.01f)) ? 0 : 1;
			}
		}

		return Error;
	}
}//namespace wrap_mirror

int main()
{
	int Error(0);

	Error += wrap_clamp_to_border::test();
	Error += wrap_mirror::test();

	return Error;
}


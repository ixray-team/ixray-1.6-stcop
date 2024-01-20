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
/// @file gli/core/filter.cpp
/// @date 2015-10-19 / 2015-10-19
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/gli.hpp>
#include <glm/gtc/epsilon.hpp>

namespace filter2d
{
	int test()
	{
		int Error = 0;

		gli::vec4 const ColorFill(1.0f, 0.5f, 0.0f, 1.0f);
		gli::vec4 const ColorBorder(0.0f, 0.5f, 1.0f, 1.0f);

		gli::texture2D Texture(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(2, 2), 1);
		Texture.clear(glm::packUnorm<gli::u8>(ColorFill));

		{
			gli::fsampler2D Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_LINEAR, gli::FILTER_LINEAR, ColorBorder);

			gli::vec4 const TexelA = Sampler.texture_lod(gli::vec2(0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelA, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelB = Sampler.texture_lod(gli::vec2(-0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelB, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelC = Sampler.texture_lod(gli::vec2(1.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelC, ColorFill, 0.01f)) ? 0 : 1;
		}
		{
			gli::fsampler2D Sampler(Texture, gli::WRAP_CLAMP_TO_BORDER, gli::FILTER_LINEAR, gli::FILTER_LINEAR, ColorBorder);

			gli::vec4 const TexelA = Sampler.texture_lod(gli::vec2(0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelA, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelB = Sampler.texture_lod(gli::vec2(-1.0f, -1.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelB, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelC = Sampler.texture_lod(gli::vec2(+2.0f, +2.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelC, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelD = Sampler.texture_lod(gli::vec2(+2.0f, -1.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelD, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelE = Sampler.texture_lod(gli::vec2(-1.0f, +2.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelE, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelF = Sampler.texture_lod(gli::vec2(-0.5f,+0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelF, (ColorFill + ColorBorder) * 0.5f, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelG = Sampler.texture_lod(gli::vec2(+0.5f,+1.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelG, (ColorFill + ColorBorder) * 0.5f, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelH = Sampler.texture_lod(gli::vec2(+0.5f,-0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelH, (ColorFill + ColorBorder) * 0.5f, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelI = Sampler.texture_lod(gli::vec2(1.5f, 0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelI, (ColorFill + ColorBorder) * 0.5f, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelJ = Sampler.texture_lod(gli::vec2(+0.0f,+0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelJ, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelK = Sampler.texture_lod(gli::vec2(+0.5f,+1.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelK, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelL = Sampler.texture_lod(gli::vec2(+0.5f,-0.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelL, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelM = Sampler.texture_lod(gli::vec2(1.0f, 0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelM, ColorFill, 0.01f)) ? 0 : 1;
		}
		{
			gli::fsampler2D Sampler(Texture, gli::WRAP_CLAMP_TO_EDGE, gli::FILTER_NEAREST, gli::FILTER_NEAREST, ColorBorder);
			gli::vec4 const TexelA = Sampler.texture_lod(gli::vec2(0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelA, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelB = Sampler.texture_lod(gli::vec2(-0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelB, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelC = Sampler.texture_lod(gli::vec2(1.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelC, ColorFill, 0.01f)) ? 0 : 1;
		}
		{
			gli::fsampler2D Sampler(Texture, gli::WRAP_CLAMP_TO_BORDER, gli::FILTER_NEAREST, gli::FILTER_NEAREST, ColorBorder);

			gli::vec4 const TexelA = Sampler.texture_lod(gli::vec2(0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelA, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelB = Sampler.texture_lod(gli::vec2(-0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelB, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelC = Sampler.texture_lod(gli::vec2(1.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelC, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelD = Sampler.texture_lod(gli::vec2(+0.5f,-0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelD, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelE = Sampler.texture_lod(gli::vec2(+0.5f, 1.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelE, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelF = Sampler.texture_lod(gli::vec2(-0.5f, 0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelF, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelG = Sampler.texture_lod(gli::vec2(+1.5f, 0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelG, ColorBorder, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelH = Sampler.texture_lod(gli::vec2(+0.5f,+0.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelH, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelI = Sampler.texture_lod(gli::vec2(+0.5f,+1.0f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelI, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelJ = Sampler.texture_lod(gli::vec2(+0.0f,+0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelJ, ColorFill, 0.01f)) ? 0 : 1;

			gli::vec4 const TexelK = Sampler.texture_lod(gli::vec2(+1.0f,+0.5f), 0.0f);
			Error += gli::all(gli::epsilonEqual(TexelK, ColorFill, 0.01f)) ? 0 : 1;
		}

		return Error;
	}
}//namespace filter2d

int main()
{
	int Error(0);

	Error += filter2d::test();

	return Error;
}


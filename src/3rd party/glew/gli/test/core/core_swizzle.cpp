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
/// @file gli/core/swizzle.cpp
/// @date 2015-09-05 / 2015-09-05
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/texture.hpp>
#include <gli/comparison.hpp>

namespace swizzle
{
	int run()
	{
		int Error(0);

		{
			gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);
			gli::texture::swizzles_type const Swizzles = Texture.swizzles();
			Error += Swizzles == gli::swizzles(gli::SWIZZLE_RED, gli::SWIZZLE_GREEN, gli::SWIZZLE_BLUE, gli::SWIZZLE_ALPHA) ? 0 : 1;
		}

		{
			gli::texture Texture(gli::TARGET_2D, gli::FORMAT_BGRA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);
			gli::texture::swizzles_type const Swizzles = Texture.swizzles();
			Error += Swizzles == gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA) ? 0 : 1;
		}

		{
			gli::texture Texture(gli::TARGET_2D, gli::FORMAT_BGRA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1, gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));
			gli::texture::swizzles_type const Swizzles = Texture.swizzles();
			Error += Swizzles == gli::swizzles(gli::SWIZZLE_RED, gli::SWIZZLE_GREEN, gli::SWIZZLE_BLUE, gli::SWIZZLE_ALPHA) ? 0 : 1;
		}

		return Error;
	}
}//namespace swizzle

namespace texture1D
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_1D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 1, 1), 1, 1, 1);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_1D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 1, 1), 1, 1, 1);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::texture1D TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(4), 1);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::texture1D TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1D::texelcoord_type(4), 1);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;

		return Error;
	}
}//namespace texture1D

namespace texture1DArray
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_1D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 1, 1), 2, 1, 4);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_1D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 1, 1), 2, 1, 4);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::texture1DArray TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1DArray::texelcoord_type(4), 2, 4);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::texture1DArray TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture1DArray::texelcoord_type(4), 2, 4);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}
}//namespace texture1DArray

namespace texture2D
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 1, 2);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 1, 2);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::texture2D TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(4, 4), 2);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::texture2D TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(4, 4), 2);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}
}//namespace texture2D

namespace texture2DArray
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_2D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 2, 1, 4);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_2D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 2, 1, 4);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::texture2DArray TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(4, 4), 2, 4);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::texture2DArray TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(4, 4), 2, 4);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}
}//namespace texture2DArray

namespace texture3D
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_3D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 4), 1, 1, 2);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_3D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 4), 1, 1, 2);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::texture3D TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(4, 4, 4), 2);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::texture3D TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture3D::texelcoord_type(4, 4, 4), 2);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}
}//namespace texture3D

namespace textureCube
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_CUBE, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 6, 2);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_CUBE, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 6, 2);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::textureCube TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4, 4), 2);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::textureCube TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4, 4), 2);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}
}//namespace textureCube

namespace textureCubeArray
{
	int run()
	{
		int Error(0);

		gli::texture TextureA(gli::TARGET_CUBE_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 3, 6, 2);
		TextureA.clear(glm::u8vec4(255, 127, 0, 192));

		gli::texture TextureB(gli::TARGET_CUBE_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 3, 6, 2);
		TextureB.clear(glm::u8vec4(0, 127, 255, 192));
		TextureB.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureB ? 0 : 1;

		gli::textureCubeArray TextureC(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4, 4), 3, 2);
		TextureC.clear(glm::u8vec4(255, 127, 0, 192));

		Error += TextureA == TextureC ? 0 : 1;

		gli::textureCubeArray TextureD(gli::FORMAT_RGBA8_UNORM_PACK8, gli::textureCube::texelcoord_type(4, 4), 3, 2);
		TextureD.clear(glm::u8vec4(0, 127, 255, 192));
		TextureD.swizzle<glm::u8vec4>(gli::swizzles(gli::SWIZZLE_BLUE, gli::SWIZZLE_GREEN, gli::SWIZZLE_RED, gli::SWIZZLE_ALPHA));

		Error += TextureA == TextureD ? 0 : 1;
		Error += TextureC == TextureD ? 0 : 1;

		return Error;
	}
}//namespace textureCubeArray

int main()
{
	int Error(0);

	Error += swizzle::run();
	Error += texture1D::run();
	Error += texture1DArray::run();
	Error += texture2D::run();
	Error += texture2DArray::run();
	Error += texture3D::run();
	Error += textureCube::run();
	Error += textureCubeArray::run();

	assert(!Error);

	return Error;
}


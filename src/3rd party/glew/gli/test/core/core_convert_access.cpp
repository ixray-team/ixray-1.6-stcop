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
/// @file gli/core/core_convert_access.cpp
/// @date 2015-10-11 / 2015-10-11
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/sampler2d.hpp>
#include <glm/gtc/epsilon.hpp>

template<typename textureType>
struct texture
{
	typedef gli::detail::accessFunc<textureType, gli::u8vec1> u8vec1access;
	typedef gli::detail::accessFunc<textureType, gli::u8vec2> u8vec2access;
	typedef gli::detail::accessFunc<textureType, gli::u8vec3> u8vec3access;
	typedef gli::detail::accessFunc<textureType, gli::u8vec4> u8vec4access;

	static int test()
	{
		int Error = 0;

		{
			textureType Texture(gli::FORMAT_R8_UNORM_PACK8, typename textureType::texelcoord_type(1), 1);

			gli::u8vec1 const Color(127);
			u8vec1access::store(Texture, typename textureType::texelcoord_type(0), 0, 0, 0, Color);
			gli::u8vec1 const Texel = u8vec1access::load(Texture, typename textureType::texelcoord_type(0), 0, 0, 0);

			Error += Color == Texel ? 0 : 1;
		}

		{
			textureType Texture(gli::FORMAT_RG8_UNORM_PACK8, typename textureType::texelcoord_type(1), 1);

			gli::u8vec2 const Color(255, 127);
			u8vec2access::store(Texture, typename textureType::texelcoord_type(0), 0, 0, 0, Color);
			gli::u8vec2 const Texel = u8vec2access::load(Texture, typename textureType::texelcoord_type(0), 0, 0, 0);

			Error += Color == Texel ? 0 : 1;
		}

		{
			textureType Texture(gli::FORMAT_RGB8_UNORM_PACK8, typename textureType::texelcoord_type(1), 1);

			gli::u8vec3 const Color(255, 127, 0);
			u8vec3access::store(Texture, typename textureType::texelcoord_type(0), 0, 0, 0, Color);
			gli::u8vec3 const Texel = u8vec3access::load(Texture, typename textureType::texelcoord_type(0), 0, 0, 0);

			Error += Color == Texel ? 0 : 1;
		}

		{
			textureType Texture(gli::FORMAT_RGBA8_UNORM_PACK8, typename textureType::texelcoord_type(1), 1);

			gli::u8vec4 const Color(255, 127, 0, 255);
			u8vec4access::store(Texture, typename textureType::texelcoord_type(0), 0, 0, 0, Color);
			gli::u8vec4 const Texel = u8vec4access::load(Texture, typename textureType::texelcoord_type(0), 0, 0, 0);

			Error += Color == Texel ? 0 : 1;
		}

		return Error;
	}
};

int main()
{
	int Error = 0;

	Error += texture<gli::texture1D>::test();
	Error += texture<gli::texture1DArray>::test();
	Error += texture<gli::texture2D>::test();
	Error += texture<gli::texture2DArray>::test();
	Error += texture<gli::texture3D>::test();
	Error += texture<gli::textureCube>::test();
	Error += texture<gli::textureCubeArray>::test();

	return Error;
}


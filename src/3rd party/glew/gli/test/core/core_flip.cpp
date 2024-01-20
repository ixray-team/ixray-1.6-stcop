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
/// @file gli/core/copy.cpp
/// @date 2013-02-04 / 2013-11-25
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/gli.hpp>

template <typename texture, typename genType>
int test_texture
(
	texture const & Texture,
	genType const & ClearColor,
	genType const & FirstColor
)
{
	int Error(0);

	texture TextureA(gli::copy(Texture));
	TextureA.template clear<genType>(ClearColor);
	*TextureA.template data<genType>() = FirstColor;

	texture TextureB = gli::flip(TextureA);
	Error += TextureA != TextureB ? 0 : 1;

	texture TextureC = gli::flip(TextureB);
	Error += TextureC == TextureA ? 0 : 1;

	return Error;
}

int main()
{
	int Error(0);

	gli::texture2D::texelcoord_type const TextureSize(32);
	gli::size_t const Levels = gli::levels(TextureSize);

	Error += test_texture(
		gli::texture2D(gli::FORMAT_R8_UNORM_PACK8, TextureSize, Levels),
		glm::uint8(255), glm::uint8(0));

	Error += test_texture(
		gli::texture2D(gli::FORMAT_RGB8_UNORM_PACK8, TextureSize, Levels),
		glm::u8vec3(255, 128, 0), glm::u8vec3(0, 128, 255));

	Error += test_texture(
		gli::texture2D(gli::FORMAT_RGBA8_UNORM_PACK8, TextureSize, Levels),
		glm::u8vec4(255, 128, 0, 255), glm::u8vec4(0, 128, 255, 255));

	Error += test_texture(
		gli::texture2D(gli::FORMAT_RGBA32_SFLOAT_PACK32, TextureSize, Levels),
		glm::f32vec4(1.0, 0.5, 0.0, 1.0), glm::f32vec4(0.0, 0.5, 1.0, 1.0));

	Error += test_texture(
		gli::texture2DArray(gli::FORMAT_RGBA8_UNORM_PACK8, TextureSize, 4, Levels),
		glm::u8vec4(255, 128, 0, 255), glm::u8vec4(0, 128, 255, 255));

	Error += test_texture(
		gli::texture2DArray(gli::FORMAT_RGBA32_SFLOAT_PACK32, TextureSize, 4, Levels),
		glm::f32vec4(1.0, 0.5, 0.0, 1.0), glm::f32vec4(0.0, 0.5, 1.0, 1.0));

	return Error;
}

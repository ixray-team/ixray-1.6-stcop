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
/// @file test/core/texture_1d.cpp
/// @date 2015-08-19 / 2015-08-19
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/texture.hpp>
#include <gli/levels.hpp>
#include <gli/comparison.hpp>
#include <gli/save.hpp>
#include <gli/load.hpp>

namespace alloc
{
	int run()
	{
		int Error(0);

		std::vector<int> Sizes;
		Sizes.push_back(16);
		Sizes.push_back(32);
		Sizes.push_back(15);
		Sizes.push_back(17);
		Sizes.push_back(1);

		for(std::size_t TargetIndex = gli::TARGET_FIRST; TargetIndex <= gli::TARGET_LAST; ++TargetIndex)
		for(std::size_t FormatIndex = gli::FORMAT_FIRST; FormatIndex <= gli::FORMAT_LAST; ++FormatIndex)
		{
			gli::format const Format = static_cast<gli::format>(FormatIndex);
			gli::target const Target = static_cast<gli::target>(TargetIndex);
			gli::texture::size_type const Faces = gli::is_target_cube(Target) ? 6 : 1;

			if(gli::is_compressed(Format) && gli::is_target_1d(Target))
				continue;

			for(std::size_t SizeIndex = 0; SizeIndex < Sizes.size(); ++SizeIndex)
			{
				gli::texture::texelcoord_type Size(Sizes[SizeIndex]);

				gli::texture TextureA(Target, Format, Size, 1, Faces, gli::levels(Size));
				gli::texture TextureB(Target, Format, Size, 1, Faces, gli::levels(Size));

				Error += TextureA == TextureB ? 0 : 1;
			}
		}

		return Error;
	}
}//namespace alloc

namespace clear
{
	int run()
	{
		int Error(0);

		glm::u8vec4 const Orange(255, 127, 0, 255);

		gli::texture::texelcoord_type Size(16, 16, 1);
		gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, Size, 1, 1, gli::levels(Size));

		Texture.clear<glm::u8vec4>(Orange);

		return Error;
	}
}//namespace

namespace query
{
	int run()
	{
		int Error(0);

		gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UINT_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);

		Error += Texture.size() == sizeof(glm::u8vec4) * 1 ? 0 : 1;
		Error += Texture.format() == gli::FORMAT_RGBA8_UINT_PACK8 ? 0 : 1;
		Error += Texture.levels() == 1 ? 0 : 1;
		Error += !Texture.empty() ? 0 : 1;
		Error += Texture.dimensions() == gli::texture::texelcoord_type(1) ? 0 : 1;

		return Error;
	}
}//namespace

namespace tex_access
{
	int run()
	{
		int Error(0);

		{
			gli::texture1D Texture(gli::FORMAT_RGBA8_UINT_PACK8, gli::texture1D::texelcoord_type(2), 2);
			assert(!Texture.empty());

			gli::image Image0 = Texture[0];
			gli::image Image1 = Texture[1];

			std::size_t Size0 = Image0.size();
			std::size_t Size1 = Image1.size();

			Error += Size0 == sizeof(glm::u8vec4) * 2 ? 0 : 1;
			Error += Size1 == sizeof(glm::u8vec4) * 1 ? 0 : 1;

			*Image0.data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);
			*Image1.data<glm::u8vec4>() = glm::u8vec4(0, 127, 255, 255);

			glm::u8vec4 * PointerA = Image0.data<glm::u8vec4>();
			glm::u8vec4 * PointerB = Image1.data<glm::u8vec4>();

			glm::u8vec4 * Pointer0 = Texture.data<glm::u8vec4>() + 0;
			glm::u8vec4 * Pointer1 = Texture.data<glm::u8vec4>() + 2;

			Error += PointerA == Pointer0 ? 0 : 1;
			Error += PointerB == Pointer1 ? 0 : 1;

			glm::u8vec4 ColorA = *Image0.data<glm::u8vec4>();
			glm::u8vec4 ColorB = *Image1.data<glm::u8vec4>();

			glm::u8vec4 Color0 = *Pointer0;
			glm::u8vec4 Color1 = *Pointer1;

			Error += ColorA == Color0 ? 0 : 1;
			Error += ColorB == Color1 ? 0 : 1;

			Error += glm::all(glm::equal(Color0, glm::u8vec4(255, 127, 0, 255))) ? 0 : 1;
			Error += glm::all(glm::equal(Color1, glm::u8vec4(0, 127, 255, 255))) ? 0 : 1;
		}

		{
			gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UINT_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);

			std::size_t SizeA = Texture.size();
			Error += SizeA == sizeof(glm::u8vec4) * 1 ? 0 : 1;

			*Texture.data<glm::u8vec4>() = glm::u8vec4(255, 127, 0, 255);

			glm::u8vec4 * Pointer0 = Texture.data<glm::u8vec4>() + 0;;
			glm::u8vec4 Color0 = *Pointer0;

			Error += glm::all(glm::equal(Color0, glm::u8vec4(255, 127, 0, 255))) ? 0 : 1;
		}

		return Error;
	}
}//namespace

namespace size
{
	struct test
	{
		test(
			gli::format const & Format,
			gli::texture::texelcoord_type const & Dimensions,
			gli::texture::size_type const & Size) :
			Format(Format),
			Dimensions(Dimensions),
			Size(Size)
		{}

		gli::format Format;
		gli::texture::texelcoord_type Dimensions;
		gli::texture::size_type Size;
	};

	int run()
	{
		int Error(0);

		std::vector<test> Tests;
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, gli::texture::texelcoord_type(1), 4));
		Tests.push_back(test(gli::FORMAT_R8_UINT_PACK8, gli::texture::texelcoord_type(1), 1));

		for(std::size_t i = 0; i < Tests.size(); ++i)
		{
			gli::texture Texture(
				gli::TARGET_2D,
				Tests[i].Format,
				gli::texture::texelcoord_type(1),
				gli::texture::size_type(1),
				gli::texture::size_type(1),
				gli::texture::size_type(1));

			Error += Texture.size() == Tests[i].Size ? 0 : 1;
			assert(!Error);
		}

		return Error;
	}
}//namespace size

namespace specialize
{
	int run()
	{
		int Error(0);

		gli::texture Texture(gli::TARGET_1D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);
		gli::texture1D Texture1D(Texture);
		gli::texture1DArray Texture1DArray(Texture);
		gli::texture2D Texture2D(Texture);
		gli::texture2DArray Texture2DArray(Texture);
		gli::texture3D Texture3D(Texture);
		gli::textureCube TextureCube(Texture);
		gli::textureCubeArray TextureCubeArray(Texture);

		Error += Texture == Texture1D ? 0 : 1;
		Error += Texture != Texture1DArray ? 0 : 1;
		Error += Texture != Texture2D ? 0 : 1;
		Error += Texture != Texture2DArray ? 0 : 1;
		Error += Texture != Texture3D ? 0 : 1;
		Error += Texture != TextureCube ? 0 : 1;
		Error += Texture != TextureCubeArray ? 0 : 1;

		gli::texture Texture1D_B(Texture1D);
		gli::texture Texture1DArray_B(Texture1DArray);
		gli::texture Texture2D_B(Texture2D);
		gli::texture Texture2DArray_B(Texture2DArray);
		gli::texture Texture3D_B(Texture3D);
		gli::texture TextureCube_B(TextureCube);
		gli::texture TextureCubeArray_B(TextureCubeArray);

		Error += Texture == Texture1D_B ? 0 : 1;
		Error += Texture != Texture1DArray_B ? 0 : 1;
		Error += Texture != Texture2D_B ? 0 : 1;
		Error += Texture != Texture2DArray_B ? 0 : 1;
		Error += Texture != Texture3D_B ? 0 : 1;
		Error += Texture != TextureCube_B ? 0 : 1;
		Error += Texture != TextureCubeArray_B ? 0 : 1;

		Error += Texture1D == Texture1D_B ? 0 : 1;
		Error += Texture1DArray == Texture1DArray_B ? 0 : 1;
		Error += Texture2D == Texture2D_B ? 0 : 1;
		Error += Texture2DArray == Texture2DArray_B ? 0 : 1;
		Error += Texture3D == Texture3D_B ? 0 : 1;
		Error += TextureCube == TextureCube_B ? 0 : 1;
		Error += TextureCubeArray == TextureCubeArray_B ? 0 : 1;

		return Error;
	}
}//namespace specialize

namespace load
{
	int run()
	{
		int Error = 0;

		// Texture 1D
		{
			gli::texture Texture(gli::TARGET_1D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);
			Texture.clear(glm::u8vec4(225, 127, 0, 255));

			gli::save(Texture, "texture_1d.ktx");
			gli::save(Texture, "texture_1d.dds");
			gli::texture TextureKTX = gli::load("texture_1d.ktx");
			gli::texture TextureDDS = gli::load("texture_1d.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		// Texture 1D array
		{
			gli::texture Texture(gli::TARGET_1D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 2, 1, 1);
			Texture.clear(glm::u8vec4(225, 127, 0, 255));
			gli::save(Texture, "texture_1d_array.ktx");
			gli::save(Texture, "texture_1d_array.dds");
			gli::texture TextureKTX = gli::load("texture_1d_array.ktx");
			gli::texture TextureDDS = gli::load("texture_1d_array.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		// Texture 2D
		{
			gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);
			Texture.clear(glm::u8vec4(225, 127, 0, 255));

			gli::save(Texture, "texture_2d.ktx");
			gli::save(Texture, "texture_2d.dds");
			gli::texture TextureKTX = gli::load("texture_2d.ktx");
			gli::texture TextureDDS = gli::load("texture_2d.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		// Texture 2D array
		{
			gli::texture Texture(gli::TARGET_2D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 2, 1, 1);
			Texture.clear(glm::u8vec4(225, 127, 0, 255));
			gli::save(Texture, "texture_2d_array.ktx");
			gli::save(Texture, "texture_2d_array.dds");
			gli::texture TextureKTX = gli::load("texture_2d_array.ktx");
			gli::texture TextureDDS = gli::load("texture_2d_array.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		// Texture 3D
		{
			gli::texture Texture(gli::TARGET_3D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 1, 1);
			gli::save(Texture, "texture_3d.ktx");
			gli::save(Texture, "texture_3d.dds");
			gli::texture TextureKTX = gli::load("texture_3d.ktx");
			gli::texture TextureDDS = gli::load("texture_3d.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		// Texture cube
		{
			gli::texture Texture(gli::TARGET_CUBE, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 1, 6, 1);
			Texture.clear(glm::u8vec4(225, 127, 0, 255));
			gli::save(Texture, "texture_cube.ktx");
			gli::save(Texture, "texture_cube.dds");
			gli::texture TextureKTX = gli::load("texture_cube.ktx");
			gli::texture TextureDDS = gli::load("texture_cube.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		// Texture cube array
		{
			gli::texture Texture(gli::TARGET_CUBE_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 2, 6, 1);
			Texture.clear(glm::u8vec4(225, 127, 0, 255));
			gli::save(Texture, "texture_cube_array.ktx");
			gli::save(Texture, "texture_cube_array.dds");
			gli::texture TextureKTX = gli::load("texture_cube_array.ktx");
			gli::texture TextureDDS = gli::load("texture_cube_array.dds");

			Error += Texture == TextureKTX ? 0 : 1;
			Error += Texture == TextureDDS ? 0 : 1;
		}

		return Error;
	}
}//namespace load

namespace data
{
	int run()
	{
		int Error = 0;

		gli::texture Texture(gli::TARGET_2D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(1), 2, 1, 1);
		Error += gli::texture2DArray(Texture)[0].data() == Texture.data(0, 0, 0) ? 0 : 1;
		Error += gli::texture2DArray(Texture)[1].data() == Texture.data(1, 0, 0) ? 0 : 1;

		return Error;
	}
}//namespace data

int main()
{
	int Error(0);

	Error += alloc::run();
	Error += size::run();
	Error += query::run();
	Error += clear::run();
	Error += tex_access::run();
	Error += specialize::run();
	Error += load::run();
	Error += data::run();

	return Error;
}


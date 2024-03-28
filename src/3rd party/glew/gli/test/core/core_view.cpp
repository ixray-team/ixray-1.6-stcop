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
/// @file gli/core/core_view.cpp
/// @date 2013-02-03 / 2013-02-03
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/gli.hpp>
#include <gli/levels.hpp>

namespace dim
{
	int test_view1D
	(
		std::vector<gli::format> const & Formats,
		gli::texture1D::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::texture1D TextureA(Formats[i], TextureSize);
			gli::texture1D TextureViewA(gli::view(
				TextureA, TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::texture1D TextureViewC(gli::view(
				TextureA, TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewC ? 0 : 1;
			Error += TextureViewA == TextureViewC ? 0 : 1;

			gli::texture1D TextureB(Formats[i], TextureSize / gli::texture1D::texelcoord_type(2));
			gli::texture1D TextureViewB(gli::view(
				TextureA, TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureB == TextureViewB ? 0 : 1;

			gli::texture1D TextureViewD(gli::view(
				TextureA, TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureB == TextureViewD ? 0 : 1;
			Error += TextureViewB == TextureViewD ? 0 : 1;

			gli::texture1D TextureD(gli::view(
				TextureA, 1, 3));

			Error += TextureA[1] == TextureD[0] ? 0 : 1;
			Error += TextureA[2] == TextureD[1] ? 0 : 1;

			gli::texture1D TextureE(gli::view(
				TextureD, 1, 1));

			Error += TextureE[0] == TextureD[1] ? 0 : 1;
			Error += TextureE[0] == TextureA[2] ? 0 : 1;
		}

		return Error;
	}

	int test_view1DArray
	(
		std::vector<gli::format> const & Formats,
		gli::texture1DArray::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::texture1DArray TextureA(Formats[i], TextureSize, gli::texture1DArray::size_type(4));

			gli::texture1DArray TextureViewA(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::texture1DArray TextureViewC(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level(), TextureA.max_level()));
		
			Error += TextureA == TextureViewC ? 0 : 1;
			Error += TextureViewC == TextureViewA ? 0 : 1;

			gli::texture1DArray TextureB(
				Formats[i], TextureSize / gli::texture1DArray::texelcoord_type(2), gli::texture1DArray::size_type(4));

			Error += TextureA != TextureB ? 0 : 1;

			gli::texture1DArray TextureViewB(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureA != TextureViewB ? 0 : 1;
			Error += TextureB == TextureViewB ? 0 : 1;

			gli::texture1DArray TextureViewD(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureViewD == TextureViewB ? 0 : 1;

			gli::texture1DArray TextureD(gli::view(
				TextureA, 0, TextureA.layers() -1, 1, 3));

			Error += TextureA[0][1] == TextureD[0][0] ? 0 : 1;
			Error += TextureA[0][2] == TextureD[0][1] ? 0 : 1;

			gli::texture1DArray TextureE(gli::view(
				TextureD, 0, TextureD.layers() -1, 0, TextureD.levels() - 1));

			Error += TextureE == TextureD ? 0 : 1;
			Error += TextureE[0] == TextureD[0] ? 0 : 1;

			gli::texture1DArray TextureF(gli::view(
				TextureE, 1, 3, 0, TextureE.levels() - 1));

			Error += TextureF[0] == TextureD[1] ? 0 : 1;
			Error += TextureF[0] == TextureE[1] ? 0 : 1;
		}

		return Error;
	}

	int test_view2D
	(
		std::vector<gli::format> const & Formats,
		gli::texture2D::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::texture2D TextureA(Formats[i], TextureSize, gli::levels(TextureSize));

			for(std::size_t Index = 0; Index < TextureA.size(); ++Index)
				*(TextureA.data<glm::byte>() + Index) = glm::byte(Index);

			gli::texture2D TextureViewA(gli::view(
				TextureA, TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::texture2D TextureD(
				gli::view(TextureA, 1, 3));

			Error += TextureA[1] == TextureD[0] ? 0 : 1;
			Error += TextureA[2] == TextureD[1] ? 0 : 1;

			gli::texture2D TextureE(TextureD, 1, 1);

			Error += TextureE[0] == TextureD[1] ? 0 : 1;
			Error += TextureE[0] == TextureA[2] ? 0 : 1;

			gli::texture2D TextureViewB(gli::view(
				TextureA,
				TextureA.base_level() + 1, TextureA.max_level()));

			gli::texture2D TextureViewD(gli::view(
				TextureA,
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureViewD == TextureViewB ? 0 : 1;
		}

		return Error;
	}

	int test_view2DArray
	(
		std::vector<gli::format> const & Formats,
		gli::texture2DArray::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::texture2DArray TextureA(Formats[i], TextureSize, 4);

			gli::texture2DArray TextureViewA(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::texture2DArray TextureB(Formats[i], TextureSize / gli::texture2DArray::texelcoord_type(2), 4);

			gli::texture2DArray TextureViewB(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level() + 1, TextureA.max_level()));

			gli::texture2DArray TextureViewD(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureViewB == TextureViewD ? 0 : 1;
			Error += TextureB == TextureViewB ? 0 : 1;

			gli::texture2DArray TextureD(gli::view(
				TextureA, 0, TextureA.layers() -1, 1, 3));

			Error += TextureA[0][1] == TextureD[0][0] ? 0 : 1;
			Error += TextureA[0][2] == TextureD[0][1] ? 0 : 1;

			gli::texture2DArray TextureE(gli::view(
				TextureD, 0, TextureD.layers() -1, 0, TextureD.levels() - 1));

			Error += TextureE == TextureD ? 0 : 1;
			Error += TextureE[0] == TextureD[0] ? 0 : 1;

			gli::texture2DArray TextureF(gli::view(
				TextureE, 1, 3, 0, TextureE.levels() - 1));

			Error += TextureF[0] == TextureD[1] ? 0 : 1;
			Error += TextureF[0] == TextureE[1] ? 0 : 1;
		}

		return Error;
	}

	int test_view3D
	(
		std::vector<gli::format> const & Formats, 
		gli::texture3D::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::texture3D TextureA(Formats[i], TextureSize, gli::levels(TextureSize));
			gli::texture3D TextureViewA(gli::view(
				TextureA, TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::texture3D::texelcoord_type SizeB(TextureSize / gli::texture3D::texelcoord_type(2));
			gli::texture3D TextureB(Formats[i], SizeB, gli::levels(SizeB));

			gli::texture3D TextureViewB(gli::view(
				TextureA, TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureB == TextureViewB ? 0 : 1;

			gli::texture3D TextureViewD(gli::view(
				TextureA, TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureViewD == TextureViewB ? 0 : 1;

			gli::texture3D TextureD(gli::view(TextureA, 1, 3));

			Error += TextureA[1] == TextureD[0] ? 0 : 1;
			Error += TextureA[2] == TextureD[1] ? 0 : 1;

			gli::texture3D TextureE(gli::view(TextureD, 1, 1));

			Error += TextureE[0] == TextureD[1] ? 0 : 1;
			Error += TextureE[0] == TextureA[2] ? 0 : 1;
		}

		return Error;
	}

	int test_viewCube
	(
		std::vector<gli::format> const & Formats, 
		gli::textureCube::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::textureCube TextureA(Formats[i], TextureSize);

			gli::textureCube TextureViewA(gli::view(
				TextureA,
				TextureA.base_face(), TextureA.max_face(),
				TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::textureCube::texelcoord_type SizeB(TextureSize / gli::textureCube::texelcoord_type(2));
			gli::textureCube TextureB(Formats[i], SizeB);

			gli::textureCube TextureViewB(gli::view(
				TextureA,
				TextureA.base_face(), TextureA.max_face(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureB == TextureViewB ? 0 : 1;

			gli::textureCube TextureViewD(gli::view(
				TextureA,
				TextureA.base_face(), TextureA.max_face(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureViewD == TextureViewB ? 0 : 1;

			gli::textureCube TextureD(gli::view(
				TextureA, 0, TextureA.faces() -1, 1, 3));

			Error += TextureA[0][1] == TextureD[0][0] ? 0 : 1;
			Error += TextureA[0][2] == TextureD[0][1] ? 0 : 1;

			gli::textureCube TextureE(gli::view(
				TextureD, 0, TextureD.faces() -1, 0, TextureD.levels() - 1));

			Error += TextureE == TextureD ? 0 : 1;
			Error += TextureE[0] == TextureD[0] ? 0 : 1;

			gli::textureCube TextureF(gli::view(
				TextureE, 1, 3, 0, TextureE.levels() - 1));

			Error += TextureF[0] == TextureD[1] ? 0 : 1;
			Error += TextureF[0] == TextureE[1] ? 0 : 1;
		}

		return Error;
	}

	int test_viewCubeArray
	(
		std::vector<gli::format> const & Formats,
		gli::textureCubeArray::texelcoord_type const & TextureSize
	)
	{
		int Error(0);

		for(std::size_t i = 0; i < Formats.size(); ++i)
		{
			gli::textureCubeArray TextureA(
				Formats[i],
				TextureSize,
				4);

			gli::textureCubeArray TextureViewA(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_face(), TextureA.max_face(),
				TextureA.base_level(), TextureA.max_level()));

			Error += TextureA == TextureViewA ? 0 : 1;

			gli::textureCubeArray::texelcoord_type SizeB(TextureSize / gli::textureCubeArray::texelcoord_type(2));
			gli::textureCubeArray TextureB(
				Formats[i],
				SizeB,
				gli::textureCubeArray::size_type(4));

			gli::textureCubeArray TextureViewB(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_face(), TextureA.max_face(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureB == TextureViewB ? 0 : 1;

			gli::textureCubeArray TextureViewD(gli::view(
				TextureA,
				TextureA.base_layer(), TextureA.max_layer(),
				TextureA.base_face(), TextureA.max_face(),
				TextureA.base_level() + 1, TextureA.max_level()));

			Error += TextureViewD == TextureViewB ? 0 : 1;

			gli::textureCubeArray TextureD(gli::view(
				TextureA,
				0, TextureA.layers() -1,
				0, TextureA.faces() -1,
				1, 3));

			Error += TextureA[0][0][1] == TextureD[0][0][0] ? 0 : 1;
			Error += TextureA[0][0][2] == TextureD[0][0][1] ? 0 : 1;

			gli::textureCubeArray TextureE(gli::view(
				TextureD,
				0, TextureA.layers() -1,
				0, TextureD.faces() -1,
				0, TextureD.levels() - 1));

			Error += TextureE == TextureD ? 0 : 1;
			Error += TextureE[0] == TextureD[0] ? 0 : 1;
			Error += TextureE[1] == TextureD[1] ? 0 : 1;

			gli::textureCubeArray TextureF(gli::view(
				TextureE,
				0, TextureA.layers() -1,
				1, 3,
				0, TextureE.levels() - 1));

			Error += TextureF[0][0] == TextureD[0][1] ? 0 : 1;
			Error += TextureF[1][0] == TextureD[1][1] ? 0 : 1;
			Error += TextureF[0][0] == TextureE[0][1] ? 0 : 1;
			Error += TextureF[1][0] == TextureE[1][1] ? 0 : 1;
		}

		return Error;
	}

	int run()
	{
		int Error(0);

		std::vector<gli::format> FormatsA;
		FormatsA.push_back(gli::FORMAT_RGBA8_UNORM_PACK8);
		FormatsA.push_back(gli::FORMAT_RGB8_UNORM_PACK8);
		FormatsA.push_back(gli::FORMAT_R8_SNORM_PACK8);
		FormatsA.push_back(gli::FORMAT_RGBA32_SFLOAT_PACK32);
		FormatsA.push_back(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8);
		FormatsA.push_back(gli::FORMAT_RGBA_BP_UNORM_BLOCK16);

		// 1D textures don't support compressed formats
		std::vector<gli::format> FormatsB;
		FormatsA.push_back(gli::FORMAT_RGBA8_UNORM_PACK8);
		FormatsA.push_back(gli::FORMAT_RGB8_UNORM_PACK8);
		FormatsA.push_back(gli::FORMAT_R8_SNORM_PACK8);
		FormatsA.push_back(gli::FORMAT_RGBA32_SFLOAT_PACK32);

		std::size_t const TextureSize(32);

		Error += test_view1D(FormatsB, gli::texture1D::texelcoord_type(TextureSize));
		Error += test_view1DArray(FormatsB, gli::texture1DArray::texelcoord_type(TextureSize));
		Error += test_view2D(FormatsA, gli::texture2D::texelcoord_type(TextureSize));
		Error += test_view2DArray(FormatsA, gli::texture2DArray::texelcoord_type(TextureSize));
		Error += test_view3D(FormatsA, gli::texture3D::texelcoord_type(TextureSize));
		Error += test_viewCube(FormatsA, gli::textureCube::texelcoord_type(TextureSize));
		Error += test_viewCubeArray(FormatsA, gli::textureCube::texelcoord_type(TextureSize));

		return Error;
	}
}//namespace dim

namespace format
{
	int run()
	{
		int Error = 0;

		{
			gli::texture2D TextureA(gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture2D::texelcoord_type(1));
			gli::texture2D TextureB(gli::view(TextureA, gli::FORMAT_R32_UINT_PACK32));
			gli::texture2D TextureC(gli::view(TextureA));

			Error += TextureA.dimensions() == TextureB.dimensions() ? 0 : 1;
		}

		{
			gli::texture TextureA(gli::TARGET_2D, gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, gli::texture::texelcoord_type(4, 4, 1), 1, 1, 3);
			gli::texture TextureB(gli::view(TextureA, gli::FORMAT_RG32_UINT_PACK32));
			gli::texture TextureC(gli::TARGET_2D, gli::FORMAT_RG32_UINT_PACK32, gli::texture::texelcoord_type(1), 1, 1, 3);

			gli::texture::texelcoord_type const DimensionsA0 = TextureA.dimensions(0);
			gli::texture::texelcoord_type const DimensionsB0 = TextureB.dimensions(0);
			gli::texture::texelcoord_type const DimensionsC0 = TextureC.dimensions(0);
			gli::texture::texelcoord_type const DimensionsA1 = TextureA.dimensions(1);
			gli::texture::texelcoord_type const DimensionsB1 = TextureB.dimensions(1);
			gli::texture::texelcoord_type const DimensionsC1 = TextureC.dimensions(1);
			gli::texture::texelcoord_type const DimensionsA2 = TextureA.dimensions(2);
			gli::texture::texelcoord_type const DimensionsB2 = TextureB.dimensions(2);
			gli::texture::texelcoord_type const DimensionsC2 = TextureC.dimensions(2);

			Error += DimensionsA0 == gli::texture::texelcoord_type(4, 4, 1) ? 0 : 1;
			Error += DimensionsB0 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;
			Error += DimensionsC0 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;
			Error += DimensionsA1 == gli::texture::texelcoord_type(2, 2, 1) ? 0 : 1;
			Error += DimensionsB1 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;
			Error += DimensionsC1 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;
			Error += DimensionsA2 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;
			Error += DimensionsB2 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;
			Error += DimensionsC2 == gli::texture::texelcoord_type(1, 1, 1) ? 0 : 1;

			gli::texture::size_type const SizeA = TextureA.size();
			gli::texture::size_type const SizeB = TextureB.size();
			gli::texture::size_type const SizeC = TextureC.size();

			Error += SizeA == gli::texture::size_type(gli::block_size(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8) * 3) ? 0 : 1;
			Error += SizeB == gli::texture::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32) * 3) ? 0 : 1;
			Error += SizeC == gli::texture::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32) * 3) ? 0 : 1;
		}

		{
			gli::texture2D TextureA(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, gli::texture2D::texelcoord_type(4));
			gli::texture2D TextureB(gli::view(TextureA, gli::FORMAT_RG32_UINT_PACK32));
			gli::texture2D TextureC(gli::FORMAT_RG32_UINT_PACK32, gli::texture2D::texelcoord_type(1), 3);

			gli::texture2D::texelcoord_type const DimensionsA = TextureA.dimensions();
			gli::texture2D::texelcoord_type const DimensionsB = TextureB.dimensions();

			Error += TextureA.size() == TextureB.size() ? 0 : 1;
			Error += TextureA.size() == TextureC.size() ? 0 : 1;
			Error += TextureB == TextureC ? 0 : 1;
			Error += DimensionsA == gli::texture2D::texelcoord_type(4) ? 0 : 1;
			Error += DimensionsB == gli::texture2D::texelcoord_type(1) ? 0 : 1;
			Error += DimensionsA != DimensionsB ? 0 : 1;

			{
				gli::texture2D::texelcoord_type const DimensionsA0 = TextureA.dimensions(0);
				gli::texture2D::texelcoord_type const DimensionsB0 = TextureB.dimensions(0);
				gli::texture2D::texelcoord_type const DimensionsC0 = TextureC.dimensions(0);
				gli::texture2D::texelcoord_type const DimensionsA1 = TextureA.dimensions(1);
				gli::texture2D::texelcoord_type const DimensionsB1 = TextureB.dimensions(1);
				gli::texture2D::texelcoord_type const DimensionsC1 = TextureC.dimensions(1);
				gli::texture2D::texelcoord_type const DimensionsA2 = TextureA.dimensions(2);
				gli::texture2D::texelcoord_type const DimensionsB2 = TextureB.dimensions(2);
				gli::texture2D::texelcoord_type const DimensionsC2 = TextureC.dimensions(2);

				Error += DimensionsA0 == gli::texture2D::texelcoord_type(4, 4) ? 0 : 1;
				Error += DimensionsB0 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;
				Error += DimensionsC0 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;
				Error += DimensionsA1 == gli::texture2D::texelcoord_type(2, 2) ? 0 : 1;
				Error += DimensionsB1 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;
				Error += DimensionsC1 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;
				Error += DimensionsA2 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;
				Error += DimensionsB2 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;
				Error += DimensionsC2 == gli::texture2D::texelcoord_type(1, 1) ? 0 : 1;

				gli::texture2D::size_type const SizeA = TextureA.size();
				gli::texture2D::size_type const SizeB = TextureB.size();
				gli::texture2D::size_type const SizeC = TextureC.size();

				Error += SizeA == gli::texture::size_type(gli::block_size(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8) * 3) ? 0 : 1;
				Error += SizeB == gli::texture::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32) * 3) ? 0 : 1;
				Error += SizeC == gli::texture::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32) * 3) ? 0 : 1;
			}

			{
				gli::image const ImageA0 = TextureA[0];
				gli::image const ImageA1 = TextureA[1];
				gli::image const ImageA2 = TextureA[2];

				gli::image const ImageB0 = TextureB[0];
				gli::image const ImageB1 = TextureB[1];
				gli::image const ImageB2 = TextureB[2];

				gli::image const ImageC0 = TextureC[0];
				gli::image const ImageC1 = TextureC[1];
				gli::image const ImageC2 = TextureC[2];

				gli::image::texelcoord_type const DimensionsA0 = TextureA[0].dimensions();
				gli::image::texelcoord_type const DimensionsB0 = TextureB[0].dimensions();
				gli::image::texelcoord_type const DimensionsC0 = TextureC[0].dimensions();
				gli::image::texelcoord_type const DimensionsA1 = TextureA[1].dimensions();
				gli::image::texelcoord_type const DimensionsB1 = TextureB[1].dimensions();
				gli::image::texelcoord_type const DimensionsC1 = TextureC[1].dimensions();
				gli::image::texelcoord_type const DimensionsA2 = TextureA[2].dimensions();
				gli::image::texelcoord_type const DimensionsB2 = TextureB[2].dimensions();
				gli::image::texelcoord_type const DimensionsC2 = TextureC[2].dimensions();

				Error += DimensionsA0 == gli::image::texelcoord_type(4, 4, 1) ? 0 : 1;
				Error += DimensionsB0 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;
				Error += DimensionsC0 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;
				Error += DimensionsA1 == gli::image::texelcoord_type(2, 2, 1) ? 0 : 1;
				Error += DimensionsB1 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;
				Error += DimensionsC1 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;
				Error += DimensionsA2 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;
				Error += DimensionsB2 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;
				Error += DimensionsC2 == gli::image::texelcoord_type(1, 1, 1) ? 0 : 1;

				gli::image::size_type const SizeA0 = ImageA0.size();
				gli::image::size_type const SizeA1 = ImageA1.size();
				gli::image::size_type const SizeA2 = ImageA2.size();

				Error += SizeA0 == gli::image::size_type(gli::block_size(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8)) ? 0 : 1;
				Error += SizeA1 == gli::image::size_type(gli::block_size(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8)) ? 0 : 1;
				Error += SizeA2 == gli::image::size_type(gli::block_size(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8)) ? 0 : 1;

				gli::image::size_type const SizeB0 = ImageB0.size();
				gli::image::size_type const SizeB1 = ImageB1.size();
				gli::image::size_type const SizeB2 = ImageB2.size();

				Error += SizeB0 == gli::image::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32)) ? 0 : 1;
				Error += SizeB1 == gli::image::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32)) ? 0 : 1;
				Error += SizeB2 == gli::image::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32)) ? 0 : 1;

				gli::image::size_type const SizeC0 = ImageC0.size();
				gli::image::size_type const SizeC1 = ImageC1.size();
				gli::image::size_type const SizeC2 = ImageC2.size();

				Error += SizeC0 == gli::image::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32)) ? 0 : 1;
				Error += SizeC1 == gli::image::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32)) ? 0 : 1;
				Error += SizeC2 == gli::image::size_type(gli::block_size(gli::FORMAT_RG32_UINT_PACK32)) ? 0 : 1;
			}
		}

		{
			gli::texture2D TextureA(gli::FORMAT_RGBA_DXT5_UNORM_BLOCK16, gli::texture2D::texelcoord_type(4));
			gli::texture2D TextureB(gli::view(TextureA, gli::FORMAT_RGBA32_UINT_PACK32));
			gli::texture2D TextureC(gli::FORMAT_RGBA32_UINT_PACK32, gli::texture2D::texelcoord_type(1), 3);
			gli::texture2D TextureD(gli::view(TextureC, gli::FORMAT_RGBA_DXT5_UNORM_BLOCK16));

			Error += TextureA == TextureD ? 0 : 1;
			Error += TextureB == TextureC ? 0 : 1;

			Error += TextureA.dimensions() == gli::texture2D::texelcoord_type(4) ? 0 : 1;
			Error += TextureB.dimensions() == gli::texture2D::texelcoord_type(1) ? 0 : 1;
			Error += TextureA.dimensions() != TextureB.dimensions() ? 0 : 1;
		}

		{
			gli::texture2D TextureA(gli::FORMAT_RG32_UINT_PACK32, gli::texture2D::texelcoord_type(4));
			gli::texture2D TextureB(gli::view(TextureA, gli::FORMAT_RG32_UINT_PACK32));
			gli::texture2D TextureC(gli::view(TextureA, gli::FORMAT_R32_UINT_PACK32));
			gli::texture2D TextureD(gli::view(TextureA, gli::FORMAT_RGB32_UINT_PACK32));
			gli::texture2D TextureE(gli::view(TextureA, gli::FORMAT_RGBA32_UINT_PACK32));

			Error += TextureA == TextureB ? 0 : 1;
			Error += TextureC.empty() ? 0 : 1;
			Error += TextureD.empty() ? 0 : 1;
			Error += TextureE.empty() ? 0 : 1;
		}

		return Error;
	}
}//namespace format

namespace clear2d
{
	int run()
	{
		int Error = 0;

		glm::u8vec4 const Black(0, 0, 0, 255);
		glm::u8vec4 const Color(255, 127, 0, 255);

		gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(8, 8, 1), 1, 1, 5);
		Texture.clear(Black);

		Texture.clear<glm::u8vec4>(0, 0, 1, glm::u8vec4(255, 127, 0, 255));

		gli::texture TextureView(gli::view(Texture, 0, 0, 0, 0, 1, 1));
		gli::texture TextureCopy(gli::copy(TextureView));
		Error += TextureView == TextureCopy ? 0 : 1;

		gli::texture TextureImage(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 1, 1);
		TextureImage.clear(Color);

		Error += TextureView == TextureImage ? 0 : 1;
		Error += TextureView.size() == TextureImage.size() ? 0 : 1;
		Error += TextureView.size<glm::u8vec4>() == TextureImage.size<glm::u8vec4>() ? 0 : 1;

		return Error;
	}
}//namespace clear2d

namespace clear2d_array
{
	int run()
	{
		int Error = 0;

		glm::u8vec4 const Black(0, 0, 0, 255);
		glm::u8vec4 const Color(255, 127, 0, 255);

		gli::texture Texture(gli::TARGET_2D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(8, 8, 1), 3, 1, 5);
		Texture.clear(Black);

		Texture.clear<glm::u8vec4>(1, 0, 1, glm::u8vec4(255, 127, 0, 255));

		gli::texture TextureView(gli::view(Texture, 1, 1, 0, 0, 1, 1));
		gli::texture TextureCopy(gli::copy(TextureView));
		Error += TextureView == TextureCopy ? 0 : 1;

		gli::texture TextureImage(gli::TARGET_2D_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 1, 1);
		TextureImage.clear(Color);

		Error += TextureView == TextureImage ? 0 : 1;
		Error += TextureView.size() == TextureImage.size() ? 0 : 1;
		Error += TextureView.size<glm::u8vec4>() == TextureImage.size<glm::u8vec4>() ? 0 : 1;

		return Error;
	}
}//namespace clear2d_array

namespace clear_cube
{
	int run()
	{
		int Error = 0;

		glm::u8vec4 const Black(0, 0, 0, 255);
		glm::u8vec4 const Color(255, 127, 0, 255);

		gli::texture Texture(gli::TARGET_CUBE, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(8, 8, 1), 1, 6, 5);
		Texture.clear(Black);

		for(gli::texture::size_type FaceIndex = 0; FaceIndex < 6; ++FaceIndex)
			Texture.clear<glm::u8vec4>(0, FaceIndex, 1, glm::u8vec4(255, 127, 0, 255));

		gli::texture TextureView(gli::view(Texture, 0, 0, 0, 5, 1, 1));
		gli::texture TextureCopy(gli::copy(TextureView));
		Error += TextureView == TextureCopy ? 0 : 1;

		gli::texture TextureImage(gli::TARGET_CUBE, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 6, 1);
		TextureImage.clear(Color);

		Error += TextureView == TextureImage ? 0 : 1;
		Error += TextureView.size() == TextureImage.size() ? 0 : 1;
		Error += TextureView.size<glm::u8vec4>() == TextureImage.size<glm::u8vec4>() ? 0 : 1;

		return Error;
	}
}//namespace clear_cube

namespace clear_cube_array
{
	int run()
	{
		int Error = 0;

		glm::u8vec4 const Black(0, 0, 0, 255);
		glm::u8vec4 const Color(255, 127, 0, 255);

		gli::texture Texture(gli::TARGET_CUBE_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(8, 8, 1), 3, 6, 5);
		Texture.clear(Black);

		for(gli::texture::size_type FaceIndex = 0; FaceIndex < 6; ++FaceIndex)
			Texture.clear<glm::u8vec4>(1, FaceIndex, 1, glm::u8vec4(255, 127, 0, 255));

		gli::texture TextureView(gli::view(Texture, 1, 1, 0, 5, 1, 1));
		gli::texture TextureCopy(gli::copy(TextureView));
		Error += TextureView == TextureCopy ? 0 : 1;

		gli::texture TextureImage(gli::TARGET_CUBE_ARRAY, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 6, 1);
		TextureImage.clear(Color);

		Error += TextureView == TextureImage ? 0 : 1;
		Error += TextureView.size() == TextureImage.size() ? 0 : 1;
		Error += TextureView.size<glm::u8vec4>() == TextureImage.size<glm::u8vec4>() ? 0 : 1;

		return Error;
	}
}//namespace clear_cube_array

namespace size
{
	int run()
	{
		int Error = 0;

		gli::texture Texture(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(8, 8, 1), 1, 1, 5);

		gli::texture TextureView(gli::view(Texture, 0, 0, 0, 0, 1, 1));

		gli::texture TextureImage(gli::TARGET_2D, gli::FORMAT_RGBA8_UNORM_PACK8, gli::texture::texelcoord_type(4, 4, 1), 1, 1, 1);

		Error += TextureView.size() == TextureImage.size() ? 0 : 1;

		return Error;
	}
}//namespace size

int main()
{
	int Error = 0;

	Error += dim::run();
	Error += format::run();
	Error += clear2d::run();
	Error += clear2d_array::run();
	Error += clear_cube::run();
	Error += clear_cube_array::run();
	Error += size::run();

	return Error;
}

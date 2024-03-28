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
/// @file gli/core/core_addressing.cpp
/// @date 2012-11-19 / 2013-11-25
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/core/storage.hpp>
#include <gli/format.hpp>

namespace layers
{
	struct test
	{
		test
		(
			gli::storage::texelcoord_type const & Dimensions,
			gli::format const & Format,
			std::size_t const & Offset,
			std::size_t const & Size
		) :
			Dimensions(Dimensions),
			Format(Format),
			Offset(Offset),
			Size(Size)
		{}

		gli::storage::texelcoord_type Dimensions;
		gli::format Format;
		std::size_t Offset;
		std::size_t Size;
	};

	int run()
	{
		int Error(0);

		std::vector<test> Tests;
		Tests.push_back(test(gli::storage::texelcoord_type(4, 4, 1), gli::FORMAT_RGBA8_UINT_PACK8, 64, 128));
		Tests.push_back(test(gli::storage::texelcoord_type(4, 4, 1), gli::FORMAT_RGB16_SFLOAT_PACK16, 96, 192));
		Tests.push_back(test(gli::storage::texelcoord_type(4, 4, 1), gli::FORMAT_RGBA32_SFLOAT_PACK32, 256, 512));
		Tests.push_back(test(gli::storage::texelcoord_type(4, 4, 1), gli::FORMAT_RGBA_DXT1_UNORM_BLOCK8, 8, 16));
		Tests.push_back(test(gli::storage::texelcoord_type(8, 8, 1), gli::FORMAT_RGBA_DXT1_UNORM_BLOCK8, 32, 64));
		Tests.push_back(test(gli::storage::texelcoord_type(4, 4, 1), gli::FORMAT_R_ATI1N_SNORM_BLOCK8, 8, 16));

		for(std::size_t i = 0; i < Tests.size(); ++i)
		{
			gli::storage Storage(
				Tests[i].Format,
				Tests[i].Dimensions,
				2,
				1,
				1);

			gli::storage::size_type const Offset = Storage.offset(1, 0, 0);
			gli::storage::size_type const Size = Storage.size();

			Error += Offset == Tests[i].Offset ? 0 : 1;
			Error += Size == Tests[i].Size ? 0 : 1;
		}

		return Error;
	}
}//namespace layers

namespace faces
{
	struct test
	{
		test
		(
			gli::format const & Format,
			std::size_t const & Level,
			std::size_t const & Offset,
			std::size_t const & Size
		) :
			Format(Format),
			Level(Level),
			Offset(Offset),
			Size(Size)
		{}

		gli::format Format;
		std::size_t Level;
		std::size_t Offset;
		std::size_t Size;
	};

	int run()
	{
		int Error(0);

		std::vector<test> Tests;
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, 0, 0, 340));
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, 1, 256, 340));
		Tests.push_back(test(gli::FORMAT_R8_UINT_PACK8, 1, 64, 85));
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, 3, 336, 340));
		Tests.push_back(test(gli::FORMAT_RGBA32_SFLOAT_PACK32, 0, 0, 1360));
		Tests.push_back(test(gli::FORMAT_RGBA32_SFLOAT_PACK32, 1, 1024, 1360));
		Tests.push_back(test(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, 0, 0, 56));
		Tests.push_back(test(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, 1, 32, 56));
		Tests.push_back(test(gli::FORMAT_RGBA_DXT5_UNORM_BLOCK16, 1, 64, 112));

		for(std::size_t i = 0; i < Tests.size(); ++i)
		{
			gli::storage Storage(Tests[i].Format, gli::storage::texelcoord_type(8, 8, 1), 1, 1, 4);
			gli::storage::size_type Offset = Storage.offset(0, 0, Tests[i].Level);
			gli::storage::size_type Size = Storage.size();

			Error += Offset == Tests[i].Offset ? 0 : 1;
			Error += Size == Tests[i].Size ? 0 : 1;
		}

		return Error;
	}
}//namespace faces

namespace levels
{
	struct test
	{
		test
		(
			gli::format const & Format,
			std::size_t const & Level,
			std::size_t const & Offset,
			std::size_t const & Size
		) :
			Format(Format),
			Level(Level),
			Offset(Offset),
			Size(Size)
		{}

		gli::format Format;
		std::size_t Level;
		std::size_t Offset;
		std::size_t Size;
	};

	int run()
	{
		int Error(0);

		std::vector<test> Tests;
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, 0, 0, 340));
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, 1, 256, 340));
		Tests.push_back(test(gli::FORMAT_RGBA8_UINT_PACK8, 3, 336, 340));
		Tests.push_back(test(gli::FORMAT_RGBA32_SFLOAT_PACK32, 0, 0, 1360));
		Tests.push_back(test(gli::FORMAT_RGBA32_SFLOAT_PACK32, 1, 1024, 1360));
		Tests.push_back(test(gli::FORMAT_RGB_DXT1_UNORM_BLOCK8, 0, 0, 56));
		Tests.push_back(test(gli::FORMAT_RGBA_DXT1_UNORM_BLOCK8, 1, 32, 56));

		for(std::size_t i = 0; i < Tests.size(); ++i)
		{
			gli::storage Storage(
				Tests[i].Format,
				gli::storage::texelcoord_type(8, 8, 1),
				1,
				1,
				4);

			gli::storage::size_type Offset = Storage.offset(0, 0, Tests[i].Level);
			gli::storage::size_type Size = Storage.size();

			Error += Offset == Tests[i].Offset ? 0 : 1;
			Error += Size == Tests[i].Size ? 0 : 1;
		}

		return Error;
	}
}//namespace levels

int main()
{
	int Error(0);

	Error += layers::run();
	Error += faces::run();
	Error += levels::run();

	return Error;
}

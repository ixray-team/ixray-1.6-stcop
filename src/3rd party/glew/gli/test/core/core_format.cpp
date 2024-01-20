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
/// @file gli/core/core_format.cpp
/// @date 2015-08-29 / 2015-08-29
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/format.hpp>

namespace valid
{
	int test()
	{
		int Error(0);

		for(std::size_t FormatIndex = gli::FORMAT_FIRST; FormatIndex < gli::FORMAT_COUNT; ++FormatIndex)
			Error += gli::is_valid(static_cast<gli::format>(FormatIndex)) ? 0 : 1;
		Error += !gli::is_valid(static_cast<gli::format>(gli::FORMAT_INVALID)) ? 0 : 1;

		return Error;
	}
}//namespace valid

namespace component
{
	int test()
	{
		int Error(0);

		for(std::size_t FormatIndex = gli::FORMAT_FIRST; FormatIndex < gli::FORMAT_COUNT; ++FormatIndex)
		{
			std::size_t const Components = gli::component_count(static_cast<gli::format>(FormatIndex));
			Error += Components > 0 && Components <= 4 ? 0 : 1;
			assert(!Error);
		}

		return Error;
	}
}//namespace component

namespace compressed
{
	int test()
	{
		int Error(0);

		Error += !gli::is_compressed(gli::FORMAT_R8_SRGB_PACK8) ? 0 : 1;
		Error += gli::is_compressed(gli::FORMAT_RGB_DXT1_SRGB_BLOCK8) ? 0 : 1;

		return Error;
	}
}//namespace format

namespace block
{
	int test()
	{
		int Error(0);

		Error += gli::block_size(gli::FORMAT_RGBA8_UNORM_PACK8) == 4 ? 0 : 1;
		Error += gli::block_size(gli::FORMAT_RGB10A2_UNORM_PACK32) == 4 ? 0 : 1;

		return Error;
	}
}//namespace block

int main()
{
	int Error(0);

	Error += valid::test();
	Error += component::test();
	Error += compressed::test();
	Error += block::test();

	return Error;
}

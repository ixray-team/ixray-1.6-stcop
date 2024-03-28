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
/// @file gli/test/core/generate_mipmaps_sampler1d_array.cpp
/// @date 2015-10-05 / 2015-10-05
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

#include <gli/comparison.hpp>
#include <gli/type.hpp>
#include <gli/view.hpp>
#include <gli/copy.hpp>
#include <gli/generate_mipmaps.hpp>

#include <glm/gtc/epsilon.hpp>

namespace generate_mipmaps
{
	template <typename genType>
	int test(gli::format Format, genType const & Black, genType const & Color, std::size_t Layers, std::size_t Size, gli::filter Filter)
	{
		int Error = 0;

		gli::texture1DArray Texture(Format, gli::texture1DArray::texelcoord_type(static_cast<gli::texture1DArray::texelcoord_type::value_type>(Size)), Layers);
		Texture.clear(Black);
		for(std::size_t Layer = 0; Layer < Layers; ++Layer)
			Texture[Layer][0].clear(Color);

		for(std::size_t Layer = 0; Layer < Layers; ++Layer)
		{
			genType const LoadC = Texture.load<genType>(gli::texture1DArray::texelcoord_type(0), Layer, Texture.max_level());
			if(Texture.levels() > 1)
				Error += LoadC == Black ? 0 : 1;

			gli::fsampler1DArray SamplerA(gli::texture1DArray(gli::copy(Texture)), gli::WRAP_CLAMP_TO_EDGE);
			SamplerA.generate_mipmaps(gli::FILTER_LINEAR);

			gli::texture1DArray MipmapsA = SamplerA();
			genType const LoadA = MipmapsA.load<genType>(gli::texture1DArray::texelcoord_type(0), Layer, MipmapsA.max_level());
			Error += LoadA == Color ? 0 : 1;
			if(Texture.levels() > 1)
				Error += LoadA != LoadC ? 0 : 1;

			// Mipmaps generation using the wrapper function
			gli::texture1DArray MipmapsB = gli::generate_mipmaps(gli::texture1DArray(gli::copy(Texture)), Filter);
			genType const LoadB = MipmapsB.load<genType>(gli::texture1DArray::texelcoord_type(0), Layer, MipmapsB.max_level());
			Error += LoadB == Color ? 0 : 1;
			if(Texture.levels() > 1)
				Error += LoadB != LoadC ? 0 : 1;
		}

		return Error;
	}
}//namespace generate_mipmaps

int main()
{
	int Error = 0;

	std::vector<gli::filter> Filters;
	Filters.push_back(gli::FILTER_NEAREST);
	Filters.push_back(gli::FILTER_LINEAR);

	std::vector<gli::size_t> Sizes;
	Sizes.push_back(2);
	Sizes.push_back(3);
	Sizes.push_back(15);
	Sizes.push_back(16);
	Sizes.push_back(17);
	Sizes.push_back(24);
	Sizes.push_back(32);
	Sizes.push_back(1);

	std::vector<gli::size_t> Layers;
	Layers.push_back(2);
	Layers.push_back(5);
	Layers.push_back(1);

	for(std::size_t FilterIndex = 0, FilterCount = Filters.size(); FilterIndex < FilterCount; ++FilterIndex)
	for(std::size_t LayerIndex = 0, LayerCount = Layers.size(); LayerIndex < LayerCount; ++LayerIndex)
	for(std::size_t SizeIndex = 0, SizeCount = Sizes.size(); SizeIndex < SizeCount; ++SizeIndex)
	{
		Error += generate_mipmaps::test(gli::FORMAT_RGBA8_UNORM_PACK8,
			glm::u8vec4(0, 0, 0, 0),
			glm::u8vec4(255, 127, 0, 255),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGBA8_SNORM_PACK8,
			glm::i8vec4(0, 0, 0, 0),
			glm::i8vec4(127, 63, 0, 1),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGBA4_UNORM_PACK16,
			gli::packUnorm4x4(glm::vec4(0.0f, 0.0f, 0.0f, 0.0f)),
			gli::packUnorm4x4(glm::vec4(1.0f, 0.5f, 0.0f, 1.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGB10A2_UNORM_PACK32,
			gli::packUnorm3x10_1x2(glm::vec4(0.0f, 0.0f, 0.0f, 0.0f)),
			gli::packUnorm3x10_1x2(glm::vec4(1.0f, 0.5f, 0.0f, 1.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGB10A2_SNORM_PACK32,
			gli::packSnorm3x10_1x2(glm::vec4(0.0f, 0.0f, 0.0f, 0.0f)),
			gli::packSnorm3x10_1x2(glm::vec4(1.0f, 0.5f, 0.0f, 1.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGB9E5_UFLOAT_PACK32,
			gli::packF3x9_E1x5(glm::vec3(0.0f, 0.0f, 0.0f)),
			gli::packF3x9_E1x5(glm::vec3(1.0f, 0.5f, 0.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_R16_SFLOAT_PACK16,
			gli::packHalf(glm::vec1(0.0f)),
			gli::packHalf(glm::vec1(1.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RG16_SFLOAT_PACK16,
			gli::packHalf(glm::vec2(0.0f, 0.0f)),
			gli::packHalf(glm::vec2(1.0f, 0.5f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGB16_SFLOAT_PACK16,
			gli::packHalf(glm::vec3(0.0f, 0.0f, 0.0f)),
			gli::packHalf(glm::vec3(1.0f, 0.5f, 0.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGBA16_SFLOAT_PACK16,
			gli::packHalf(glm::vec4(0.0f, 0.0f, 0.0f, 0.0f)),
			gli::packHalf(glm::vec4(1.0f, 0.5f, 0.0f, 1.0f)),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_R32_SFLOAT_PACK32,
			glm::vec1(0.0f),
			glm::vec1(1.0f),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RG32_SFLOAT_PACK32,
			glm::vec2(0.0f, 0.0f),
			glm::vec2(1.0f, 0.5f),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGB32_SFLOAT_PACK32,
			glm::vec3(0.0f, 0.0f, 0.0f),
			glm::vec3(1.0f, 0.5f, 0.0f),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);

		Error += generate_mipmaps::test(gli::FORMAT_RGBA32_SFLOAT_PACK32,
			glm::vec4(0.0f, 0.0f, 0.0f, 0.0f),
			glm::vec4(1.0f, 0.5f, 0.0f, 1.0f),
			Layers[LayerIndex], Sizes[SizeIndex], Filters[FilterIndex]);
	}

	return Error;
}


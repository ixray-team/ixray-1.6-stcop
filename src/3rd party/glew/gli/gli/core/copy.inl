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
/// @file gli/core/copy.inl
/// @date 2013-01-23 / 2013-02-03
/// @author Christophe Riccio
///////////////////////////////////////////////////////////////////////////////////

namespace gli{
namespace detail
{
	inline void copy_images
	(
		texture const & Src, texture & Dst,
		texture::size_type BaseLayer, texture::size_type MaxLayer,
		texture::size_type BaseFace, texture::size_type MaxFace,
		texture::size_type BaseLevel, texture::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLayer >= 0 && BaseLayer <= MaxLayer && MaxLayer < Src.layers());
		GLI_ASSERT(BaseFace >= 0 && BaseFace <= MaxFace && MaxFace < Src.faces());
		GLI_ASSERT(BaseLevel >= 0 && BaseLevel <= MaxLevel && MaxLevel < Src.levels());

		texture::size_type LevelsSize = 0;
		for(texture::size_type LevelIndex = 0; LevelIndex < MaxLevel - BaseLevel + 1; ++LevelIndex)
		{
			GLI_ASSERT(Dst.size(LevelIndex) == Src.size(LevelIndex));
			LevelsSize += Dst.size(LevelIndex);
		}

		for(texture::size_type LayerIndex = 0, LayerCount = MaxLayer - BaseLayer + 1; LayerIndex < LayerCount; ++LayerIndex)
		for(texture::size_type FaceIndex = 0, FaceCount = MaxFace - BaseFace + 1; FaceIndex < FaceCount; ++FaceIndex)
		{
			memcpy(Dst.data(LayerIndex, FaceIndex, BaseLevel), Src.data(BaseLayer + LayerIndex, BaseFace + FaceIndex, BaseLevel), LevelsSize);
		}
	}
}//namespace detail

	inline image copy(image const & Image)
	{
		image Result(Image.format(), Image.dimensions());

		memcpy(Result.data(), Image.data(), Image.size());
		
		return Result;
	}

	template <>
	inline texture copy(texture const & Texture)
	{
		texture Copy(
			Texture.target(),
			Texture.format(),
			Texture.dimensions(),
			Texture.layers(),
			Texture.faces(),
			Texture.levels());

		detail::copy_images(
			Texture, Copy,
			0, Texture.layers() - 1,
			0, Texture.faces() - 1,
			0, Texture.levels() - 1);

		//memcpy(Copy.data(), Texture.data(), Copy.size());

		return Copy;
	}

	template <typename texType>
	inline texture copy(texType const & Texture)
	{
		texture Copy(
			Texture.target(),
			Texture.format(),
			Texture.texture::dimensions(),
			Texture.layers(),
			Texture.faces(),
			Texture.levels());

		detail::copy_images(
			Texture, Copy,
			0, Texture.layers() - 1,
			0, Texture.faces() - 1,
			0, Texture.levels() - 1);

		//memcpy(Copy.data(), Texture.data(), Copy.size());

		return Copy;
	}

	template <typename texType>
	inline texture copy(texType const & Texture, typename texType::format_type Format)
	{
		GLI_ASSERT(block_size(Texture.format()) == block_size(Format));

		texture Copy(
			Texture.target(),
			Format,
			Texture.dimensions(),
			Texture.layers(),
			Texture.faces(),
			Texture.levels());

		detail::copy_images(
			Texture, Copy,
			0, Texture.layers() - 1,
			0, Texture.faces() - 1,
			0, Texture.levels() - 1);

		return Copy;
	}

	inline texture copy
	(
		texture1D const & Texture,
		texture1D::size_type BaseLevel, texture1D::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel <= MaxLevel);
		GLI_ASSERT(BaseLevel < Texture.levels());
		GLI_ASSERT(MaxLevel < Texture.levels());
	
		texture1D Copy(
			Texture.format(),
			Texture.dimensions(BaseLevel),
			MaxLevel - BaseLevel + 1);

		memcpy(Copy.data(), Texture.data(0, 0, BaseLevel), Copy.size());

		return Copy;
	}

	inline texture copy
	(
		texture1DArray const & Texture,
		texture1DArray::size_type BaseLayer, texture1DArray::size_type MaxMayer,
		texture1DArray::size_type BaseLevel, texture1DArray::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel <= MaxLevel);
		GLI_ASSERT(BaseLevel < Texture.levels());
		GLI_ASSERT(MaxLevel < Texture.levels());
		GLI_ASSERT(BaseLayer <= MaxMayer);
		GLI_ASSERT(BaseLayer < Texture.layers());
		GLI_ASSERT(MaxMayer < Texture.layers());

		texture1DArray Copy(
			Texture.format(),
			Texture[BaseLayer].dimensions(BaseLevel),
			MaxMayer - BaseLayer + 1,
			MaxLevel - BaseLevel + 1);

		for(texture1DArray::size_type Layer = 0; Layer < Copy.layers(); ++Layer)
			memcpy(Copy.data(Layer, 0, 0), Texture.data(Layer + BaseLayer, 0, BaseLevel), Copy[Layer].size());

		return Copy;
	}

	inline texture copy
	(
		texture2D const & Texture,
		texture2D::size_type BaseLevel, texture2D::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel <= MaxLevel);
		GLI_ASSERT(BaseLevel < Texture.levels());
		GLI_ASSERT(MaxLevel < Texture.levels());
	
		texture2D Copy(
			Texture.format(),
			Texture.dimensions(BaseLevel),
			MaxLevel - BaseLevel + 1);

		memcpy(Copy.data(), Texture.data(0, 0, BaseLevel), Copy.size());

		return Copy;
	}

	inline texture copy
	(
		texture2DArray const & Texture,
		texture2DArray::size_type BaseLayer, texture2DArray::size_type MaxMayer,
		texture2DArray::size_type BaseLevel, texture2DArray::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel <= MaxLevel);
		GLI_ASSERT(BaseLevel < Texture.levels());
		GLI_ASSERT(MaxLevel < Texture.levels());
		GLI_ASSERT(BaseLayer <= MaxMayer);
		GLI_ASSERT(BaseLayer < Texture.layers());
		GLI_ASSERT(MaxMayer < Texture.layers());

		texture2DArray Copy(
			Texture.format(),
			Texture.dimensions(BaseLevel),
			MaxMayer - BaseLayer + 1,
			MaxLevel - BaseLevel + 1);

		for(texture2DArray::size_type Layer = 0; Layer < Copy.layers(); ++Layer)
			memcpy(Copy.data(Layer, 0, 0), Texture.data(Layer + BaseLayer, 0, BaseLevel), Copy[Layer].size());

		return Copy;
	}

	inline texture copy
	(
		texture3D const & Texture,
		texture3D::size_type BaseLevel, texture3D::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel <= MaxLevel);
		GLI_ASSERT(BaseLevel < Texture.levels());
		GLI_ASSERT(MaxLevel < Texture.levels());

		texture3D Copy(
			Texture.format(),
			Texture.dimensions(BaseLevel),
			MaxLevel - BaseLevel + 1);

		memcpy(Copy.data(), Texture.data(0, 0, BaseLevel), Copy.size());

		return Copy;
	}

	inline texture copy
	(
		textureCube const & Texture,
		textureCube::size_type BaseFace, textureCube::size_type MaxFace,
		textureCube::size_type BaseLevel, textureCube::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel >= 0 && BaseLevel < Texture.levels() && BaseLevel <= MaxLevel && MaxLevel < Texture.levels());
		GLI_ASSERT(BaseFace <= MaxFace);
		GLI_ASSERT(BaseFace < Texture.faces());
		GLI_ASSERT(MaxFace < Texture.faces());

		textureCube Copy(
			Texture.format(),
			Texture[BaseFace].dimensions(BaseLevel),
			MaxLevel - BaseLevel + 1);

		for(textureCube::size_type Face = 0; Face < Copy.faces(); ++Face)
			memcpy(Copy[Face].data(), Texture[Face + BaseFace][BaseLevel].data(), Copy[Face].size());

		return Copy;
	}

	inline texture copy
	(
		textureCubeArray const & Texture,
		textureCubeArray::size_type BaseLayer, textureCubeArray::size_type MaxLayer,
		textureCubeArray::size_type BaseFace, textureCubeArray::size_type MaxFace,
		textureCubeArray::size_type BaseLevel, textureCubeArray::size_type MaxLevel
	)
	{
		GLI_ASSERT(BaseLevel <= MaxLevel);
		GLI_ASSERT(BaseLevel < Texture.levels());
		GLI_ASSERT(MaxLevel < Texture.levels());
		GLI_ASSERT(BaseFace <= MaxFace);
		GLI_ASSERT(BaseFace < Texture.faces());
		GLI_ASSERT(MaxFace < Texture.faces());
		GLI_ASSERT(BaseLayer <= MaxLayer);
		GLI_ASSERT(BaseLayer < Texture.layers());
		GLI_ASSERT(MaxLayer < Texture.layers());

		textureCubeArray Copy(
			Texture.format(),
			Texture[BaseLayer][BaseFace].dimensions(BaseLevel),
			MaxLayer - BaseLayer + 1,
			MaxLevel - BaseLevel + 1);

		for(textureCubeArray::size_type Layer = 0; Layer < Copy.layers(); ++Layer)
		for(textureCubeArray::size_type Face = 0; Face < Copy[Layer].faces(); ++Face)
			memcpy(Copy[Layer][Face].data(), Texture[Layer + BaseLayer][Face + BaseFace][BaseLevel].data(), Copy[Layer][Face].size());

		return Copy;
	}
}//namespace gli

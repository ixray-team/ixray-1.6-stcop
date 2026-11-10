#include "RHI.h"
#include <DirectXTex.h>

#ifdef IXR_WINDOWS
#include <wincodec.h>
#else
// HACK: SAL's empty __null annotation breaks the standard NULL token inside stb.
#pragma push_macro("__null")
#undef __null
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stb/stb_image_write.h>
#pragma pop_macro("__null")
#endif

HRESULT
CRHI::GetDDSMetadata(const void* data, size_t size, RHITextureMetadata& out_metadata)
{
	DirectX::TexMetadata metadata = {};
	HRESULT result = DirectX::GetMetadataFromDDSMemory(static_cast<const uint8_t*>(data), size, DirectX::DDS_FLAGS_NONE, metadata);
	if (SUCCEEDED(result)) {
		out_metadata = { static_cast<u32>(metadata.mipLevels), static_cast<u32>(metadata.width), static_cast<u32>(metadata.height),
			static_cast<int>(metadata.format), metadata.IsCubemap(), metadata.IsVolumemap() };
	}
	return result;
}

bool
CRHI::DecodeAlpha(const void* Data, size_t Size, xr_vector<u8>& Alpha, u32& Width, u32& Height)
{
	DirectX::ScratchImage Source, Decoded;
	if (!Data || FAILED(DirectX::LoadFromDDSMemory(static_cast<const u8*>(Data), Size, DirectX::DDS_FLAGS_NONE, nullptr, Source)))
	{
		return false;
	}
	const DirectX::Image* Image = Source.GetImage(0, 0, 0);
	if (!Image || !Image->width || !Image->height || Image->width > 16384 || Image->height > 16384)
	{
		return false;
	}
	if (Image->format != DXGI_FORMAT_R8G8B8A8_UNORM)
	{
		const HRESULT Result = DirectX::IsCompressed(Image->format) ? DirectX::Decompress(*Image, DXGI_FORMAT_R8G8B8A8_UNORM, Decoded)
			: DirectX::Convert(*Image, DXGI_FORMAT_R8G8B8A8_UNORM, DirectX::TEX_FILTER_DEFAULT, DirectX::TEX_THRESHOLD_DEFAULT, Decoded);
		if (FAILED(Result))
		{
			return false;
		}
		Image = Decoded.GetImage(0, 0, 0);
	}
	Width = u32(Image->width);
	Height = u32(Image->height);
	Alpha.resize(size_t(Width) * Height);
	for (u32 Y = 0; Y < Height; Y++)
	{
		for (u32 X = 0; X < Width; X++)
		{
			Alpha[size_t(Y) * Width + X] = Image->pixels[Y * Image->rowPitch + X * 4 + 3];
		}
	}
	return true;
}

HRESULT
CRHI::LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bind_flags, ERHI_CPU_ACCESS_FLAG cpu_flags, int& lod, bool fallback, IRHISurface** out_surface)
{
    return DevicePtr->LoadDDS(data, size, usage, bind_flags, cpu_flags, lod, fallback, out_surface);
}

HRESULT CRHI::EncodeJpeg(const u8* Pixels, u32 Width, u32 Height, u32 Pitch, xr_vector<u8>& Out)
{
	Out.clear();
	DirectX::ScratchImage Image;
	HRESULT Result = Image.Initialize2D(DXGI_FORMAT_B8G8R8A8_UNORM, Width, Height, 1, 1);
	if (FAILED(Result))
	{
		return Result;
	}
	const DirectX::Image* Frame = Image.GetImage(0, 0, 0);
	for (u32 Row = 0; Row < Height; ++Row)
	{
		u8* Line = Frame->pixels + size_t(Row) * Frame->rowPitch;
		memcpy(Line, Pixels + size_t(Row) * Pitch, size_t(Width) * 4);
		for (u32 Pixel = 0; Pixel < Width; ++Pixel)
		{
			Line[Pixel * 4 + 3] = 0xff;
#ifndef IXR_WINDOWS
			std::swap(Line[Pixel * 4], Line[Pixel * 4 + 2]);
#endif
		}
	}
#ifdef IXR_WINDOWS
	DirectX::Blob Encoded;
	Result = DirectX::SaveToWICMemory(*Frame, DirectX::WIC_FLAGS_NONE, GUID_ContainerFormatJpeg, Encoded);
	if (SUCCEEDED(Result))
	{
		const u8* Begin = static_cast<const u8*>(Encoded.GetBufferPointer());
		Out.assign(Begin, Begin + Encoded.GetBufferSize());
	}
	return Result;
#else
	const auto Append = [](void* Context, void* Data, int Size)
	{
		const u8* Begin = static_cast<const u8*>(Data);
		static_cast<xr_vector<u8>*>(Context)->insert(static_cast<xr_vector<u8>*>(Context)->end(), Begin, Begin + Size);
	};
	return stbi_write_jpg_to_func(Append, &Out, static_cast<int>(Width), static_cast<int>(Height), 4, Frame->pixels, 90) ? S_OK : E_FAIL;
#endif
}

HRESULT
CRHI::EncodeRenderTarget(IRHIRenderTargetView* target, u32 width, u32 height, u32 format, bool linear, bool srgb, xr_vector<u8>& out_data)
{
	out_data.clear();
	auto surface = target->GetSurface();
	u32 source_width = surface->GetWidth();
	u32 source_height = surface->GetHeight();
	if (!source_width || !source_height || source_width > UINT32_MAX / 4 / source_height) {
		return E_INVALIDARG;
	}
	xr_vector<u8> pixels(size_t(source_width) * source_height * 4);
	u32 row_pitch = 0;
	if (!DevicePtr->ReadRenderTargetPixels(target, pixels.data(), static_cast<u32>(pixels.size()), source_width, source_height, row_pitch)) {
		u64 required = u64(row_pitch) * source_height;
		if (required <= pixels.size() || required > UINT32_MAX) {
			return E_FAIL;
		}
		pixels.resize(static_cast<size_t>(required));
		if (!DevicePtr->ReadRenderTargetPixels(target, pixels.data(), static_cast<u32>(pixels.size()), source_width, source_height, row_pitch)) {
			return E_FAIL;
		}
	}
	DirectX::ScratchImage image;
	HRESULT result = image.Initialize2D(static_cast<DXGI_FORMAT>(surface->GetFormat()), source_width, source_height, 1, 1);
	if (FAILED(result)) {
		return result;
	}
	const auto input = image.GetImage(0, 0, 0);
	if (input->rowPitch != size_t(source_width) * 4 || row_pitch < input->rowPitch ||
		size_t(row_pitch) * source_height > pixels.size()) {
		return E_INVALIDARG;
	}
	for (u32 row_idx = 0; row_idx < source_height; ++row_idx) {
		memcpy(image.GetPixels() + row_idx * input->rowPitch, pixels.data() + size_t(row_idx) * row_pitch, input->rowPitch);
	}
#ifndef IXR_WINDOWS
	for (u32 pixel_idx = 0; format != 3 && pixel_idx < source_width * source_height; ++pixel_idx) {
		std::swap(image.GetPixels()[pixel_idx * 4], image.GetPixels()[pixel_idx * 4 + 2]);
	}
#endif
	DirectX::ScratchImage resized;
	if (width && height) {
		result = DirectX::Resize(*input, width, height, linear ? DirectX::TEX_FILTER_LINEAR : DirectX::TEX_FILTER_DEFAULT, resized);
		if (FAILED(result)) {
			return result;
		}
	}
	const auto output = width && height ? resized.GetImage(0, 0, 0) : input;
	if (format == 3) {
		// The back buffer carries no meaningful alpha, while the load menu draws the save thumbnail with blending.
		for (size_t pixel_idx = 0; pixel_idx < output->width * output->height; ++pixel_idx) {
			output->pixels[pixel_idx * 4 + 3] = 0xff;
		}
	}
#ifdef IXR_WINDOWS
	DirectX::Blob encoded;
	switch (format) {
	case 0: result = DirectX::SaveToWICMemory(*output, srgb ? DirectX::WIC_FLAGS_FORCE_SRGB : DirectX::WIC_FLAGS_NONE, GUID_ContainerFormatJpeg, encoded); break;
	case 1: result = DirectX::SaveToTGAMemory(*output, srgb ? DirectX::TGA_FLAGS_FORCE_SRGB : DirectX::TGA_FLAGS_NONE, encoded); break;
	case 2: result = DirectX::SaveToWICMemory(*output, srgb ? DirectX::WIC_FLAGS_FORCE_SRGB : DirectX::WIC_FLAGS_NONE, GUID_ContainerFormatPng, encoded); break;
	case 3: result = DirectX::SaveToDDSMemory(*output, DirectX::DDS_FLAGS_NONE, encoded); break;
	default: return E_INVALIDARG;
	}
	if (SUCCEEDED(result)) {
		auto begin = static_cast<const u8*>(encoded.GetBufferPointer());
		out_data.assign(begin, begin + encoded.GetBufferSize());
	}
	return result;
#else
	if (format == 3) {
		// The load menu reads save thumbnails as DDS on every platform.
		DirectX::Blob encoded;
		result = DirectX::SaveToDDSMemory(*output, DirectX::DDS_FLAGS_NONE, encoded);
		if (SUCCEEDED(result)) {
			auto begin = static_cast<const u8*>(encoded.GetBufferPointer());
			out_data.assign(begin, begin + encoded.GetBufferSize());
		}
		return result;
	}
	auto callback = [](void* context, void* data, int size) {
		auto& bytes = *static_cast<xr_vector<u8>*>(context);
		auto begin = static_cast<u8*>(data);
		bytes.insert(bytes.end(), begin, begin + size);
	};
	int written = 0;
	if (format == 0) {
		written = stbi_write_jpg_to_func(callback, &out_data, output->width, output->height, 4, output->pixels, 90);
	} else if (format == 1) {
		written = stbi_write_tga_to_func(callback, &out_data, output->width, output->height, 4, output->pixels);
	} else {
		written = stbi_write_png_to_func(callback, &out_data, output->width, output->height, 4, output->pixels, output->rowPitch);
	}
	return written ? S_OK : E_FAIL;
#endif
}

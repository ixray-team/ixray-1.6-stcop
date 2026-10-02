#include "RHI.h"
#include <DirectXTex.h>

#ifdef IXR_WINDOWS
#include <wincodec.h>
#else
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stb/stb_image_write.h>
#endif

HRESULT
CRHI::GetDDSMetadata(const void* data, size_t size, RHITextureMetadata& out_metadata)
{
	DirectX::TexMetadata metadata = {};
	HRESULT result = DirectX::GetMetadataFromDDSMemory(data, size, DirectX::DDS_FLAGS_NONE, metadata);
	if (SUCCEEDED(result)) {
		out_metadata = { static_cast<u32>(metadata.mipLevels), static_cast<u32>(metadata.width), static_cast<u32>(metadata.height),
			static_cast<int>(metadata.format), metadata.IsCubemap(), metadata.IsVolumemap() };
	}
	return result;
}

HRESULT
CRHI::LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bind_flags, ERHI_CPU_ACCESS_FLAG cpu_flags, int& lod, bool fallback, IRHISurface** out_surface)
{
    return DevicePtr->LoadDDS(data, size, usage, bind_flags, cpu_flags, lod, fallback, out_surface);
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
	for (u32 pixel_idx = 0; pixel_idx < source_width * source_height; ++pixel_idx) {
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

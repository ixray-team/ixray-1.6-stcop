#include "Device.h"
#include <DirectXTex.h>

HRESULT InternalDevice11::LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bind_flags, ERHI_CPU_ACCESS_FLAG cpu_flags, int& lod, bool fallback, IRHISurface** out_surface)
{
	*out_surface = nullptr;
	DirectX::ScratchImage image;
	DirectX::TexMetadata metadata = {};
	HRESULT result = DirectX::LoadFromDDSMemory(data, size, fallback ? DirectX::DDS_FLAGS_NO_16BPP : DirectX::DDS_FLAGS_NONE, &metadata, image);
	if (FAILED(result)) {
		return result;
	}
	size_t first_mip = 0;
	if (!metadata.IsCubemap() && !metadata.IsVolumemap() && lod) {
		while (metadata.mipLevels > 1 && metadata.width > 4 && metadata.height > 4 && lod) {
			metadata.width /= 2;
			metadata.height /= 2;
			--metadata.mipLevels;
			--lod;
			++first_mip;
		}
		metadata.width = std::max(size_t(4), metadata.width);
		metadata.height = std::max(size_t(4), metadata.height);
	}
	ID3D11Resource* texture = nullptr;
	result = DirectX::CreateTextureEx(static_cast<ID3D11Device*>(RawDevice), image.GetImages() + first_mip,
		image.GetImageCount() - first_mip, metadata, static_cast<D3D11_USAGE>(usage), bind_flags, GetD3D11CPUAccess(cpu_flags),
		metadata.miscFlags, DirectX::CREATETEX_DEFAULT, &texture);
	if (SUCCEEDED(result)) {
		RHITextureDesc desc;
		*out_surface = GRHI->CreateTextureFromMemory(texture, 0, desc);
	}
	if (texture && FAILED(result)) {
		texture->Release();
	}
	return result;
}

void* InternalDevice11::GetContext()
{
    return HWRenderContext;
}

void* InternalDevice11::GetSwapchain()
{
    return HWSwapchain;
}

void InternalDevice11::BeginFrame()
{
}

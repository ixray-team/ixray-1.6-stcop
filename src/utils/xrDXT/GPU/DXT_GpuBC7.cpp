#include "StdAfx.h"

#ifdef IXR_WINDOWS
#include <d3d11.h>
#include <wrl/client.h>
#include <DirectXTex.h>

using Microsoft::WRL::ComPtr;

namespace
{
	class GpuBC7Encoder
	{
	public:
		static GpuBC7Encoder& Instance()
		{
			static GpuBC7Encoder Encoder;
			return Encoder;
		}

		bool Compress(const char* FileName);

	private:
		GpuBC7Encoder();

		ComPtr<ID3D11Device> Device;
		std::mutex Lock;
	};

	GpuBC7Encoder::GpuBC7Encoder()
	{
		const D3D_FEATURE_LEVEL FeatureLevel = D3D_FEATURE_LEVEL_11_0;
		const HRESULT Result = D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, 0, &FeatureLevel, 1, D3D11_SDK_VERSION, Device.GetAddressOf(), nullptr, nullptr);

		if (FAILED(Result))
		{
			Msg("! [xrDXT] GPU BC7 encoder is unavailable (0x%08X), using CPU", u32(Result));
			Device.Reset();
			return;
		}

		Msg("* [xrDXT] GPU BC7 encoder is ready");
	}

	bool GpuBC7Encoder::Compress(const char* FileName)
	{
		const std::wstring Path = std::filesystem::path(FileName).wstring();

		DirectX::TexMetadata Metadata;
		DirectX::ScratchImage Source;
		if (FAILED(DirectX::LoadFromDDSFile(Path.c_str(), DirectX::DDS_FLAGS_NONE, &Metadata, Source)))
		{
			return false;
		}

		DirectX::ScratchImage Converted;
		const DirectX::ScratchImage* Input = &Source;
		if (Metadata.format != DXGI_FORMAT_R8G8B8A8_UNORM)
		{
			if (FAILED(DirectX::Convert(Source.GetImages(), Source.GetImageCount(), Metadata, DXGI_FORMAT_R8G8B8A8_UNORM, DirectX::TEX_FILTER_DEFAULT, DirectX::TEX_THRESHOLD_DEFAULT, Converted)))
			{
				return false;
			}
			Input = &Converted;
		}

		DirectX::ScratchImage Compressed;
		{
			std::scoped_lock Guard(Lock);
			if (!Device)
			{
				return false;
			}

			const HRESULT Result = DirectX::Compress(Device.Get(), Input->GetImages(), Input->GetImageCount(), Input->GetMetadata(), DXGI_FORMAT_BC7_UNORM, DirectX::TEX_COMPRESS_DEFAULT, 1.f, Compressed);
			if (FAILED(Result))
			{
				Msg("! [xrDXT] GPU BC7 compression failed for %s (0x%08X), using CPU", FileName, u32(Result));
				if (FAILED(Device->GetDeviceRemovedReason()))
				{
					Msg("! [xrDXT] GPU device lost, BC7 encoder switched to CPU");
					Device.Reset();
				}
				return false;
			}
		}

		return SUCCEEDED(DirectX::SaveToDDSFile(Compressed.GetImages(), Compressed.GetImageCount(), Compressed.GetMetadata(), DirectX::DDS_FLAGS_NONE, Path.c_str()));
	}
}

bool DXTCompressBC7GPU(const char* FileName)
{
	return GpuBC7Encoder::Instance().Compress(FileName);
}
#endif

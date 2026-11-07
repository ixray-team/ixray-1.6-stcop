#include <DirectXTex.h>
#define STB_IMAGE_WRITE_IMPLEMENTATION
#undef NULL
#define NULL 0
#include "../../Layers/xrRender/ScreenshotEncoding.h"
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iterator>
#include <vector>

int main(int ArgumentCount, char** Arguments)
{
	DirectX::ScratchImage Source;
	if (FAILED(Source.Initialize2D(DXGI_FORMAT_B8G8R8A8_UNORM, 2, 1, 1, 1)))
	{
		return 1;
	}
	const uint8_t Pixels[] = {12, 34, 56, 255, 90, 78, 123, 255};
	memcpy(Source.GetPixels(), Pixels, sizeof(Pixels));
	DirectX::Blob Encoded;
	DirectX::ScratchImage Decoded;
	DirectX::TexMetadata Metadata = {};
	if (FAILED(DirectX::SaveToDDSMemory(*Source.GetImage(0, 0, 0), DirectX::DDS_FLAGS_NONE, Encoded)) || Encoded.GetBufferSize() < 4 || memcmp(Encoded.GetBufferPointer(), "DDS ", 4) != 0 || FAILED(DirectX::LoadFromDDSMemory(static_cast<const uint8_t*>(Encoded.GetBufferPointer()), Encoded.GetBufferSize(), DirectX::DDS_FLAGS_NONE, &Metadata, Decoded)) || Metadata.width != 2 || Metadata.height != 1 || Metadata.format != DXGI_FORMAT_B8G8R8A8_UNORM || memcmp(Decoded.GetPixels(), Pixels, sizeof(Pixels)) != 0)
	{
		puts("FAIL: native BGRA DDS encode/decode");
		return 2;
	}
	std::vector<uint8_t> TGA;
	const auto WriteTGA = [](void* Context, void* Data, int Size)
	{
		auto& Bytes = *static_cast<std::vector<uint8_t>*>(Context);
		const auto* First = static_cast<const uint8_t*>(Data);
		Bytes.insert(Bytes.end(), First, First + Size);
	};
	const uint8_t ExpectedRGBA[] = {56, 34, 12, 255, 123, 78, 90, 255};
	if (!ScreenshotEncoding::WriteBGRATGA(WriteTGA, &TGA, *Source.GetImage(0, 0, 0)) || FAILED(DirectX::LoadFromTGAMemory(TGA.data(), TGA.size(), DirectX::TGA_FLAGS_NONE, &Metadata, Decoded)) || Metadata.format != DXGI_FORMAT_R8G8B8A8_UNORM || Metadata.width != 2 || Metadata.height != 1 || memcmp(Decoded.GetPixels(), ExpectedRGBA, sizeof(ExpectedRGBA)) != 0 || memcmp(Source.GetPixels(), Pixels, sizeof(Pixels)) != 0)
	{
		puts("FAIL: synchronous BGRA TGA output colors or source preservation");
		return 4;
	}
	puts("PASS: synchronous BGRA TGA output asymmetric colors and unchanged DDS source");
	if (ArgumentCount > 1)
	{
		std::ifstream File(Arguments[1], std::ios::binary);
		const std::vector<uint8_t> Bytes((std::istreambuf_iterator<char>(File)), std::istreambuf_iterator<char>());
		if (!File || Bytes.empty() || FAILED(DirectX::LoadFromDDSMemory(Bytes.data(), Bytes.size(), DirectX::DDS_FLAGS_NONE, &Metadata, Decoded)))
		{
			fprintf(stderr, "FAIL: generated save thumbnail is not readable DDS: %s\n", Arguments[1]);
			return 3;
		}
		printf("PASS: generated DDS thumbnail %zux%zu\n", Metadata.width, Metadata.height);
	}
	puts("PASS: native DDS encoding, metadata, BGRA pixels and decoding");
	return 0;
}


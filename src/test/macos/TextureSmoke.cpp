#include <DirectXTex.h>
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


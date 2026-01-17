#include "DX9ShaderDeclaration.h"
#include <d3d9.h>

inline BYTE ConvertFormatToDeclType(ERHI_FORMAT format)
{
	switch (format)
	{
	case ERHI_FORMAT::R32G32B32A32_FLOAT: return D3DDECLTYPE_FLOAT4;
	case ERHI_FORMAT::R32G32B32_FLOAT:    return D3DDECLTYPE_FLOAT3;
	case ERHI_FORMAT::R32G32_FLOAT:       return D3DDECLTYPE_FLOAT2;
	case ERHI_FORMAT::R32_FLOAT:          return D3DDECLTYPE_FLOAT1;

	case ERHI_FORMAT::R8G8B8A8_UNORM:
	case ERHI_FORMAT::B8G8R8A8_UNORM:    return D3DDECLTYPE_D3DCOLOR;

	case ERHI_FORMAT::R16G16_FLOAT:       return D3DDECLTYPE_SHORT2;      // нужно проверить, может SHORT2N
	case ERHI_FORMAT::R16G16B16A16_FLOAT: return D3DDECLTYPE_SHORT4;      // аналогично

	case ERHI_FORMAT::UNKNOWN:
	case ERHI_FORMAT::R32G32B32A32_UINT:
	case ERHI_FORMAT::R32G32B32_UINT:
	case ERHI_FORMAT::R32G32_UINT:
	case ERHI_FORMAT::R32_UINT:
	case ERHI_FORMAT::R8G8B8A8_UINT:
		return D3DDECLTYPE_UNUSED;

	default:
		return D3DDECLTYPE_UNUSED;
	}
}

inline u32 GetFormatSize(ERHI_FORMAT fmt)
{
	switch (fmt)
	{
	case ERHI_FORMAT::R32G32B32A32_FLOAT: return 16;
	case ERHI_FORMAT::R32G32B32_FLOAT:    return 12;
	case ERHI_FORMAT::R32G32_FLOAT:       return 8;
	case ERHI_FORMAT::R32_FLOAT:          return 4;
	case ERHI_FORMAT::R8G8B8A8_UNORM:     return 4;
	case ERHI_FORMAT::R16G16_FLOAT:       return 4;
	case ERHI_FORMAT::R16_FLOAT:          return 2;
	case ERHI_FORMAT::R8_UNORM:           return 1;
	default: VERIFY(!"check"); return 0;
	}
}

inline BYTE ConvertSemanticToUsage(const char* semantic)
{
	if (_stricmp(semantic, "POSITION") == 0) return D3DDECLUSAGE_POSITION;
	if (_stricmp(semantic, "NORMAL") == 0) return D3DDECLUSAGE_NORMAL;
	if (_stricmp(semantic, "TEXCOORD") == 0) return D3DDECLUSAGE_TEXCOORD;
	if (_stricmp(semantic, "COLOR") == 0) return D3DDECLUSAGE_COLOR;
	if (_stricmp(semantic, "TANGENT") == 0) return D3DDECLUSAGE_TANGENT;
	if (_stricmp(semantic, "BINORMAL") == 0) return D3DDECLUSAGE_BINORMAL;

	return D3DDECLUSAGE_POSITION;
}

DX9ShaderDeclaration::~DX9ShaderDeclaration()
{
	if (VertDecl)
	{
		VertDecl->Release();
		VertDecl = nullptr;
	}
}

void DX9ShaderDeclaration::GenerateLayerDescriptors(void*)
{
	if (!DX9Descriptors.empty())
	{
		return;
	}

	IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)GRHI->DevicePtr->RawDevice;
	DX9Descriptors.resize(Descriptors.size() + 1);

	u32 offset = 0; // начальное смещение
	for (size_t i = 0; i < Descriptors.size(); ++i)
	{
		const RHIInputElementDesc& Desc = Descriptors[i];

		DX9Descriptors[i].Stream = static_cast<WORD>(Desc.InputSlot);

		DX9Descriptors[i].Offset = Desc.AlignedByteOffset != static_cast<u32>(-1)
			? static_cast<WORD>(Desc.AlignedByteOffset)
			: static_cast<WORD>(offset);

		DX9Descriptors[i].Type = ConvertFormatToDeclType(Desc.Format);
		DX9Descriptors[i].Method = D3DDECLMETHOD_DEFAULT;
		DX9Descriptors[i].Usage = ConvertSemanticToUsage(Desc.SemanticName);
		DX9Descriptors[i].UsageIndex = static_cast<BYTE>(Desc.SemanticIndex);

		offset += GetFormatSize(Desc.Format);
	}

	DX9Descriptors.back() = D3DDECL_END();

	HRESULT hr = DxDevice->CreateVertexDeclaration(DX9Descriptors.data(), &VertDecl);
	if (FAILED(hr))
	{
		VertDecl = nullptr;
	}
}

void DX9ShaderDeclaration::ApplyLayout()
{
	IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)GRHI->DevicePtr->RawDevice;
	CHK_DX(DxDevice->SetVertexDeclaration(VertDecl));
}
#pragma once
#include "DX11ShaderDeclaration.h"

inline UINT DX11GetFormatSize(DXGI_FORMAT fmt)
{
	switch (fmt)
	{
		// 128-бит
	case DXGI_FORMAT_R32G32B32A32_FLOAT:   return 16;
	case DXGI_FORMAT_R32G32B32A32_UINT:    return 16;
	case DXGI_FORMAT_R32G32B32A32_SINT:    return 16;

		// 96-бит
	case DXGI_FORMAT_R32G32B32_FLOAT:      return 12;
	case DXGI_FORMAT_R32G32B32_UINT:       return 12;
	case DXGI_FORMAT_R32G32B32_SINT:       return 12;

		// 64-бит
	case DXGI_FORMAT_R16G16B16A16_FLOAT:   return 8;
	case DXGI_FORMAT_R16G16B16A16_UNORM:   return 8;
	case DXGI_FORMAT_R16G16B16A16_UINT:    return 8;
	case DXGI_FORMAT_R16G16B16A16_SNORM:   return 8;
	case DXGI_FORMAT_R16G16B16A16_SINT:    return 8;

	case DXGI_FORMAT_R32G32_FLOAT:         return 8;
	case DXGI_FORMAT_R32G32_UINT:          return 8;
	case DXGI_FORMAT_R32G32_SINT:          return 8;

	case DXGI_FORMAT_R32G8X24_TYPELESS:    return 8; // спец. случай для depth/stencil
	case DXGI_FORMAT_D32_FLOAT_S8X24_UINT: return 8;

		// 32-бит
	case DXGI_FORMAT_R32_FLOAT:            return 4;
	case DXGI_FORMAT_R32_UINT:             return 4;
	case DXGI_FORMAT_R32_SINT:             return 4;

	case DXGI_FORMAT_R16G16_FLOAT:         return 4;
	case DXGI_FORMAT_R16G16_UNORM:         return 4;
	case DXGI_FORMAT_R16G16_UINT:          return 4;
	case DXGI_FORMAT_R16G16_SNORM:         return 4;
	case DXGI_FORMAT_R16G16_SINT:          return 4;

	case DXGI_FORMAT_R10G10B10A2_UNORM:    return 4;
	case DXGI_FORMAT_R10G10B10A2_UINT:     return 4;
	case DXGI_FORMAT_R11G11B10_FLOAT:      return 4;

	case DXGI_FORMAT_B8G8R8A8_UNORM:       return 4;
	case DXGI_FORMAT_R8G8B8A8_UNORM:       return 4;
	case DXGI_FORMAT_R8G8B8A8_UINT:        return 4;
	case DXGI_FORMAT_R8G8B8A8_SNORM:       return 4;
	case DXGI_FORMAT_R8G8B8A8_SINT:        return 4;

	case DXGI_FORMAT_R32_TYPELESS:         return 4;
	case DXGI_FORMAT_D32_FLOAT:            return 4;
	case DXGI_FORMAT_R24G8_TYPELESS:       return 4;
	case DXGI_FORMAT_D24_UNORM_S8_UINT:    return 4;

		// 16-бит
	case DXGI_FORMAT_R16_FLOAT:            return 2;
	case DXGI_FORMAT_R16_UNORM:            return 2;
	case DXGI_FORMAT_R16_UINT:             return 2;
	case DXGI_FORMAT_R16_SNORM:            return 2;
	case DXGI_FORMAT_R16_SINT:             return 2;

	case DXGI_FORMAT_R8G8_UNORM:           return 2;
	case DXGI_FORMAT_R8G8_UINT:            return 2;
	case DXGI_FORMAT_R8G8_SNORM:           return 2;
	case DXGI_FORMAT_R8G8_SINT:            return 2;

		// 8-бит
	case DXGI_FORMAT_R8_UNORM:             return 1;
	case DXGI_FORMAT_R8_UINT:              return 1;
	case DXGI_FORMAT_R8_SNORM:             return 1;
	case DXGI_FORMAT_R8_SINT:              return 1;

	case DXGI_FORMAT_A8_UNORM:             return 1;

		// специальные и блочные форматы (размер зависит от блока)
	case DXGI_FORMAT_BC1_TYPELESS:
	case DXGI_FORMAT_BC1_UNORM:
	case DXGI_FORMAT_BC1_UNORM_SRGB:
	case DXGI_FORMAT_BC2_TYPELESS:
	case DXGI_FORMAT_BC2_UNORM:
	case DXGI_FORMAT_BC2_UNORM_SRGB:
	case DXGI_FORMAT_BC3_TYPELESS:
	case DXGI_FORMAT_BC3_UNORM:
	case DXGI_FORMAT_BC3_UNORM_SRGB:
	case DXGI_FORMAT_BC4_TYPELESS:
	case DXGI_FORMAT_BC4_UNORM:
	case DXGI_FORMAT_BC4_SNORM:
	case DXGI_FORMAT_BC5_TYPELESS:
	case DXGI_FORMAT_BC5_UNORM:
	case DXGI_FORMAT_BC5_SNORM:
	case DXGI_FORMAT_BC6H_TYPELESS:
	case DXGI_FORMAT_BC6H_UF16:
	case DXGI_FORMAT_BC6H_SF16:
	case DXGI_FORMAT_BC7_TYPELESS:
	case DXGI_FORMAT_BC7_UNORM:
	case DXGI_FORMAT_BC7_UNORM_SRGB:
		return 0;

	default:
		return 0;
	}
}



void DX11ShaderDeclaration::GenerateLayerDescriptors(void* Signature)
{
	if (!DX11Descriptors.empty())
	{
		return;
	}

	DX11Descriptors.reserve(Descriptors.size());

	UINT offset = 0;
	for (size_t i = 0; i < Descriptors.size(); ++i)
	{
		const RHIInputElementDesc& Desc = Descriptors[i];
		if (Desc.SemanticName == nullptr)
			continue;

		D3D11_INPUT_ELEMENT_DESC& Dx11Desc = DX11Descriptors.emplace_back();
		Dx11Desc.SemanticName = Desc.SemanticName;
		Dx11Desc.SemanticIndex = Desc.SemanticIndex;
		Dx11Desc.Format = static_cast<DXGI_FORMAT>(Desc.Format);
		Dx11Desc.InputSlot = Desc.InputSlot;

		Dx11Desc.AlignedByteOffset = (Desc.AlignedByteOffset != static_cast<u32>(-1)) ? Desc.AlignedByteOffset : offset;

		Dx11Desc.InputSlotClass = static_cast<D3D11_INPUT_CLASSIFICATION>(Desc.InputSlotClass);
		Dx11Desc.InstanceDataStepRate = Desc.InstanceDataStepRate;

		offset += DX11GetFormatSize(Dx11Desc.Format);
	}

	ID3D11Device* DxDevice = (ID3D11Device*)GRHI->DevicePtr->RawDevice;
	ID3DBlob* InputSignature = (ID3DBlob*)Signature;
	VERIFY(InputSignature);

	HRESULT hr = DxDevice->CreateInputLayout
	(
		DX11Descriptors.data(),
		(UINT)DX11Descriptors.size(),
		InputSignature->GetBufferPointer(),
		InputSignature->GetBufferSize(),
		&InputLayout
	);

	if (FAILED(hr))
	{
		InputLayout = nullptr;
	}
}

void DX11ShaderDeclaration::ApplyLayout()
{
	ID3D11DeviceContext* DxContext = (ID3D11DeviceContext*)GRHI->GetContext();
	DxContext->IASetInputLayout(InputLayout);
}

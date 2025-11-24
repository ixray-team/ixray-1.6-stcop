#include "RHI.h"
#include <magic_enum/magic_enum.hpp>

IRHIShaderDeclaration::IRHIShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount)
{
	Descriptors.resize(DescCount);

	size_t Iter = 0;
	for (RHIInputElementDesc& Desc : Descriptors)
	{
		Desc.AlignedByteOffset = DescList[Iter].AlignedByteOffset;
		Desc.SemanticName = DescList[Iter].SemanticName;
		Desc.InputSlot = DescList[Iter].InputSlot;
		Desc.SemanticIndex = DescList[Iter].SemanticIndex;
		Desc.Format = DescList[Iter].Format;
		Desc.InputSlotClass = DescList[Iter].InputSlotClass;
		Desc.InstanceDataStepRate = DescList[Iter].InstanceDataStepRate;
		Iter++;
	}

	//memcpy(Descriptors.data(), DescList, DescCount * sizeof(RHIInputElementDesc));
	VertexSize = GRHI->GetInputElementDescStride(DescList, DescCount);
}

RHI_API size_t RHIUtils::Shader::ComputeVertexStride(const xr_vector<RHIInputElementDesc>& il)
{
	size_t stride = 0;

	for (const auto& elem : il)
	{
		switch (elem.Format)
		{
		case ERHI_FORMAT::R32_FLOAT:               stride += 4;  break;
		case ERHI_FORMAT::R32G32_FLOAT:            stride += 8;  break;
		case ERHI_FORMAT::R32G32B32_FLOAT:         stride += 12; break;
		case ERHI_FORMAT::R32G32B32A32_FLOAT:      stride += 16; break;

		case ERHI_FORMAT::R8G8B8A8_UNORM:
		case ERHI_FORMAT::B8G8R8A8_UNORM:
			stride += 4; break;

		default:
			VERIFY2(!"! [ComputeVertexStride] Unhandled format", magic_enum::enum_name(elem.Format).data());
			break;
		}
	}

	return stride;
}

RHI_API bool RHIUtils::Shader::CreateInputLayoutFromFVF(uint32_t fvfCode, xr_vector<RHIInputElementDesc>& il)
{
	static constexpr ERHI_FORMAT s_blendFormats[] =
	{
		ERHI_FORMAT::R32_FLOAT,
		ERHI_FORMAT::R32G32_FLOAT,
		ERHI_FORMAT::R32G32B32_FLOAT,
		ERHI_FORMAT::R32G32B32A32_FLOAT,
	};

	static constexpr ERHI_FORMAT s_texCoordFormats[] =
	{
		ERHI_FORMAT::R32G32_FLOAT,
		ERHI_FORMAT::R32G32B32_FLOAT,
		ERHI_FORMAT::R32G32B32A32_FLOAT,
		ERHI_FORMAT::R32_FLOAT
	};

	il.clear();

	if ((fvfCode & ((RHI_FVF_RESERVED0 | RHI_FVF_RESERVED2) & ~RHI_FVF_POSITION_MASK)) != 0)
		return false;

	const uint32_t nTexCoords = (fvfCode & RHI_FVF_TEXCOUNT_MASK) >> RHI_FVF_TEXCOUNT_SHIFT;
	if (nTexCoords > 8)
		return false;

	uint32_t offset = 0;

	switch (fvfCode & RHI_FVF_POSITION_MASK)
	{
	case 0:
		break;

	case RHI_FVF_XYZRHW:
	case RHI_FVF_XYZW:
		il.emplace_back(RHIInputElementDesc{ "SV_Position", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
		offset += 16;
		break;

	default:
		il.emplace_back(RHIInputElementDesc{ "SV_Position", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
		offset += 12;
		break;
	}

	size_t weights = 0;
	switch (fvfCode & RHI_FVF_POSITION_MASK)
	{
	case RHI_FVF_XYZB1: weights = 1; break;
	case RHI_FVF_XYZB2: weights = 2; break;
	case RHI_FVF_XYZB3: weights = 3; break;
	case RHI_FVF_XYZB4: weights = 4; break;
	case RHI_FVF_XYZB5: weights = 5; break;
	}

	if (weights > 0)
	{
		if (fvfCode & (RHI_FVF_LASTBETA_UBYTE4 | RHI_FVF_LASTBETA_D3DCOLOR))
		{
			if (weights > 1)
			{
				il.emplace_back(RHIInputElementDesc{ "BLENDWEIGHT", 0, s_blendFormats[weights - 2], 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
				offset += (weights - 1) * 4;
			}

			il.emplace_back(RHIInputElementDesc{ "BLENDINDICES", 0,
				(fvfCode & RHI_FVF_LASTBETA_UBYTE4) ? ERHI_FORMAT::R8G8B8A8_UNORM : ERHI_FORMAT::B8G8R8A8_UNORM,
				0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
			offset += 4;
		}
		else if (weights == 5)
		{
			il.clear();
			return false;
		}
		else
		{
			il.emplace_back(RHIInputElementDesc{ "BLENDWEIGHT", 0, s_blendFormats[weights - 1], 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
			offset += weights * 4;
		}
	}

	if (fvfCode & RHI_FVF_NORMAL)
	{
		il.emplace_back(RHIInputElementDesc{ "NORMAL", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
		offset += 12;
	}

	if (fvfCode & RHI_FVF_PSIZE)
	{
		il.emplace_back(RHIInputElementDesc{ "PSIZE", 0, ERHI_FORMAT::R32_FLOAT, 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
		offset += 4;
	}

	if (fvfCode & RHI_FVF_DIFFUSE)
	{
		il.emplace_back(RHIInputElementDesc{ "COLOR", 0, ERHI_FORMAT::B8G8R8A8_UNORM, 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
		offset += 4;
	}

	if (fvfCode & RHI_FVF_SPECULAR)
	{
		il.emplace_back(RHIInputElementDesc{ "COLOR", 1, ERHI_FORMAT::B8G8R8A8_UNORM, 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });
		offset += 4;
	}

	if (nTexCoords > 0)
	{
		for (uint32_t t = 0; t < nTexCoords; ++t)
		{
			const size_t index = (fvfCode >> (16 + t * 2)) & 0x3;
			il.emplace_back(RHIInputElementDesc{ "TEXCOORD", static_cast<u32>(t), s_texCoordFormats[index], 0, offset, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 });

			switch (index)
			{
			case 0: offset += 8;  break;  // R32G32
			case 1: offset += 12; break;  // R32G32B32
			case 2: offset += 16; break;  // R32G32B32A32
			case 3: offset += 4;  break;  // R32
			default: break;
			}
		}
	}

	return true;
}
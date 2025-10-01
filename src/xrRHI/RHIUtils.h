#pragma once

namespace RHIUtils
{
	inline bool CreateVertexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		RHIBufferDesc desc = {};
		desc.Usage = bImmutable ? ERHI_USAGE::USAGE_DEFAULT : ERHI_USAGE::USAGE_DYNAMIC;
		desc.Size = DataSize;
		desc.Type = ERHI_BUFFER_TYPE::VERTEX;
		desc.CPUAccessFlags = bImmutable ? 0 : ERHI_CPU_ACCESS_FLAG_WRITE;

		RHIBufferSubresource resource = {};
		resource.pSysMem = pData;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, pData ? &resource : nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateIndexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true, bool Read = false)
	{
		RHIBufferDesc desc = {};
		desc.Usage = (bImmutable || Read) ? ERHI_USAGE::USAGE_DEFAULT : ERHI_USAGE::USAGE_DYNAMIC;
		desc.Size = DataSize;
		desc.Type = ERHI_BUFFER_TYPE::INDEX;
		desc.CPUAccessFlags = bImmutable ? 0 : ERHI_CPU_ACCESS_FLAG_WRITE;

		RHIBufferSubresource resource = {};
		resource.pSysMem = pData;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, pData ? &resource : nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	// Will return nullptr on DX9
	inline bool CreateConstantBuffer(IRHIBuffer** ppBuffer, u32 DataSize)
	{
		RHIBufferDesc desc = {};
		desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		desc.Size = DataSize;
		desc.Type = ERHI_BUFFER_TYPE::CONSTANT;
		desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG_WRITE;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateInputLayoutFromFVF(uint32_t fvfCode, xr_vector<RHIInputElementDesc>& il)
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

		switch (fvfCode & RHI_FVF_POSITION_MASK)
		{
		case 0:
			break;

		case RHI_FVF_XYZRHW:
			il.emplace_back(
				RHIInputElementDesc{ "POSITIONT", 0, ERHI_FORMAT::R32G32B32A32_FLOAT,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
			break;
		case RHI_FVF_XYZW:
			il.emplace_back(
				RHIInputElementDesc{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
			break;

		default:
			il.emplace_back(
				RHIInputElementDesc{ "POSITION", 0, ERHI_FORMAT::R32G32B32_FLOAT,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
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
				// subtract one for where the blendindices were
				if (weights > 1)
				{
					il.emplace_back(
						RHIInputElementDesc{ "BLENDWEIGHT", 0, s_blendFormats[weights - 2],
						0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
					);
				}

				il.emplace_back(
					RHIInputElementDesc{ "BLENDINDICES", 0,
					(fvfCode & RHI_FVF_LASTBETA_UBYTE4) ? ERHI_FORMAT::R8G8B8A8_UNORM : ERHI_FORMAT::B8G8R8A8_UNORM,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
				);
			}
			else if (weights == 5)
			{
				// RHI_FVF_XYZB5 is only supported when the 5th beta is RHI_FVF_LASTBETA_UBYTE4/D3DCOLOR
				il.clear();
				return false;
			}
			else
			{
				il.emplace_back(
					RHIInputElementDesc{ "BLENDWEIGHT", 0, s_blendFormats[weights - 1],
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
				);
			}
		}

		if (fvfCode & RHI_FVF_NORMAL)
		{
			il.emplace_back(
				RHIInputElementDesc{ "NORMAL", 0, ERHI_FORMAT::R32G32B32_FLOAT,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
		}

		if (fvfCode & RHI_FVF_PSIZE)
		{
			il.emplace_back(
				RHIInputElementDesc{ "PSIZE", 0, ERHI_FORMAT::R32_FLOAT,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
		}

		if (fvfCode & RHI_FVF_DIFFUSE)
		{
			il.emplace_back(
				RHIInputElementDesc{ "COLOR", 0, ERHI_FORMAT::B8G8R8A8_UNORM,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
		}

		if (fvfCode & RHI_FVF_SPECULAR)
		{
			il.emplace_back(
				RHIInputElementDesc{ "COLOR", 1, ERHI_FORMAT::B8G8R8A8_UNORM,
					0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
			);
		}

		if (nTexCoords > 0)
		{
			for (uint32_t t = 0; t < nTexCoords; ++t)
			{
				const size_t index = (fvfCode >> (16 + t * 2)) & 0x3;
				il.emplace_back(
					RHIInputElementDesc{ "TEXCOORD", static_cast<UINT>(t),
						s_texCoordFormats[index],
						0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
				);
			}
		}

		return true;
	}

	template <std::size_t N>
	inline constexpr u32 ComputeVertexSize(const RHIInputElementDesc(&decl)[N])
	{
		u32 size = 0;
		for (std::size_t i = 0; i < N; ++i)
		{
			switch (decl[i].Format)
			{
			case ERHI_FORMAT::R32G32B32A32_FLOAT: size += 16; break;
			case ERHI_FORMAT::R32G32B32_FLOAT:    size += 12; break;
			case ERHI_FORMAT::R32G32_FLOAT:       size += 8;  break;
			case ERHI_FORMAT::R32_FLOAT:          size += 4;  break;
			case ERHI_FORMAT::R8G8B8A8_UNORM:     size += 4;  break;
			case ERHI_FORMAT::R16G16_FLOAT:       size += 4;  break;
			case ERHI_FORMAT::R16_FLOAT:          size += 2;  break;
			case ERHI_FORMAT::R8_UNORM:           size += 1;  break;
			default:                               size += 0; break;
			}
		}
		return size;
	}

	inline u32 ComputeVertexSize(const xr_vector<RHIInputElementDesc>& decl)
	{
		u32 stride = 0;
		for (auto& e : decl)
		{
			u32 size = 0;
			switch (e.Format)
			{
			case ERHI_FORMAT::R32G32B32A32_FLOAT: size += 16; break;
			case ERHI_FORMAT::R32G32B32_FLOAT:    size += 12; break;
			case ERHI_FORMAT::R32G32_FLOAT:       size += 8;  break;
			case ERHI_FORMAT::R32_FLOAT:          size += 4;  break;
			case ERHI_FORMAT::R8G8B8A8_UNORM:     size += 4;  break;
			case ERHI_FORMAT::R16G16_FLOAT:       size += 4;  break;
			case ERHI_FORMAT::R16_FLOAT:          size += 2;  break;
			case ERHI_FORMAT::R8_UNORM:           size += 1;  break;
			default:                              size += 0; break;
			}

			u32 end = e.AlignedByteOffset + size;
			if (end > stride)
				stride = end;
		}
		return stride;
	}
}
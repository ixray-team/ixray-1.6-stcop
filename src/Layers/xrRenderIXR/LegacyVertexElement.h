#pragma once

namespace XRay::Legacy
{
	struct LEGACYVERTEXELEMENT9
	{
		u16 Stream;     // Stream index
		u16 Offset;     // Offset in the stream in bytes
		u8 Type;       // Data type
		u8 Method;     // Processing method
		u8 Usage;      // Semantics
		u8 UsageIndex; // Semantic index
	};

	constexpr u32 LEGACYMAXDECLLENGTH = 64;

	inline size_t GetDeclLength(const LEGACYVERTEXELEMENT9* pDecl)
	{
		if (!pDecl)
			return 0;

		size_t length = 0;
		while (pDecl->Stream != 0xFF)
		{
			if (length >= LEGACYMAXDECLLENGTH)
				return 0;

			++pDecl;
			++length;
		}
		return length;
	}

	static const char* GetSemanticName(u8 usage)
	{
		switch (usage)
		{
		case 0:  return "POSITION";
		case 9:  return "POSITIONT";
		case 3:  return "NORMAL";
		case 5:  return "TEXCOORD";
		case 6:  return "TANGENT";
		case 7:  return "BINORMAL";
		case 10: return "COLOR";
		case 1:  return "BLENDWEIGHT";
		case 2:  return "BLENDINDICES";
		default: return "UNKNOWN";
		}
	}

	static ERHI_FORMAT ConvertDeclTypeToFormat(u8 type)
	{
		switch (type)
		{
		case D3DDECLTYPE_FLOAT1: return ERHI_FORMAT::R32_FLOAT;
		case D3DDECLTYPE_FLOAT2: return	ERHI_FORMAT::R32G32_FLOAT;
		case D3DDECLTYPE_FLOAT3: return	ERHI_FORMAT::R32G32B32_FLOAT;
		case D3DDECLTYPE_FLOAT4: return ERHI_FORMAT::R32G32B32A32_FLOAT;
		case D3DDECLTYPE_D3DCOLOR: return ERHI_FORMAT::R8G8B8A8_UNORM;	// Warning. Explicit RGB component swizzling is nesessary	//	Not available 
		case D3DDECLTYPE_UBYTE4: return	ERHI_FORMAT::R8G8B8A8_UINT;			// Note: Shader gets UINT values, but if Direct3D 9 style integral floats are needed (0.0f, 1.0f... 255.f), UINT can just be converted to float32 in shader. 
		case D3DDECLTYPE_SHORT2: return	ERHI_FORMAT::R16G16_SINT;			// Note: Shader gets SINT values, but if Direct3D 9 style integral floats are needed, SINT can just be converted to float32 in shader. 
		case D3DDECLTYPE_SHORT4: return	ERHI_FORMAT::R16G16B16A16_SINT;	// Note: Shader gets SINT values, but if Direct3D 9 style integral floats are needed, SINT can just be converted to float32 in shader. 
		case D3DDECLTYPE_UBYTE4N: return ERHI_FORMAT::R8G8B8A8_UNORM;
		case D3DDECLTYPE_SHORT2N: return ERHI_FORMAT::R16G16_SNORM;
		case D3DDECLTYPE_SHORT4N: return ERHI_FORMAT::R16G16B16A16_SNORM;
		case D3DDECLTYPE_USHORT2N: return ERHI_FORMAT::R16G16_UNORM;
		case D3DDECLTYPE_USHORT4N: return ERHI_FORMAT::R16G16B16A16_UNORM;
		case D3DDECLTYPE_FLOAT16_2: return ERHI_FORMAT::R16G16_FLOAT;
		case D3DDECLTYPE_FLOAT16_4: return ERHI_FORMAT::R16G16B16A16_FLOAT;
		default: return ERHI_FORMAT::UNKNOWN;
		}
	}

	constexpr u8 g_declTypeSizes[] =
	{
		4,  // D3DDECLTYPE_FLOAT1
		8,  // D3DDECLTYPE_FLOAT2
		12, // D3DDECLTYPE_FLOAT3
		16, // D3DDECLTYPE_FLOAT4
		4,  // D3DDECLTYPE_D3DCOLOR
		4,  // D3DDECLTYPE_UBYTE4
		4,  // D3DDECLTYPE_SHORT2
		8,  // D3DDECLTYPE_SHORT4
		4,  // D3DDECLTYPE_UBYTE4N
		4,  // D3DDECLTYPE_SHORT2N
		8,  // D3DDECLTYPE_SHORT4N
		4,  // D3DDECLTYPE_USHORT2N
		8,  // D3DDECLTYPE_USHORT4N
		4,  // D3DDECLTYPE_UDEC3
		4,  // D3DDECLTYPE_DEC3N
		4,  // D3DDECLTYPE_FLOAT16_2
		8,  // D3DDECLTYPE_FLOAT16_4
	};

	inline size_t ComputeVertexSize(const LEGACYVERTEXELEMENT9* pDecl, u32 stream)
	{
		if (!pDecl || stream >= 16u /*D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT*/)
			return 0;

		size_t currentSize = 0;
		size_t count = 0;

		//search for the max offset in the stream,
		//(min)vertex size = max offset + type size
		while (pDecl->Stream != 0xFF)
		{
			++count;
			if (count > LEGACYMAXDECLLENGTH)
				return 0;

			// only look at items of this stream and vertex elements actually in the data stream (not generated)
			// UV is phantom data.
			if ((pDecl->Stream == stream) && (pDecl->Method != 4))
			{
				if (pDecl->Type >= std::size(g_declTypeSizes))
					return 0;

				const size_t slotSize = g_declTypeSizes[pDecl->Type];
				if (currentSize < slotSize + pDecl->Offset)
					currentSize = slotSize + pDecl->Offset;
			}

			++pDecl;
		}

		return currentSize;
	}
}
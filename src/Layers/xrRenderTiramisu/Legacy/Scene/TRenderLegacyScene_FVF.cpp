#include "TLegacyScene.h"
#include "src/xrCore/stream_reader.h"


#pragma pack(push,4)
struct D3DVERTEXELEMEN_D3D9
{
	u16    Stream;     // Stream index
    u16    Offset;     // Offset in the stream in bytes
	u8    Type;       // Data type
    u8    Method;     // Processing method
    u8    Usage;      // Semantics
    u8    UsageIndex; // Semantic index
};
#pragma pack(pop)
enum D3D9DECLTYPE
{
    D3D9DECLTYPE_FLOAT1 = 0,  // 1D float expanded to (value, 0., 0., 1.)
    D3D9DECLTYPE_FLOAT2 = 1,  // 2D float expanded to (value, value, 0., 1.)
    D3D9DECLTYPE_FLOAT3 = 2,  // 3D float expanded to (value, value, value, 1.)
    D3D9DECLTYPE_FLOAT4 = 3,  // 4D float
    D3D9DECLTYPE_D3DCOLOR = 4,  // 4D packed unsigned bytes mapped to 0. to 1. range
                                 // Input is in D3DCOLOR format (ARGB) expanded to (R, G, B, A)
        D3D9DECLTYPE_UBYTE4 = 5,  // 4D unsigned byte
        D3D9DECLTYPE_SHORT2 = 6,  // 2D signed short expanded to (value, value, 0., 1.)
        D3D9DECLTYPE_SHORT4 = 7,  // 4D signed short

    // The following types are valid only with vertex shaders >= 2.0


    D3D9DECLTYPE_UBYTE4N = 8,  // Each of 4 bytes is normalized by dividing to 255.0
    D3D9DECLTYPE_SHORT2N = 9,  // 2D signed short normalized (v[0]/32767.0,v[1]/32767.0,0,1)
    D3D9DECLTYPE_SHORT4N = 10,  // 4D signed short normalized (v[0]/32767.0,v[1]/32767.0,v[2]/32767.0,v[3]/32767.0)
    D3D9DECLTYPE_USHORT2N = 11,  // 2D unsigned short normalized (v[0]/65535.0,v[1]/65535.0,0,1)
    D3D9DECLTYPE_USHORT4N = 12,  // 4D unsigned short normalized (v[0]/65535.0,v[1]/65535.0,v[2]/65535.0,v[3]/65535.0)
    D3D9DECLTYPE_UDEC3 = 13,  // 3D unsigned 10 10 10 format expanded to (value, value, value, 1)
    D3D9DECLTYPE_DEC3N = 14,  // 3D signed 10 10 10 format normalized and expanded to (v[0]/511.0, v[1]/511.0, v[2]/511.0, 1)
    D3D9DECLTYPE_FLOAT16_2 = 15,  // Two 16-bit floating point values, expanded to (value, value, 0, 1)
    D3D9DECLTYPE_FLOAT16_4 = 16,  // Four 16-bit floating point values
    D3D9DECLTYPE_UNUSED = 17,  // When the type field in a decl is unused.
};

enum D3D9DECLUSAGE
{
    D3D9DECLUSAGE_POSITION = 0,
    D3D9DECLUSAGE_BLENDWEIGHT,   // 1
    D3D9DECLUSAGE_BLENDINDICES,  // 2
    D3D9DECLUSAGE_NORMAL,        // 3
    D3D9DECLUSAGE_PSIZE,         // 4
    D3D9DECLUSAGE_TEXCOORD,      // 5
    D3D9DECLUSAGE_TANGENT,       // 6
    D3D9DECLUSAGE_BINORMAL,      // 7
    D3D9DECLUSAGE_TESSFACTOR,    // 8
    D3D9DECLUSAGE_POSITIONT,     // 9
    D3D9DECLUSAGE_COLOR,         // 10
    D3D9DECLUSAGE_FOG,           // 11
    D3D9DECLUSAGE_DEPTH,         // 12
    D3D9DECLUSAGE_SAMPLE,        // 13
} ;

enum D3D9DECLMETHOD
{
    D3D9DECLMETHOD_DEFAULT = 0,
    D3D9DECLMETHOD_PARTIALU,
    D3D9DECLMETHOD_PARTIALV,
    D3D9DECLMETHOD_CROSSUV,    // Normal
    D3D9DECLMETHOD_UV,
    D3D9DECLMETHOD_LOOKUP,               // Lookup a displacement map
    D3D9DECLMETHOD_LOOKUPPRESAMPLED,     // Lookup a pre-sampled displacement map
} ;

#define D3D9DECLEND() {0xFF,0,D3D9DECLTYPE_UNUSED,0,0,0}


static D3DVERTEXELEMEN_D3D9	LDeclR1LMap[] =	// 12+4+4+4+4+4	= 32
{
	{0, 0,  D3D9DECLTYPE_FLOAT3,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_POSITION,	0 },
	{0, 12, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_NORMAL,	0 },
	{0, 16, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TANGENT,	0 },
	{0, 20, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_BINORMAL,	0 },
	{0, 24, D3D9DECLTYPE_SHORT2,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TEXCOORD,	0 },
	{0, 28, D3D9DECLTYPE_SHORT2,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TEXCOORD,	1 },
    D3D9DECLEND()
};
static D3DVERTEXELEMEN_D3D9		LDeclR1Vert[] =	// 12+4+4+4+4+4 = 32
{
	{0, 0,  D3D9DECLTYPE_FLOAT3,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_POSITION,	0 },
	{0, 12, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_NORMAL,	0 },
	{0, 16, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TANGENT,	0 },
	{0, 20, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_BINORMAL,	0 },
	{0, 24, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_COLOR,		0 },
	{0, 28, D3D9DECLTYPE_SHORT2,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TEXCOORD,	0 },
    D3D9DECLEND()
};
static D3DVERTEXELEMEN_D3D9		LDeclXVert[] =	// 12
{
	{0, 0,  D3D9DECLTYPE_FLOAT3,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_POSITION,	0 },
    D3D9DECLEND()
};
static D3DVERTEXELEMEN_D3D9	LDeclMUModel[] = // 12+4+4+4+8=32
{
	{0, 0,  D3D9DECLTYPE_FLOAT3,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_POSITION,	0 },
	{0, 12, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_NORMAL,	0 },
	{0, 16, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TANGENT,	0 },
	{0, 20, D3D9DECLTYPE_D3DCOLOR,	D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_BINORMAL,	0 },
	{0, 24, D3D9DECLTYPE_SHORT4,		D3D9DECLMETHOD_DEFAULT, 	D3D9DECLUSAGE_TEXCOORD,	0 },
    D3D9DECLEND()
};

ELegacyLevelVertexType TLegacyScene::GetAndConvertFVF(CStreamReader* base_fs, uint32_t& OutSize)
{
	D3DVERTEXELEMEN_D3D9 TempArray[65];
	base_fs->r(TempArray, sizeof(TempArray));
	
	if (memcmp(TempArray,LDeclR1LMap,sizeof(LDeclR1LMap)) == 0)
	{
		constexpr int32_t Offset = sizeof(LDeclR1LMap) - sizeof(TempArray);
		base_fs->advance(Offset);
		OutSize = sizeof(FLegacyLevelVertex_BaseWithLightMap);
		return ELegacyLevelVertexType::BaseWithLightMap;
	}
	if (memcmp(TempArray,LDeclR1Vert,sizeof(LDeclR1Vert)) == 0)
	{
		constexpr int32_t Offset = sizeof(LDeclR1Vert) - sizeof(TempArray);
		base_fs->advance(Offset);
		OutSize = sizeof(FLegacyLevelVertex_BaseWithLightColor);
		return ELegacyLevelVertexType::BaseWithLightColor;
	}
	if (memcmp(TempArray,LDeclMUModel,sizeof(LDeclMUModel)) == 0)
	{
		constexpr int32_t Offset = sizeof(LDeclMUModel) - sizeof(TempArray);
		base_fs->advance(Offset);
		OutSize = sizeof(FLegacyLevelVertex_MultipleUsageModel);
		return ELegacyLevelVertexType::MultipleUsageModel;
	}
	if (memcmp(TempArray,LDeclXVert,sizeof(LDeclXVert)) == 0)
	{
		constexpr int32_t Offset = sizeof(LDeclXVert) - sizeof(TempArray);
		base_fs->advance(Offset);
		OutSize = sizeof(FLegacyLevelVertex_OnlyVertex);
		return ELegacyLevelVertexType::VertexOnly;
	}
	NODEFAULT;
    return ELegacyLevelVertexType::VertexOnly;
}

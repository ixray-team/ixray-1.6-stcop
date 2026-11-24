#include "stdafx.h"
#include "R_DStreams.h"
#include "PackedVertex.h"

template <typename Source, PackedVertex (*Convert)(const Source&)>
static void EncodeVertices(PackedVertex* Target, const u8* Data, u32 Count)
{
	const Source* Items = reinterpret_cast<const Source*>(Data);
	for (u32 Index = 0; Index < Count; ++Index)
	{
		Target[Index] = Convert(Items[Index]);
	}
}

struct VertexFormatInfo
{
	u32 Stride;
	void (*Encode)(PackedVertex*, const u8*, u32);
};

static constexpr VertexFormatInfo VertexFormats[] =
{
	{ sizeof(PackedVertex), nullptr },
	{ sizeof(FVF::L), EncodeVertices<FVF::L, VertexPacking::FromL> },
	{ sizeof(FVF::LIT), EncodeVertices<FVF::LIT, VertexPacking::FromLIT> },
	{ sizeof(FVF::TL), EncodeVertices<FVF::TL, VertexPacking::FromTL> },
	{ sizeof(FVF::TL0uv), EncodeVertices<FVF::TL0uv, VertexPacking::FromTL0uv> },
	{ sizeof(FVF::V), EncodeVertices<FVF::V, VertexPacking::FromV> },
};

u32 VertexStride(EVertexFormat Format)
{
	return VertexFormats[u32(Format)].Stride;
}

void PackVertices(EVertexFormat Format, PackedVertex* Target, const void* Source, u32 Count)
{
	VertexFormats[u32(Format)].Encode(Target, static_cast<const u8*>(Source), Count);
}

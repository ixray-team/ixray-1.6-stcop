#ifndef r_DStreamsH
#define r_DStreamsH
#pragma once
#include "RenderAPI.h"

enum
{
	LOCKFLAGS_FLUSH		= D3DLOCK_DISCARD,
	LOCKFLAGS_APPEND	= D3DLOCK_NOOVERWRITE
};

struct PackedVertex;

enum class EVertexFormat : u8
{
	Packed,
	L,
	LIT,
	TL,
	TL0uv,
	V
};

// Vertices of a legacy format as the 32-byte records the vertex shaders pull
u32 VertexStride(EVertexFormat Format);
void PackVertices(EVertexFormat Format, PackedVertex* Target, const void* Source, u32 Count);
#endif

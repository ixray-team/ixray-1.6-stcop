#ifndef VERTEX_HLSLI
#define VERTEX_HLSLI

#include "shared\fixed_cb.hlsli"

struct PackedVertex
{
	float3 P;
	uint N;
	uint TF;
	uint UV;
	uint C;
	uint X;
};

struct PulledVertex
{
	float3 P;
	float3 N;
	float3 T;
	float3 B;
	float2 UV;
	uint C;
	uint X;
};

StructuredBuffer<PackedVertex> g_RingVertices : register(t57);
StructuredBuffer<PackedVertex> g_SkinnedVertices : register(t58);
StructuredBuffer<PackedVertex> g_Vertices4 : register(t59);
StructuredBuffer<PackedVertex> g_Vertices3 : register(t60);
StructuredBuffer<PackedVertex> g_Vertices2 : register(t61);
StructuredBuffer<PackedVertex> g_Vertices1 : register(t62);
StructuredBuffer<PackedVertex> g_Vertices0 : register(t63);

#define PULL_INPUT uint DrawId : DRAWID, uint VertexId : SV_VertexID

float2 DecodeS16x2(uint W)
{
	return float2(asint(uint2(W << 16, W)) >> 16);
}

float4 DecodeU8x4(uint W)
{
	return float4((W.xxxx >> uint4(16, 8, 0, 24)) & 0xFFu) * (1.0f / 255.0f);
}

float3 DecodeHalf3(uint C, uint X)
{
	return f16tof32(uint3(C, C >> 16, X));
}

float3 DecodeOctRaw(uint W)
{
	float2 E = max(DecodeS16x2(W) * (1.0f / 32767.0f), -1.0f);
	float3 N = float3(E, 1.0f - abs(E.x) - abs(E.y));
	float T = saturate(-N.z);
	N.xy += N.xy >= 0.0f ? -T : T;
	return N;
}

float3 DecodeTangent(float3 Raw, float3 N, uint TF)
{
	float3 T1 = normalize(abs(Raw.y) > abs(Raw.z) ? float3(Raw.y, -Raw.x, 0.0f) : float3(Raw.z, 0.0f, -Raw.x));
	float3 T2 = cross(T1, N);
	float P = float(TF & 0x7FFFu) * (1.0f / 32767.0f);
	float S = P <= 0.5f ? 1.0f - 4.0f * P : 4.0f * P - 3.0f;
	float2 D = normalize(float2(S, (P <= 0.5f ? 1.0f : -1.0f) * (1.0f - abs(S))));
	return D.x * T1 + D.y * T2;
}

float2 DecodeUV(uint TF, uint UV)
{
	int2 Q = (asint(uint2(UV << 16, UV)) >> 16 << 8) | int2((TF.xx >> uint2(16, 24)) & 0xFFu);
	return float2(Q) * (1.0f / 262144.0f);
}

PulledVertex Decode(PackedVertex V)
{
	PulledVertex O;
	float3 Raw = DecodeOctRaw(V.N);
	O.P = V.P;
	O.N = normalize(Raw);
	O.T = DecodeTangent(Raw, O.N, V.TF);
	O.B = ((V.TF & 0x8000u) ? -1.0f : 1.0f) * cross(O.N, O.T);
	O.UV = DecodeUV(V.TF, V.UV);
	O.C = V.C;
	O.X = V.X;
	return O;
}

uint VertexIndex(uint VertexId)
{
	uint Local = (draw_flags & DRAW_QUADS) ? VertexId / 6u * 4u + ((0x123210u >> (VertexId % 6u * 4u)) & 0xFu) : VertexId;
	return draw_vertex_base + Local;
}

PackedVertex LoadPage(uint Page, uint Index)
{
	[branch]
	if (Page == 1u)
	{
		return g_Vertices1[Index];
	}
	[branch]
	if (Page == 2u)
	{
		return g_Vertices2[Index];
	}
	[branch]
	if (Page == 3u)
	{
		return g_Vertices3[Index];
	}
	[branch]
	if (Page == 4u)
	{
		return g_Vertices4[Index];
	}
	return g_Vertices0[Index];
}

PackedVertex LoadPacked(uint Index)
{
	uint Source = draw_flags & DRAW_SOURCE_MASK;
	[branch]
	if (Source == DRAW_SOURCE_RING)
	{
		return g_RingVertices[Index];
	}
	[branch]
	if (Source == DRAW_SOURCE_SKINNED)
	{
		return g_SkinnedVertices[Index];
	}
	return LoadPage((draw_flags >> DRAW_PAGE_SHIFT) & DRAW_PAGE_MASK, Index);
}

PackedVertex LoadSource(uint VertexId)
{
	[branch]
	if ((draw_flags & DRAW_SOURCE_MASK) == DRAW_SOURCE_SKINNED)
	{
		return LoadPage((draw_flags >> DRAW_PAGE_SHIFT) & DRAW_PAGE_MASK, draw_source_base + VertexId);
	}
	return LoadPacked(VertexIndex(VertexId));
}

PulledVertex PullVertex(uint VertexId)
{
	return Decode(LoadPacked(VertexIndex(VertexId)));
}

struct RingVertex
{
	float3 P;
	float2 UV;
	float4 C;
	uint X;
};

RingVertex PullRing(uint VertexId)
{
	PackedVertex V = LoadPacked(VertexIndex(VertexId));
	RingVertex O;
	O.P = V.P;
	O.UV = DecodeUV(V.TF, V.UV);
	O.C = DecodeU8x4(V.C);
	O.X = V.X;
	return O;
}

#endif

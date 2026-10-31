#pragma once
#include <bit>
#include "RenderAPI.h"
#include "FVF.h"

struct PackedVertex
{
	Fvector P;
	u32 N;
	u32 TF;
	u32 UV;
	u32 C;
	u32 X;
};
static_assert(sizeof(PackedVertex) == 32);

namespace VertexPacking
{
	RENDER_API u32 EncodeOct(const Fvector& N);
	// Unnormalized, bit-exact with the shader's DecodeOctRaw: the tangent basis branches on it.
	RENDER_API Fvector DecodeOct(u32 Value);
	RENDER_API void EncodeFrame(PackedVertex& Vertex, Fvector N, const Fvector& T, const Fvector& B);
	RENDER_API bool UVRecenter(const Fvector2* UV, u32 Count, Fvector2& Offset);
	RENDER_API Fvector2 FitUV(const Fvector2* UV, u32 Count, const char* Name);
	RENDER_API void GenerateTangents(const Fvector* Positions, const Fvector2* UV, u32 VertexCount, const u16* Indices, u32 IndexCount, Fvector* Tangents, Fvector* Binormals);

	inline void EncodeUV(PackedVertex& Vertex, float U, float V)
	{
		VERIFY(U >= -32.0f && U < 32.0f && V >= -32.0f && V < 32.0f);
		const u32 QU = u32(std::lrint(std::max(-8388608.0f, std::min(U * 262144.0f, 8388607.0f))));
		const u32 QV = u32(std::lrint(std::max(-8388608.0f, std::min(V * 262144.0f, 8388607.0f))));
		Vertex.TF = (Vertex.TF & 0xFFFFu) | ((QU & 0xFFu) << 16) | ((QV & 0xFFu) << 24);
		Vertex.UV = ((QU >> 8) & 0xFFFFu) | (QV >> 8 << 16);
	}

	inline void EncodeFitUV(PackedVertex& Vertex, const Fvector2& UV, const Fvector2& Offset)
	{
		constexpr float Limit = 32.0f - 1.0f / 262144.0f;
		EncodeUV(Vertex, std::clamp(UV.x + Offset.x, -32.0f, Limit), std::clamp(UV.y + Offset.y, -32.0f, Limit));
	}

	inline u32 EncodeColor(float X, float Y, float Z, float W)
	{
		const auto Unorm8 = [](float Value) { return u32(std::max(0.0f, std::min(Value, 1.0f)) * 255.0f + 0.5f); };
		return color_rgba(Unorm8(X), Unorm8(Y), Unorm8(Z), Unorm8(W));
	}

	inline PackedVertex FromL(const FVF::L& Source)
	{
		return { Source.p, 0, 0, 0, Source.color, 0 };
	}

	inline PackedVertex FromLIT(const FVF::LIT& Source)
	{
		PackedVertex Vertex{ Source.p, 0, 0, 0, Source.color, 0 };
		EncodeUV(Vertex, Source.t.x, Source.t.y);
		return Vertex;
	}

	inline PackedVertex FromTL(const FVF::TL& Source)
	{
		PackedVertex Vertex{ { Source.p.x, Source.p.y, Source.p.z }, 0, 0, 0, Source.color, std::bit_cast<u32>(Source.p.w) };
		EncodeUV(Vertex, Source.uv.x, Source.uv.y);
		return Vertex;
	}

	inline PackedVertex FromTL0uv(const FVF::TL0uv& Source)
	{
		return { { Source.p.x, Source.p.y, Source.p.z }, 0, 0, 0, Source.color, std::bit_cast<u32>(Source.p.w) };
	}

	inline PackedVertex FromV(const FVF::V& Source)
	{
		PackedVertex Vertex{ Source.p, 0, 0, 0, 0xFFFFFFFFu, 0 };
		EncodeUV(Vertex, Source.t.x, Source.t.y);
		return Vertex;
	}
}

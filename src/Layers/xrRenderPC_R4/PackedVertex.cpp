#include "stdafx.h"
#include "PackedVertex.h"

namespace
{
	u32 Snorm16(float Value)
	{
		return u32(u16(s16(std::lrint(std::clamp(Value, -1.0f, 1.0f) * 32767.0f))));
	}

	bool AxisOffset(float Min, float Max, float& Offset)
	{
		const float Low = std::floor(Max - 32.0f) + 1.0f;
		const float High = std::floor(Min + 32.0f);
		const bool Fits = Low <= High;
		Offset = Fits ? -std::clamp(0.0f, Low, High) : 0.0f;
		return Fits;
	}
}

namespace VertexPacking
{
	u32 EncodeOct(const Fvector& N)
	{
		const float L1 = std::abs(N.x) + std::abs(N.y) + std::abs(N.z);
		if (!std::isfinite(L1) || L1 == 0.0f)
		{
			return 0;
		}
		float X = N.x / L1;
		float Y = N.y / L1;
		if (N.z < 0.0f)
		{
			const float FoldX = std::copysign(1.0f - std::abs(Y), X);
			Y = std::copysign(1.0f - std::abs(X), Y);
			X = FoldX;
		}
		return Snorm16(X) | (Snorm16(Y) << 16);
	}

	Fvector DecodeOct(u32 Value)
	{
		const float X = std::max(float(s16(Value)) * (1.0f / 32767.0f), -1.0f);
		const float Y = std::max(float(s16(Value >> 16)) * (1.0f / 32767.0f), -1.0f);
		const float Z = 1.0f - std::abs(X) - std::abs(Y);
		const float T = std::clamp(-Z, 0.0f, 1.0f);
		return { X >= 0.0f ? X - T : X + T, Y >= 0.0f ? Y - T : Y + T, Z };
	}

	void EncodeFrame(PackedVertex& Vertex, Fvector N, const Fvector& T, const Fvector& B)
	{
		const Fvector Source = N;
		Vertex.N = EncodeOct(N);
		Fvector Raw = DecodeOct(Vertex.N);
		// Guard band: keeps |y| and |z| apart so mad/fma differences cannot flip the shader's basis branch.
		while (std::abs(std::abs(Raw.y) - std::abs(Raw.z)) < 1.0f / 4096.0f)
		{
			N.y += N.y >= 0.0f ? 1.0f / 8192.0f : -1.0f / 8192.0f;
			N.normalize();
			Vertex.N = EncodeOct(N);
			Raw = DecodeOct(Vertex.N);
		}
		Fvector Normal = Raw;
		Normal.normalize();
		Fvector T1 = std::abs(Raw.y) > std::abs(Raw.z) ? Fvector{ Raw.y, -Raw.x, 0.0f } : Fvector{ Raw.z, 0.0f, -Raw.x };
		T1.normalize();
		Fvector T2;
		T2.crossproduct(T1, Normal);
		const float A = T.dotproduct(T1);
		const float D = T.dotproduct(T2);
		const float L1 = std::abs(A) + std::abs(D);
		const float S = std::isfinite(L1) && L1 > 0.0f ? A / L1 : 1.0f;
		const float P = D >= 0.0f ? 0.25f * (1.0f - S) : 0.5f + 0.25f * (1.0f + S);
		Fvector Cross;
		Cross.crossproduct(Source, T);
		Vertex.TF = (Vertex.TF & 0xFFFF0000u) | u32(std::lrint(P * 32767.0f)) | (Cross.dotproduct(B) < 0.0f ? 0x8000u : 0u);
	}

	bool UVRecenter(const Fvector2* UV, u32 Count, Fvector2& Offset)
	{
		Fvector2 Min{ flt_max, flt_max };
		Fvector2 Max{ -flt_max, -flt_max };
		for (u32 Index = 0; Index < Count; ++Index)
		{
			Min.min(UV[Index]);
			Max.max(UV[Index]);
		}
		const bool FitsU = AxisOffset(Min.x, Max.x, Offset.x);
		return AxisOffset(Min.y, Max.y, Offset.y) && FitsU;
	}
}

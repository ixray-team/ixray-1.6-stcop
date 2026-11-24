/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "ClusterPages.h"

namespace ClusterPages
{
namespace
{
constexpr u32 MostVertices = 192;

Fvector OctRaw(s32 X, s32 Y)
{
	const float U = float(X) * (1.0f / 255.0f);
	const float V = float(Y) * (1.0f / 255.0f);
	const float Z = 1.0f - std::abs(U) - std::abs(V);
	const float T = std::clamp(-Z, 0.0f, 1.0f);
	return { U >= 0.0f ? U - T : U + T, V >= 0.0f ? V - T : V + T, Z };
}

Fvector TangentAxis(const Fvector& Raw)
{
	Fvector Axis = std::abs(Raw.y) > std::abs(Raw.z) ? Fvector{ Raw.y, -Raw.x, 0.0f } : Fvector{ Raw.z, 0.0f, -Raw.x };
	Axis.normalize();
	return Axis;
}

u32 EncodeFrame(const PackedVertex& Vertex)
{
	const Fvector Raw = VertexPacking::DecodeOct(Vertex.N);
	Fvector Normal = Raw;
	Normal.normalize();
	const Fvector Axis = TangentAxis(Raw);
	Fvector Across;
	Across.crossproduct(Axis, Normal);
	const float Turn = float(Vertex.TF & 0x7FFFu) * (1.0f / 32767.0f);
	const float Along = Turn <= 0.5f ? 1.0f - 4.0f * Turn : 4.0f * Turn - 3.0f;
	const float Aside = (Turn <= 0.5f ? 1.0f : -1.0f) * (1.0f - std::abs(Along));
	const Fvector Tangent = Fvector().mad(Fvector().mul(Axis, Along), Across, Aside);

	const float L1 = std::abs(Normal.x) + std::abs(Normal.y) + std::abs(Normal.z);
	float X = L1 > 0.0f ? Normal.x / L1 : 0.0f;
	float Y = L1 > 0.0f ? Normal.y / L1 : 0.0f;

	if (Normal.z < 0.0f)
	{
		const float Fold = std::copysign(1.0f - std::abs(Y), X);
		Y = std::copysign(1.0f - std::abs(X), Y);
		X = Fold;
	}

	const s32 OctX = std::clamp(s32(std::lrint(X * 255.0f)), -255, 255);
	s32 OctY = std::clamp(s32(std::lrint(Y * 255.0f)), -255, 255);
	Fvector Stored = OctRaw(OctX, OctY);

	// The shader picks the tangent's axis by which of y and z is larger: a tie would be picked either way
	for (u32 Try = 0; Try < 4 && std::abs(std::abs(Stored.y) - std::abs(Stored.z)) < 1.0f / 1024.0f; ++Try)
	{
		OctY += OctY < 255 ? 1 : -2;
		Stored = OctRaw(OctX, OctY);
	}

	Fvector Kept = Stored;
	Kept.normalize();
	const Fvector KeptAxis = TangentAxis(Stored);
	Fvector KeptAcross;
	KeptAcross.crossproduct(KeptAxis, Kept);
	const float A = Tangent.dotproduct(KeptAxis);
	const float D = Tangent.dotproduct(KeptAcross);
	const float Sum = std::abs(A) + std::abs(D);
	const float S = Sum > 0.0f ? A / Sum : 1.0f;
	const float Packed = D >= 0.0f ? 0.25f * (1.0f - S) : 0.5f + 0.25f * (1.0f + S);
	const u32 Hemi = ((Vertex.C >> 16) & 0xFFu) >> 2;
	return u32(OctX + 255) | u32(OctY + 255) << 9 | u32(std::lrint(Packed * 127.0f)) << 18 | ((Vertex.TF >> 15) & 1u) << 25 | Hemi << 26;
}

u32 EncodeBaked(const PackedVertex& Vertex, Baked Kind)
{
	switch (Kind)
	{
	case Baked::Lightmap:
		return u32((s32(s16(Vertex.X)) + 32768) >> 3) | u32((s32(s16(Vertex.X >> 16)) + 32768) >> 3) << 13;
	case Baked::Colour:
		return ((Vertex.C >> 8) & 0xFFu) | (Vertex.X & 0xFFFFFFu) << 8;
	case Baked::Tree:
		return (Vertex.X & 0xFFFFu) | ((Vertex.C >> 8) & 0xFFu) << 16;
	default:
		return 0;
	}
}

void PutBits(xr_vector<u32>& Words, u64& At, u32 Value, u32 Count)
{
	if (!Count)
	{
		return;
	}

	const u32 Shift = u32(At & 31);
	const size_t Word = size_t(At >> 5);
	Words.resize(std::max(Words.size(), Word + 2), 0);
	Words[Word] |= Value << Shift;
	Words[Word + 1] |= Shift ? Value >> (32 - Shift) : 0;
	At += Count;
}
} // namespace

void Append(Store& Out, const PackedVertex* Vertices, const u16* Corners, u32 TriangleCount, Baked Kind, bool KeepsUv)
{
	R_ASSERT(TriangleCount * 3 <= MostVertices);
	u16 Local[MostVertices];
	s32 Grid[MostVertices][3];
	s32 Uvs[MostVertices][2];
	s32 UvLow[2] = { INT_MAX, INT_MAX };
	s32 UvHigh[2] = { INT_MIN, INT_MIN };
	u32 Count = 0;
	Cluster Record = {};
	Record.Header = u32(Out.Attributes.size());
	Record.FirstCorner = u32(Out.Corners.size() / 4);
	const u32 FirstBaked = Kind == Baked::None ? u32(-1) : u32(Out.BakedWords.size());
	Out.Attributes.resize(Out.Attributes.size() + HeaderWords, 0);
	s32 Low[3] = { INT_MAX, INT_MAX, INT_MAX };
	s32 High[3] = { INT_MIN, INT_MIN, INT_MIN };

	for (u32 Corner = 0; Corner < TriangleCount * 3; ++Corner)
	{
		const u16 Source = Corners[Corner];
		const u32 Slot = u32(std::find(Local, Local + Count, Source) - Local);

		if (Slot == Count)
		{
			const PackedVertex& Vertex = Vertices[Source];
			Local[Count] = Source;

			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Grid[Count][Axis] = s32(std::lrint(double((&Vertex.P.x)[Axis]) * GridScale));
				Low[Axis] = std::min(Low[Axis], Grid[Count][Axis]);
				High[Axis] = std::max(High[Axis], Grid[Count][Axis]);
			}

			Uvs[Count][0] = s32(s16(Vertex.UV)) * 256 + s32((Vertex.TF >> 16) & 0xFFu);
			Uvs[Count][1] = s32(s16(Vertex.UV >> 16)) * 256 + s32(Vertex.TF >> 24);

			for (u32 Axis = 0; Axis < 2; ++Axis)
			{
				UvLow[Axis] = std::min(UvLow[Axis], Uvs[Count][Axis]);
				UvHigh[Axis] = std::max(UvHigh[Axis], Uvs[Count][Axis]);
			}

			Out.Attributes.push_back(EncodeFrame(Vertex));
			Out.Attributes.push_back(0);

			if (Kind != Baked::None)
			{
				Out.BakedWords.push_back(EncodeBaked(Vertex, Kind));
			}

			++Count;
		}

		Out.Corners.push_back(u8(Slot));
	}

	Out.Corners.resize((Out.Corners.size() + 3) & ~size_t(3), 0);

	u8 Places[MostVertices];
	u8 PlaceOf[MostVertices];
	u32 PlaceCount = 0;

	for (u32 Vertex = 0; Vertex < Count; ++Vertex)
	{
		// A tree's vertex is moved by its sway weight: the low half of its baked word
		const auto IsSame = [&](u8 Other)
		{
			return std::equal(Grid[Vertex], Grid[Vertex] + 3, Grid[Other]) && (!KeepsUv || std::equal(Uvs[Vertex], Uvs[Vertex] + 2, Uvs[Other])) &&
				(Kind != Baked::Tree || ((Out.BakedWords[FirstBaked + Vertex] ^ Out.BakedWords[FirstBaked + Other]) & 0xFFFFu) == 0);
		};

		const u32 Place = u32(std::find_if(Places, Places + PlaceCount, IsSame) - Places);
		PlaceOf[Vertex] = u8(Place);

		if (Place == PlaceCount)
		{
			Places[PlaceCount++] = u8(Vertex);
		}
	}

	Out.Corners.push_back(u8(PlaceCount));
	Out.Corners.insert(Out.Corners.end(), Places, Places + PlaceCount);

	for (u32 Corner = 0; Corner < TriangleCount * 3; ++Corner)
	{
		Out.Corners.push_back(PlaceOf[Out.Corners[size_t(Record.FirstCorner) * 4 + Corner]]);
	}

	Out.Corners.resize((Out.Corners.size() + 3) & ~size_t(3), 0);
	Out.Places += PlaceCount;
	u32* Header = Out.Attributes.data() + Record.Header;
	// The UV as the heap's vertex holds it, less the least of the cluster: its own steps, not a share of a range. A
	// terrain's is repeated hundreds of times by its detail textures over triangles tens of metres wide, and sixteen
	// bits of a share of the cluster's range moved them by texels. Only a cluster wider than sixteen bits of steps loses
	// the low ones
	u32 Shifts[2] = {};

	for (u32 Axis = 0; Axis < 2 && Count; ++Axis)
	{
		Shifts[Axis] = u32(std::max(0, s32(std::bit_width(u32(UvHigh[Axis] - UvLow[Axis]))) - 16));
		Header[4 + Axis] = u32(UvLow[Axis]);
		Header[6 + Axis] = Shifts[Axis];
	}

	for (u32 Vertex = 0; Vertex < Count; ++Vertex)
	{
		const u32 U = std::min(u32(Uvs[Vertex][0] - UvLow[0] + (1 << Shifts[0] >> 1)) >> Shifts[0], 0xFFFFu);
		const u32 V = std::min(u32(Uvs[Vertex][1] - UvLow[1] + (1 << Shifts[1] >> 1)) >> Shifts[1], 0xFFFFu);
		Header[HeaderWords + Vertex * 2 + 1] = U | V << 16;
	}

	for (u32 Axis = 0; Axis < 3 && Count; ++Axis)
	{
		const u32 Width = u32(std::bit_width(u32(High[Axis] - Low[Axis])));
		R_ASSERT(Width < 32 && Low[Axis] >= -0x800000 && Low[Axis] < 0x800000);
		Header[Axis] = (u32(Low[Axis]) & 0xFFFFFFu) | Width << 24;
	}

	Header[0] |= u32(Kind) << 29;
	Header[3] = u32(Out.Positions.size());
	Header[8] = FirstBaked;
	Record.VertexCount = Count;
	Out.Vertices += Count;
	u64 At = u64(Out.Positions.size()) << 5;

	for (u32 Vertex = 0; Vertex < Count; ++Vertex)
	{
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			PutBits(Out.Positions, At, u32(Grid[Vertex][Axis] - Low[Axis]), Header[Axis] >> 24 & 31u);
		}
	}

	Out.Positions.resize(size_t((At + 31) >> 5));
	Out.Clusters.push_back(Record);
}

u32 Join(Store& From, xr_vector<u32>& Words)
{
	const u32 PositionBase = u32(From.Attributes.size());
	// A vertex's position is read as four words from where it starts
	const u32 BakedBase = PositionBase + u32(From.Positions.size()) + 3;
	const u32 CornerBase = BakedBase + u32(From.BakedWords.size());
	R_ASSERT(u64(CornerBase) + From.Corners.size() / 4 < 0x100000000ull);

	for (const Cluster& Item : From.Clusters)
	{
		u32* Header = From.Attributes.data() + Item.Header;
		Header[3] += PositionBase;
		Header[8] = Header[8] == u32(-1) ? Header[8] : Header[8] + BakedBase;
	}

	Words.swap(From.Attributes);
	Words.insert(Words.end(), From.Positions.begin(), From.Positions.end());
	Words.resize(BakedBase, 0);
	Words.insert(Words.end(), From.BakedWords.begin(), From.BakedWords.end());
	Words.resize(size_t(CornerBase) + From.Corners.size() / 4);
	std::memcpy(Words.data() + CornerBase, From.Corners.data(), From.Corners.size());
	return CornerBase;
}
} // namespace ClusterPages

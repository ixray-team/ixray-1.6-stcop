#include "stdafx.h"
#include "DetailModel.h"

namespace
{
	constexpr u32 TrimAlphaRef = 16;
	constexpr u32 TrimMaxVerts = 8;
	constexpr float TrimMaxAreaRatio = 0.85f;

	struct TrimMask
	{
		xr_string Name;
		u32 Width = 0;
		u32 Height = 0;
		xr_vector<u64> Bits;

		bool Test(int X, int Y) const
		{
			const int W = int(Width), H = int(Height);
			const u32 I = u32((X % W + W) % W + (Y % H + H) % H * W);
			return (Bits[I >> 6] >> (I & 63)) & 1;
		}
	};

	struct TrimPolygon
	{
		u32 Mask;
		Fvector4 Rect;
		u32 Count;
		float Area;
		Fvector2 Points[TrimMaxVerts];
	};

	xr_vector<TrimMask> g_TrimMasks;
	xr_vector<TrimPolygon> g_TrimPolygons;

	u32 FindMask(const char* Name)
	{
		for (u32 i = 0; i < g_TrimMasks.size(); i++)
		{
			if (g_TrimMasks[i].Name == Name)
			{
				return i;
			}
		}
		TrimMask& Mask = g_TrimMasks.emplace_back();
		Mask.Name = Name;
		string_path Path;
		IReader* Reader = (FS.exist(Path, "$level$", Name, ".dds") || FS.exist(Path, _game_textures_, Name, ".dds")) ? FS.r_open(Path) : nullptr;
		xr_vector<u8> Alpha;
		if (Reader && GRHI->DecodeAlpha(Reader->pointer(), size_t(Reader->length()), Alpha, Mask.Width, Mask.Height))
		{
			Mask.Bits.assign((Alpha.size() + 63) >> 6, 0);
			for (size_t i = 0; i < Alpha.size(); i++)
			{
				Mask.Bits[i >> 6] |= u64(Alpha[i] >= TrimAlphaRef) << (i & 63);
			}
		}
		if (Reader)
		{
			FS.r_close(Reader);
		}
		if (Mask.Bits.empty())
		{
			Msg("! CDetail: cannot read alpha of '%s', cards are not trimmed", Name);
		}
		return u32(g_TrimMasks.size() - 1);
	}

	float Cross(const Fvector2& O, const Fvector2& A, const Fvector2& B)
	{
		return (A.x - O.x) * (B.y - O.y) - (A.y - O.y) * (B.x - O.x);
	}

	u32 BuildPolygon(const TrimMask& Mask, const Fvector4& Rect, Fvector2* Out, float& Area)
	{
		const float X0 = Rect.x * Mask.Width, Y0 = Rect.y * Mask.Height, X1 = Rect.z * Mask.Width, Y1 = Rect.w * Mask.Height;
		xr_vector<Fvector2> Points;
		for (int Y = int(std::floor(Y0)) - 1; Y < Y1 + 1.0f; Y++)
		{
			int Lo = INT_MAX, Hi = INT_MIN;
			for (int X = int(std::floor(X0)) - 1; X < X1 + 1.0f; X++)
			{
				if (Mask.Test(X, Y))
				{
					Lo = std::min(Lo, X);
					Hi = X;
				}
			}
			if (Lo > Hi)
			{
				continue;
			}
			for (const float PY : { Y - 1.0f, Y + 2.0f })
			{
				for (const int PX : { Lo - 1, Hi + 2 })
				{
					Points.push_back(Fvector2().set(std::clamp(float(PX), X0, X1), std::clamp(PY, Y0, Y1)));
				}
			}
		}
		if (Points.empty())
		{
			return 0;
		}

		std::sort(Points.begin(), Points.end(), [](const Fvector2& A, const Fvector2& B) { return A.x < B.x || (A.x == B.x && A.y < B.y); });
		xr_vector<Fvector2> Hull(Points.size() * 2 + 1);
		size_t K = 0;
		for (size_t i = 0; i < Points.size(); i++)
		{
			while (K >= 2 && Cross(Hull[K - 2], Hull[K - 1], Points[i]) <= 0.0f)
			{
				K--;
			}
			Hull[K++] = Points[i];
		}
		for (size_t i = Points.size() - 1, Lower = K + 1; i-- > 0;)
		{
			while (K >= Lower && Cross(Hull[K - 2], Hull[K - 1], Points[i]) <= 0.0f)
			{
				K--;
			}
			Hull[K++] = Points[i];
		}
		Hull.resize(K - 1);
		if (Hull.size() < 3)
		{
			return 0;
		}

		while (Hull.size() > TrimMaxVerts)
		{
			const size_t N = Hull.size();
			size_t Best = N;
			float BestArea = flt_max;
			Fvector2 BestPoint;
			for (size_t i = 0; i < N; i++)
			{
				const Fvector2& P = Hull[(i + N - 1) % N], &A = Hull[i], &B = Hull[(i + 1) % N], &Q = Hull[(i + 2) % N];
				const float D1x = A.x - P.x, D1y = A.y - P.y, D2x = B.x - Q.x, D2y = B.y - Q.y, Ex = B.x - A.x, Ey = B.y - A.y;
				const float Den = D1x * D2y - D1y * D2x;
				if (std::abs(Den) < 1e-6f)
				{
					continue;
				}
				const float T = (Ex * D2y - Ey * D2x) / Den, S = (Ex * D1y - Ey * D1x) / Den;
				Fvector2 X;
				X.set(A.x + D1x * T, A.y + D1y * T);
				const float Added = std::abs(Cross(A, X, B));
				if (T <= 0.0f || S <= 0.0f || Added >= BestArea || X.x < X0 - 1e-3f || X.x > X1 + 1e-3f || X.y < Y0 - 1e-3f || X.y > Y1 + 1e-3f)
				{
					continue;
				}
				Best = i;
				BestArea = Added;
				BestPoint.set(std::clamp(X.x, X0, X1), std::clamp(X.y, Y0, Y1));
			}
			if (Best == N)
			{
				return 0;
			}
			Hull[Best] = BestPoint;
			Hull.erase(Hull.begin() + (Best + 1) % N);
		}

		Area = 0.0f;
		for (size_t i = 2; i < Hull.size(); i++)
		{
			Area += 0.5f * Cross(Hull[0], Hull[i - 1], Hull[i]);
		}
		if (Area >= TrimMaxAreaRatio * (X1 - X0) * (Y1 - Y0))
		{
			return 0;
		}
		for (size_t i = 0; i < Hull.size(); i++)
		{
			Out[i].set(Hull[i].x / Mask.Width, Hull[i].y / Mask.Height);
		}
		Area /= float(Mask.Width) * float(Mask.Height);
		return u32(Hull.size());
	}

	const TrimPolygon& FindPolygon(u32 Mask, const Fvector4& Rect)
	{
		for (const TrimPolygon& Polygon : g_TrimPolygons)
		{
			if (Polygon.Mask == Mask && !memcmp(&Polygon.Rect, &Rect, sizeof(Rect)))
			{
				return Polygon;
			}
		}
		TrimPolygon& Polygon = g_TrimPolygons.emplace_back();
		Polygon.Mask = Mask;
		Polygon.Rect = Rect;
		Polygon.Count = BuildPolygon(g_TrimMasks[Mask], Rect, Polygon.Points, Polygon.Area);
		return Polygon;
	}
}

bool CDetail::Trim(xr_vector<fvfVertexIn>& Verts, xr_vector<u16>& Inds) const
{
	const u32 MaskIndex = FindMask(*Texture);
	const TrimMask& Mask = g_TrimMasks[MaskIndex];
	if (Mask.Bits.empty())
	{
		return false;
	}

	const u32 Faces = number_indices / 3;
	xr_vector<u32> Uses(number_vertices, 0);
	for (u32 i = 0; i < number_indices; i++)
	{
		if (indices[i] >= number_vertices)
		{
			return false;
		}
		Uses[indices[i]]++;
	}

	struct Card
	{
		u32 Face;
		u32 Count;
		fvfVertexIn Verts[TrimMaxVerts];
	};
	xr_vector<Card> Cards;
	xr_vector<u32> Owner(Faces, u32(-1));
	float Before = 0.0f, After = 0.0f;
	const auto Side = [](float Value, float Min, float Max)
	{
		const float Eps = (Max - Min) * 1e-3f;
		return std::abs(Value - Min) <= Eps ? 0u : std::abs(Value - Max) <= Eps ? 1u : 4u;
	};

	for (u32 F0 = 0; F0 < Faces; F0++)
	{
		const u16* A = indices + F0 * 3;
		if (Owner[F0] != u32(-1) || A[0] == A[1] || A[1] == A[2] || A[0] == A[2])
		{
			continue;
		}
		u32 F1 = F0 + 1, Lone = 0;
		u16 Extra = 0;
		for (; F1 < Faces; F1++)
		{
			const u16* B = indices + F1 * 3;
			u32 Shared = 0;
			Extra = B[0];
			for (u32 j = 0; j < 3; j++)
			{
				const bool InB = A[j] == B[0] || A[j] == B[1] || A[j] == B[2];
				Shared += InB;
				Lone = InB ? Lone : j;
				Extra = B[j] == A[0] || B[j] == A[1] || B[j] == A[2] ? Extra : B[j];
			}
			if (Shared == 2 && Uses[Extra] == 1 && Uses[A[Lone]] == 1 && Uses[A[(Lone + 1) % 3]] == 2 && Uses[A[(Lone + 2) % 3]] == 2)
			{
				break;
			}
		}
		if (F1 == Faces)
		{
			continue;
		}

		const u16 Quad[4] = { A[0], A[1], A[2], Extra };
		float U0 = flt_max, V0 = flt_max, U1 = -flt_max, V1 = -flt_max;
		for (const u16 I : Quad)
		{
			U0 = std::min(U0, vertices[I].u);
			V0 = std::min(V0, vertices[I].v);
			U1 = std::max(U1, vertices[I].u);
			V1 = std::max(V1, vertices[I].v);
		}
		if (U0 < -EPS_L || V0 < -EPS_L || U1 > 1.0f + EPS_L || V1 > 1.0f + EPS_L || (U1 - U0) * Mask.Width < 4.0f || (V1 - V0) * Mask.Height < 4.0f)
		{
			continue;
		}
		u32 Corner[4], Corners = 0;
		for (u32 j = 0; j < 4; j++)
		{
			Corner[j] = Side(vertices[Quad[j]].u, U0, U1) + Side(vertices[Quad[j]].v, V0, V1) * 2;
			Corners |= 1u << Corner[j];
		}
		if (Corners != 15 || (Corner[Lone] ^ Corner[3]) != 3)
		{
			continue;
		}

		const fvfVertexIn& Base = vertices[A[0]];
		Fvector E1, E2, AxisU, AxisV;
		E1.sub(vertices[A[1]].P, Base.P);
		E2.sub(vertices[A[2]].P, Base.P);
		const float D1u = vertices[A[1]].u - Base.u, D1v = vertices[A[1]].v - Base.v;
		const float D2u = vertices[A[2]].u - Base.u, D2v = vertices[A[2]].v - Base.v;
		const float Det = D1u * D2v - D1v * D2u;
		AxisU.mul(E1, D2v).mad(E2, -D1v).div(Det);
		AxisV.mul(E2, D1u).mad(E1, -D2u).div(Det);
		const auto Map = [&](float U, float V) { return Fvector().mad(Base.P, AxisU, U - Base.u).mad(AxisV, V - Base.v); };
		if (Map(vertices[Extra].u, vertices[Extra].v).distance_to(vertices[Extra].P) > 1e-3f * (E1.magnitude() + E2.magnitude()))
		{
			continue;
		}

		Fvector4 Rect;
		Rect.set(U0, V0, U1, V1);
		const TrimPolygon& Polygon = FindPolygon(MaskIndex, Rect);
		if (!Polygon.Count)
		{
			continue;
		}
		Card& C = Cards.emplace_back();
		C.Face = F0;
		C.Count = Polygon.Count;
		for (u32 k = 0; k < C.Count; k++)
		{
			const Fvector2& T = Polygon.Points[Det < 0.0f ? C.Count - 1 - k : k];
			C.Verts[k] = { Map(T.x, T.y), T.x, T.y };
		}
		Owner[F0] = Owner[F1] = u32(Cards.size() - 1);
		const float Scale = Fvector().crossproduct(AxisU, AxisV).magnitude();
		Before += Scale * (U1 - U0) * (V1 - V0);
		After += Scale * Polygon.Area;
	}
	if (Cards.empty())
	{
		return false;
	}

	xr_vector<u32> Remap(number_vertices, 0);
	for (u32 i = 0; i < number_indices; i++)
	{
		if (Owner[i / 3] != u32(-1))
		{
			Remap[indices[i]] = u32(-1);
		}
	}
	for (u32 v = 0; v < number_vertices; v++)
	{
		if (Remap[v] != u32(-1))
		{
			Remap[v] = u32(Verts.size());
			Verts.push_back(vertices[v]);
		}
	}
	for (u32 F = 0; F < Faces; F++)
	{
		if (Owner[F] == u32(-1))
		{
			Inds.insert(Inds.end(), { u16(Remap[indices[F * 3]]), u16(Remap[indices[F * 3 + 1]]), u16(Remap[indices[F * 3 + 2]]) });
			continue;
		}
		const Card& C = Cards[Owner[F]];
		if (C.Face != F)
		{
			continue;
		}
		const u32 First = u32(Verts.size());
		Verts.insert(Verts.end(), C.Verts, C.Verts + C.Count);
		for (u32 k = 2; k < C.Count; k++)
		{
			Inds.insert(Inds.end(), { u16(First), u16(First + k - 1), u16(First + k) });
		}
	}
	if (Verts.size() > 65535)
	{
		return false;
	}
	Msg("* CDetail: '%s' trimmed %u cards to %.0f%% area", *Texture, u32(Cards.size()), 100.0f * After / Before);
	return true;
}

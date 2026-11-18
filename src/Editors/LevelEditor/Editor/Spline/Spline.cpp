#include "stdafx.h"
#include "Spline.h"

using XRay::Editor::Spline::CSpline;
using XRay::Editor::Spline::SSplinePoint;
using XRay::Editor::Spline::SSplineSample;

namespace
{
	constexpr u16 SPLINE_VERSION = 1;

	template <typename T>
	T CatmullRom(const T& P0, const T& P1, const T& P2, const T& P3, float t)
	{
		const float T2 = t * t;
		const float T3 = T2 * t;
		return ((P1 * 2.f) + (P2 - P0) * t + (P0 * 2.f - P1 * 5.f + P2 * 4.f - P3) * T2 + (P1 * 3.f - P0 - P2 * 3.f + P3) * T3) * 0.5f;
	}

	Fvector CatmullRomV(const Fvector& P0, const Fvector& P1, const Fvector& P2, const Fvector& P3, float t)
	{
		Fvector R;
		R.x = CatmullRom(P0.x, P1.x, P2.x, P3.x, t);
		R.y = CatmullRom(P0.y, P1.y, P2.y, P3.y, t);
		R.z = CatmullRom(P0.z, P1.z, P2.z, P3.z, t);
		return R;
	}

	float DistanceRaySegment(const Fvector& Start, const Fvector& Dir, const Fvector& P)
	{
		Fvector D;
		D.sub(P, Start);
		const float T = std::max(0.f, D.dotproduct(Dir));
		Fvector Closest;
		Closest.mad(Start, Dir, T);
		return Closest.distance_to(P);
	}
}

const SSplinePoint& CSpline::At(int i) const
{
	const int N = int(Points.size());
	if (Closed)
	{
		return Points[((i % N) + N) % N];
	}
	return Points[clampr(i, 0, N - 1)];
}

u32 CSpline::SegmentCount() const
{
	if (Points.size() < 2)
	{
		return 0;
	}
	return Closed ? u32(Points.size()) : u32(Points.size() - 1);
}

SSplinePoint CSpline::Evaluate(float t) const
{
	SSplinePoint R;
	if (Points.empty())
	{
		return R;
	}
	if (Points.size() == 1)
	{
		return Points[0];
	}

	const u32 Count = SegmentCount();
	t = clampr(t, 0.f, float(Count));
	int Seg = std::min(iFloor(t), int(Count) - 1);
	const float F = t - Seg;

	const SSplinePoint& P0 = At(Seg - 1);
	const SSplinePoint& P1 = At(Seg);
	const SSplinePoint& P2 = At(Seg + 1);
	const SSplinePoint& P3 = At(Seg + 2);

	// Endpoints of an open spline: mirror the neighbour for a natural end tangent
	Fvector A = P0.Pos;
	Fvector D = P3.Pos;
	if (!Closed && Seg == 0)
	{
		A.lerp(P1.Pos, P2.Pos, -1.f);
	}
	if (!Closed && Seg + 2 >= int(Points.size()))
	{
		D.lerp(P2.Pos, P1.Pos, -1.f);
	}

	R.Pos = CatmullRomV(A, P1.Pos, P2.Pos, D, F);
	R.Width = P1.Width + (P2.Width - P1.Width) * F;
	R.Roll = P1.Roll + (P2.Roll - P1.Roll) * F;
	return R;
}

Fvector CSpline::EvaluateTangent(float t) const
{
	const float Count = float(SegmentCount());
	const float E = 0.01f;
	const float T0 = clampr(t - E, 0.f, Count);
	const float T1 = clampr(t + E, 0.f, Count);

	Fvector Tan;
	Tan.sub(Evaluate(T1).Pos, Evaluate(T0).Pos);
	if (Tan.square_magnitude() < EPS_S * EPS_S)
	{
		Tan.set(0, 0, 1);
	}
	Tan.normalize();
	return Tan;
}

void CSpline::Sample(float Step, xr_vector<SSplineSample>& Out) const
{
	Out.clear();
	if (!IsValid())
	{
		return;
	}

	Step = std::max(Step, 0.05f);
	const u32 Count = SegmentCount();

	// Dense pass to measure arc length, then resample evenly
	constexpr u32 Dense = 32;
	xr_vector<float> Params;
	xr_vector<float> Lengths;
	Params.reserve(Count * Dense + 1);
	Lengths.reserve(Count * Dense + 1);

	Fvector Prev = Evaluate(0.f).Pos;
	float Total = 0.f;
	Params.push_back(0.f);
	Lengths.push_back(0.f);
	for (u32 i = 1; i <= Count * Dense; ++i)
	{
		const float T = float(i) / float(Dense);
		const Fvector P = Evaluate(T).Pos;
		Total += P.distance_to(Prev);
		Prev = P;
		Params.push_back(T);
		Lengths.push_back(Total);
	}

	const u32 NumSamples = std::max(2u, u32(iCeil(Total / Step)) + 1);
	Out.reserve(NumSamples);

	size_t Cursor = 1;
	for (u32 i = 0; i < NumSamples; ++i)
	{
		const float Dist = Total * float(i) / float(NumSamples - 1);
		while (Cursor + 1 < Lengths.size() && Lengths[Cursor] < Dist)
		{
			++Cursor;
		}

		const float L0 = Lengths[Cursor - 1];
		const float L1 = Lengths[Cursor];
		const float F = (L1 - L0 > EPS_S) ? clampr((Dist - L0) / (L1 - L0), 0.f, 1.f) : 0.f;
		const float T = Params[Cursor - 1] + (Params[Cursor] - Params[Cursor - 1]) * F;

		const SSplinePoint SP = Evaluate(T);

		SSplineSample S;
		S.Pos = SP.Pos;
		S.Tangent = EvaluateTangent(T);
		S.Width = SP.Width;
		S.Distance = Dist;
		S.Param = T;

		// LH, +Y up: right = up x forward
		Fvector WorldUp = {0, 1, 0};
		S.Right.crossproduct(WorldUp, S.Tangent);
		if (S.Right.square_magnitude() < EPS_S)
		{
			S.Right.set(1, 0, 0);
		}
		S.Right.normalize();
		S.Up.crossproduct(S.Tangent, S.Right);
		S.Up.normalize();

		if (!fis_zero(SP.Roll))
		{
			Fvector R, U;
			R.mul(S.Right, cosf(SP.Roll)).mad(S.Up, sinf(SP.Roll));
			U.crossproduct(S.Tangent, R);
			S.Right = R.normalize();
			S.Up = U.normalize();
		}

		Out.push_back(S);
	}
}

int CSpline::PickPoint(const Fvector& Start, const Fvector& Dir, float Radius) const
{
	int Best = -1;
	float BestDist = Radius;
	for (size_t i = 0; i < Points.size(); ++i)
	{
		const float D = DistanceRaySegment(Start, Dir, Points[i].Pos);
		if (D < BestDist)
		{
			BestDist = D;
			Best = int(i);
		}
	}
	return Best;
}

u32 CSpline::FindInsertIndex(const Fvector& P) const
{
	if (Points.size() < 2)
	{
		return u32(Points.size());
	}

	// Closest point on the curve, by dense sampling
	const u32 Count = SegmentCount();
	float BestDist = flt_max;
	float BestT = 0.f;
	for (u32 i = 0; i <= Count * 16; ++i)
	{
		const float T = float(i) / 16.f;
		const float D = Evaluate(T).Pos.distance_to(P);
		if (D < BestDist)
		{
			BestDist = D;
			BestT = T;
		}
	}

	// Near an open end: extend the spline instead of inserting
	if (!Closed)
	{
		if (BestT <= 0.01f && Points.front().Pos.distance_to(P) > EPS_L)
		{
			Fvector Dir;
			Dir.sub(Points[1].Pos, Points[0].Pos);
			Fvector ToP;
			ToP.sub(P, Points[0].Pos);
			if (Dir.dotproduct(ToP) < 0.f)
			{
				return 0;
			}
		}
		if (BestT >= float(Count) - 0.01f)
		{
			return u32(Points.size());
		}
	}

	return std::min(u32(iFloor(BestT)) + 1, u32(Points.size()));
}

void CSpline::Save(IWriter& F) const
{
	F.w_u16(SPLINE_VERSION);
	F.w_u8(Closed ? 1 : 0);
	F.w_u32(u32(Points.size()));
	for (const SSplinePoint& P : Points)
	{
		F.w_fvector3(P.Pos);
		F.w_float(P.Width);
		F.w_float(P.Roll);
	}
}

bool CSpline::Load(IReader& F)
{
	const u16 Version = F.r_u16();
	if (Version != SPLINE_VERSION)
	{
		Msg("! Spline: unsupported version %u", Version);
		return false;
	}

	Closed = F.r_u8() != 0;
	const u32 Count = F.r_u32();
	Points.resize(Count);
	for (SSplinePoint& P : Points)
	{
		F.r_fvector3(P.Pos);
		P.Width = F.r_float();
		P.Roll = F.r_float();
	}
	return true;
}

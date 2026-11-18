#include "stdafx.h"
#include "TerrainMask.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::TerrainMask::CTerrainMask;
using XRay::Editor::TerrainMask::SCellWeights;
using XRay::Editor::TerrainMask::SLayer;
using XRay::Editor::TerrainMask::LAYER_COUNT;
using XRay::Editor::TerrainMask::PREVIEW_TEX_SIZE;

namespace
{
	constexpr u16 MASK_VERSION = 2; // 2: surface painting

	float Smooth(float E0, float E1, float X)
	{
		if (E1 <= E0)
		{
			return X >= E1 ? 1.f : 0.f;
		}
		const float T = clampr((X - E0) / (E1 - E0), 0.f, 1.f);
		return T * T * (3.f - 2.f * T);
	}

	// 1 inside [Lo, Hi], soft edges of width Blend outside
	float Band(float V, float Lo, float Hi, float Blend)
	{
		return Smooth(Lo - Blend, Lo, V) * (1.f - Smooth(Hi, Hi + Blend, V));
	}

	float Hash01(int X, int Z)
	{
		u32 N = u32(X) * 374761393u + u32(Z) * 668265263u;
		N = (N ^ (N >> 13)) * 1274126177u;
		N ^= N >> 16;
		return float(N & 0xFFFF) / 65535.f;
	}

	float ValueNoise(float X, float Z)
	{
		const int X0 = iFloor(X);
		const int Z0 = iFloor(Z);
		float Fx = X - X0;
		float Fz = Z - Z0;
		Fx = Fx * Fx * (3.f - 2.f * Fx);
		Fz = Fz * Fz * (3.f - 2.f * Fz);
		const float A = Hash01(X0, Z0) + (Hash01(X0 + 1, Z0) - Hash01(X0, Z0)) * Fx;
		const float B = Hash01(X0, Z0 + 1) + (Hash01(X0 + 1, Z0 + 1) - Hash01(X0, Z0 + 1)) * Fx;
		return A + (B - A) * Fz;
	}

	float Fbm(float X, float Z)
	{
		return ValueNoise(X, Z) * 0.6f + ValueNoise(X * 2.1f, Z * 2.1f) * 0.3f + ValueNoise(X * 4.3f, Z * 4.3f) * 0.1f;
	}

	void Blend(SCellWeights& C, u32 Layer, float W)
	{
		for (u32 i = 0; i < LAYER_COUNT; ++i)
		{
			C.W[i] *= (1.f - W);
		}
		C.W[Layer] += W;
	}

	void Pack(const SCellWeights& C, u8* Out)
	{
		float Sum = 0.f;
		for (float W : C.W)
		{
			Sum += std::max(W, 0.f);
		}
		if (Sum < EPS_S)
		{
			Out[0] = 255;
			Out[1] = Out[2] = Out[3] = 0;
			return;
		}

		int Total = 0;
		int Dominant = 0;
		for (u32 i = 0; i < LAYER_COUNT; ++i)
		{
			const int V = iFloor(std::max(C.W[i], 0.f) / Sum * 255.f + 0.5f);
			Out[i] = u8(clampr(V, 0, 255));
			Total += Out[i];
			if (Out[i] > Out[Dominant])
			{
				Dominant = int(i);
			}
		}
		// Exact sum of 255, the rounding error goes to the dominant channel
		Out[Dominant] = u8(clampr(int(Out[Dominant]) + 255 - Total, 0, 255));
	}

	u32 LerpColor(u32 A, u32 B, float T)
	{
		auto L = [&](u32 Ca, u32 Cb) { return u32(Ca + (float(Cb) - float(Ca)) * T); };
		return color_rgba(L(color_get_R(A), color_get_R(B)), L(color_get_G(A), color_get_G(B)), L(color_get_B(A), color_get_B(B)), 255);
	}
}

void XRay::Editor::TerrainMask::PackWeights(const SCellWeights& C, u8* Out)
{
	Pack(C, Out);
}

void SLayer::EnsureLoaded()
{
	if (Tried && Loaded == Texture)
	{
		return;
	}

	Tried = true;
	Loaded = Texture;
	Small.clear();
	AvgColor = 0xFF808080;

	if (!Texture.size())
	{
		return;
	}

	U32Vec Pixels;
	u32 W = 0, H = 0;
	if (!ImageLib.LoadTextureData(*Texture, Pixels, W, H) || W == 0 || H == 0)
	{
		Msg("! Terrain mask: can't load layer texture [%s]", *Texture);
		return;
	}

	// Box filter down to PREVIEW_TEX_SIZE^2
	const u32 S = PREVIEW_TEX_SIZE;
	Small.resize(S * S);
	double TR = 0, TG = 0, TB = 0;
	for (u32 y = 0; y < S; ++y)
	{
		for (u32 x = 0; x < S; ++x)
		{
			const u32 X0 = x * W / S, X1 = std::max(X0 + 1, (x + 1) * W / S);
			const u32 Y0 = y * H / S, Y1 = std::max(Y0 + 1, (y + 1) * H / S);
			double R = 0, G = 0, B = 0;
			u32 N = 0;
			for (u32 py = Y0; py < Y1 && py < H; ++py)
			{
				for (u32 px = X0; px < X1 && px < W; ++px)
				{
					const u32 C = Pixels[size_t(py) * W + px];
					R += color_get_R(C);
					G += color_get_G(C);
					B += color_get_B(C);
					++N;
				}
			}
			N = std::max(N, 1u);
			Small[y * S + x] = color_rgba(u32(R / N), u32(G / N), u32(B / N), 255);
			TR += R / N;
			TG += G / N;
			TB += B / N;
		}
	}
	AvgColor = color_rgba(u32(TR / (S * S)), u32(TG / (S * S)), u32(TB / (S * S)), 255);
}

u32 SLayer::SampleTiled(float WorldX, float WorldZ) const
{
	if (Small.empty())
	{
		return AvgColor;
	}

	const float S = float(PREVIEW_TEX_SIZE);
	const float Tile = std::max(TileSize, 0.1f);
	float U = WorldX / Tile;
	float V = WorldZ / Tile;
	U = (U - floorf(U)) * S;
	V = (V - floorf(V)) * S;

	const int X0 = iFloor(U) % int(PREVIEW_TEX_SIZE);
	const int Y0 = iFloor(V) % int(PREVIEW_TEX_SIZE);
	const int X1 = (X0 + 1) % int(PREVIEW_TEX_SIZE);
	const int Y1 = (Y0 + 1) % int(PREVIEW_TEX_SIZE);
	const float Fx = U - floorf(U);
	const float Fy = V - floorf(V);

	const u32 A = LerpColor(Small[Y0 * PREVIEW_TEX_SIZE + X0], Small[Y0 * PREVIEW_TEX_SIZE + X1], Fx);
	const u32 B = LerpColor(Small[Y1 * PREVIEW_TEX_SIZE + X0], Small[Y1 * PREVIEW_TEX_SIZE + X1], Fx);
	return LerpColor(A, B, Fy);
}

void CTerrainMask::Resize(u32 W, u32 H)
{
	if (W == Width && H == Height && Final.size() == size_t(W) * H * 4)
	{
		return;
	}

	Width = W;
	Height = H;
	const size_t N = size_t(W) * H;
	PaintWeights.assign(N * 4, 0);
	PaintCoverage.assign(N, 0);
	RoadLayer.assign(N, 255);
	RoadWeight.assign(N, 0);
	ShoulderLayer.assign(N, 255);
	ShoulderWeight.assign(N, 0);
	Final.assign(N * 4, 0);
	SurfaceColor.assign(N, 0xFF808080);
	SurfaceCoverage.assign(N, 0);
	Dirty = true;
	RoadsDirty = true;
}

bool CTerrainMask::HasPaint() const
{
	return std::any_of(PaintCoverage.begin(), PaintCoverage.end(), [](u8 C) { return C != 0; });
}

bool CTerrainMask::HasSurfacePaint() const
{
	return std::any_of(SurfaceCoverage.begin(), SurfaceCoverage.end(), [](u8 C) { return C != 0; });
}

void CTerrainMask::ClearSurfacePaint()
{
	std::fill(SurfaceCoverage.begin(), SurfaceCoverage.end(), u8(0));
	Dirty = true;
}

void CTerrainMask::PaintSurfaceCell(u32 X, u32 Z, u32 Color, float W, bool Erase)
{
	if (X >= Width || Z >= Height)
	{
		return;
	}

	const size_t Idx = size_t(Z) * Width + X;
	W = clampr(W, 0.f, 1.f);
	float Cov = SurfaceCoverage[Idx] / 255.f;

	if (Erase)
	{
		Cov *= (1.f - W);
	}
	else
	{
		// Without previous paint the new color replaces the stale buffer value
		SurfaceColor[Idx] = (Cov <= 0.f) ? Color : LerpColor(SurfaceColor[Idx], Color, W);
		Cov = Cov + (1.f - Cov) * W;
	}
	SurfaceCoverage[Idx] = u8(clampr(iFloor(Cov * 255.f + 0.5f), 0, 255));
}

u32 CTerrainMask::SurfaceAt(u32 X, u32 Z, u32 Base) const
{
	const size_t Idx = size_t(Z) * Width + X;
	if (Idx >= SurfaceCoverage.size() || !SurfaceCoverage[Idx])
	{
		return Base;
	}
	return LerpColor(Base, SurfaceColor[Idx], SurfaceCoverage[Idx] / 255.f);
}

u32 CTerrainMask::SampleSurface(float Gx, float Gz, u32 Base, float& OutCoverage) const
{
	OutCoverage = 0.f;
	if (Width < 2 || Height < 2 || SurfaceCoverage.empty())
	{
		return Base;
	}

	Gx = clampr(Gx, 0.f, float(Width - 1));
	Gz = clampr(Gz, 0.f, float(Height - 1));
	const u32 X0 = std::min(u32(Gx), Width - 2);
	const u32 Z0 = std::min(u32(Gz), Height - 2);
	const float Fx = Gx - X0;
	const float Fz = Gz - Z0;

	// Coverage-weighted bilinear filter, so unpainted neighbours don't darken the edge
	float R = 0, G = 0, B = 0, Cw = 0, Wsum = 0;
	const u32 Xs[2] = {X0, X0 + 1};
	const u32 Zs[2] = {Z0, Z0 + 1};
	for (u32 j = 0; j < 2; ++j)
	{
		for (u32 i = 0; i < 2; ++i)
		{
			const float Wb = (i ? Fx : 1.f - Fx) * (j ? Fz : 1.f - Fz);
			const size_t Idx = size_t(Zs[j]) * Width + Xs[i];
			const float C = SurfaceCoverage[Idx] / 255.f;
			const u32 Col = SurfaceColor[Idx];
			R += color_get_R(Col) * C * Wb;
			G += color_get_G(Col) * C * Wb;
			B += color_get_B(Col) * C * Wb;
			Cw += C * Wb;
			Wsum += Wb;
		}
	}

	OutCoverage = Wsum > 0.f ? Cw / Wsum : 0.f;
	if (Cw <= 0.f)
	{
		return Base;
	}
	const u32 Painted = color_rgba(u32(R / Cw), u32(G / Cw), u32(B / Cw), 255);
	return LerpColor(Base, Painted, OutCoverage);
}

void CTerrainMask::ClearPaint()
{
	std::fill(PaintWeights.begin(), PaintWeights.end(), u8(0));
	std::fill(PaintCoverage.begin(), PaintCoverage.end(), u8(0));
	Dirty = true;
}

SCellWeights CTerrainMask::ComputeAuto(const SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X, int Z) const
{
	SCellWeights C = {{1.f, 0.f, 0.f, 0.f}};

	auto LocalAt = [&](int Px, int Pz) -> Fvector
	{
		Fvector P;
		P.x = -(Px * HM.StepX() - HM.HalfX());
		P.z = Pz * HM.StepZ() - HM.HalfZ();
		P.y = HM.ToLocalY(HM.GetHeightClamped(Px, Pz), ScaleY);
		return P;
	};

	const Fvector L = LocalAt(X, Z);
	Fvector W;
	Xform.transform_tiny(W, L);

	Fvector Tx, Tz, N;
	Tx.sub(LocalAt(X + 1, Z), LocalAt(X - 1, Z));
	Tz.sub(LocalAt(X, Z + 1), LocalAt(X, Z - 1));
	N.crossproduct(Tz, Tx);
	if (N.y < 0.f)
	{
		N.invert();
	}
	N.normalize_safe();
	const float SlopeDeg = rad2deg(acosf(clampr(N.y, -1.f, 1.f)));

	for (u32 i = 1; i < LAYER_COUNT; ++i)
	{
		const SLayer& Ly = Layers[i];
		if (!Ly.AutoEnabled)
		{
			continue;
		}

		float Wt = Band(W.y, Ly.HeightMin, Ly.HeightMax, Ly.HeightBlend) * Band(SlopeDeg, Ly.SlopeMin, Ly.SlopeMax, Ly.SlopeBlend);
		if (Ly.Noise > 0.f)
		{
			const float Freq = 1.f / std::max(Ly.NoiseScale, 0.1f);
			const float Nz = Fbm(W.x * Freq + i * 17.3f, W.z * Freq - i * 9.1f);
			Wt = clampr(Wt + (Nz - 0.5f) * Ly.Noise, 0.f, 1.f);
		}

		if (Wt > 0.f)
		{
			Blend(C, i, Wt);
		}
	}
	return C;
}

void CTerrainMask::RebuildFinal(const SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X0, int Z0, int X1, int Z1)
{
	if (Width == 0 || Height == 0)
	{
		return;
	}

	X0 = std::max(X0, 0);
	Z0 = std::max(Z0, 0);
	X1 = std::min(X1, int(Width) - 1);
	Z1 = std::min(Z1, int(Height) - 1);
	if (X0 > X1 || Z0 > Z1)
	{
		return;
	}

	xr_parallel_for(Z0, Z1 + 1, [&](int z)
	{
		for (int x = X0; x <= X1; ++x)
		{
			const size_t Idx = size_t(z) * Width + x;
			SCellWeights C = ComputeAuto(HM, Xform, ScaleY, x, z);

			// Hand painting over the rules
			const float Cov = PaintCoverage[Idx] / 255.f;
			if (Cov > 0.f)
			{
				for (u32 i = 0; i < LAYER_COUNT; ++i)
				{
					C.W[i] = C.W[i] * (1.f - Cov) + (PaintWeights[Idx * 4 + i] / 255.f) * Cov;
				}
			}

			// Roads on top: shoulders first, then the road surface
			if (ShoulderLayer[Idx] < LAYER_COUNT && ShoulderWeight[Idx])
			{
				Blend(C, ShoulderLayer[Idx], ShoulderWeight[Idx] / 255.f);
			}
			if (RoadLayer[Idx] < LAYER_COUNT && RoadWeight[Idx])
			{
				Blend(C, RoadLayer[Idx], RoadWeight[Idx] / 255.f);
			}

			Pack(C, &Final[Idx * 4]);
		}
	});
}

void CTerrainMask::RebuildFinal(const SHeightMap& HM, const Fmatrix& Xform, float ScaleY)
{
	RebuildFinal(HM, Xform, ScaleY, 0, 0, int(Width) - 1, int(Height) - 1);
	Dirty = false;
}

SCellWeights CTerrainMask::GetFinal(u32 X, u32 Z) const
{
	SCellWeights C = {{1.f, 0.f, 0.f, 0.f}};
	if (X >= Width || Z >= Height)
	{
		return C;
	}
	const u8* P = &Final[(size_t(Z) * Width + X) * 4];
	for (u32 i = 0; i < LAYER_COUNT; ++i)
	{
		C.W[i] = P[i] / 255.f;
	}
	return C;
}

SCellWeights CTerrainMask::SampleFinal(float Gx, float Gz) const
{
	Gx = clampr(Gx, 0.f, float(Width - 1));
	Gz = clampr(Gz, 0.f, float(Height - 1));
	const u32 X0 = std::min(u32(Gx), Width - 2);
	const u32 Z0 = std::min(u32(Gz), Height - 2);
	const float Fx = Gx - X0;
	const float Fz = Gz - Z0;

	const SCellWeights A = GetFinal(X0, Z0), B = GetFinal(X0 + 1, Z0), C = GetFinal(X0, Z0 + 1), D = GetFinal(X0 + 1, Z0 + 1);
	SCellWeights R;
	for (u32 i = 0; i < LAYER_COUNT; ++i)
	{
		const float Top = A.W[i] + (B.W[i] - A.W[i]) * Fx;
		const float Bot = C.W[i] + (D.W[i] - C.W[i]) * Fx;
		R.W[i] = Top + (Bot - Top) * Fz;
	}
	return R;
}

void CTerrainMask::PaintCell(u32 X, u32 Z, u32 Layer, float W, bool Erase)
{
	if (X >= Width || Z >= Height || Layer >= LAYER_COUNT)
	{
		return;
	}

	const size_t Idx = size_t(Z) * Width + X;
	float Cov = PaintCoverage[Idx] / 255.f;
	W = clampr(W, 0.f, 1.f);

	if (Erase)
	{
		Cov *= (1.f - W);
		PaintCoverage[Idx] = u8(iFloor(Cov * 255.f + 0.5f));
		return;
	}

	SCellWeights P;
	if (Cov <= 0.f)
	{
		P = {{0.f, 0.f, 0.f, 0.f}};
		P.W[Layer] = 1.f;
	}
	else
	{
		for (u32 i = 0; i < LAYER_COUNT; ++i)
		{
			P.W[i] = PaintWeights[Idx * 4 + i] / 255.f;
		}
		Blend(P, Layer, W);
	}

	Cov = Cov + (1.f - Cov) * W;
	PaintCoverage[Idx] = u8(clampr(iFloor(Cov * 255.f + 0.5f), 0, 255));
	Pack(P, &PaintWeights[Idx * 4]);
}

u32 CTerrainMask::PreviewColor(u32 X, u32 Z, float WorldX, float WorldZ) const
{
	const SCellWeights C = GetFinal(X, Z);
	float R = 0, G = 0, B = 0;
	for (u32 i = 0; i < LAYER_COUNT; ++i)
	{
		if (C.W[i] <= 0.f)
		{
			continue;
		}
		const u32 T = Layers[i].SampleTiled(WorldX, WorldZ);
		R += color_get_R(T) * C.W[i];
		G += color_get_G(T) * C.W[i];
		B += color_get_B(T) * C.W[i];
	}
	const u32 Base = color_rgba(u32(clampr(R, 0.f, 255.f)), u32(clampr(G, 0.f, 255.f)), u32(clampr(B, 0.f, 255.f)), 255);
	return SurfaceAt(X, Z, Base);
}

void CTerrainMask::Save(IWriter& F) const
{
	F.w_u16(MASK_VERSION);
	for (const SLayer& L : Layers)
	{
		F.w_stringZ(*L.Texture ? *L.Texture : "");
		F.w_float(L.TileSize);
		F.w_u8(L.AutoEnabled ? 1 : 0);
		F.w_float(L.HeightMin);
		F.w_float(L.HeightMax);
		F.w_float(L.HeightBlend);
		F.w_float(L.SlopeMin);
		F.w_float(L.SlopeMax);
		F.w_float(L.SlopeBlend);
		F.w_float(L.Noise);
		F.w_float(L.NoiseScale);
	}

	const bool Paint = HasPaint();
	F.w_u32(Width);
	F.w_u32(Height);
	F.w_u8(Paint ? 1 : 0);
	if (Paint)
	{
		F.w(PaintWeights.data(), u32(PaintWeights.size()));
		F.w(PaintCoverage.data(), u32(PaintCoverage.size()));
	}

	const bool Surface = HasSurfacePaint();
	F.w_u8(Surface ? 1 : 0);
	if (Surface)
	{
		F.w(SurfaceColor.data(), u32(SurfaceColor.size() * sizeof(u32)));
		F.w(SurfaceCoverage.data(), u32(SurfaceCoverage.size()));
	}
}

bool CTerrainMask::Load(IReader& F)
{
	const u16 Version = F.r_u16();
	if (Version < 1 || Version > MASK_VERSION)
	{
		Msg("! Terrain mask: unsupported version %u", Version);
		return false;
	}

	for (SLayer& L : Layers)
	{
		shared_str Str;
		F.r_stringZ(Str);
		L.Texture = Str;
		L.TileSize = F.r_float();
		L.AutoEnabled = F.r_u8() != 0;
		L.HeightMin = F.r_float();
		L.HeightMax = F.r_float();
		L.HeightBlend = F.r_float();
		L.SlopeMin = F.r_float();
		L.SlopeMax = F.r_float();
		L.SlopeBlend = F.r_float();
		L.Noise = F.r_float();
		L.NoiseScale = F.r_float();
		L.Tried = false;
	}

	const u32 W = F.r_u32();
	const u32 H = F.r_u32();
	const bool Paint = F.r_u8() != 0;

	Width = Height = 0;
	Resize(W, H);
	if (Paint)
	{
		F.r(PaintWeights.data(), u32(PaintWeights.size()));
		F.r(PaintCoverage.data(), u32(PaintCoverage.size()));
	}

	if (Version >= 2 && F.r_u8() != 0)
	{
		F.r(SurfaceColor.data(), u32(SurfaceColor.size() * sizeof(u32)));
		F.r(SurfaceCoverage.data(), u32(SurfaceCoverage.size()));
	}
	return true;
}

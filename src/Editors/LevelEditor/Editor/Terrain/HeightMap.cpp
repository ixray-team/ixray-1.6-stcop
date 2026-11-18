#include "stdafx.h"
#include "HeightMap.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::HeightmapUtils::SHeightMapChunk;
using XRay::Editor::HeightmapUtils::SHeightMapRenderData;
using XRay::Editor::HeightmapUtils::SHeightMapVertex;


void SHeightMapRenderData::Clear()
{
	InvalidateGpu();
	Chunks.clear();
	ChunksX = ChunksZ = 0;
	IsLayoutDirty = true;
}

Fvector SHeightMap::GridToLocal(float gx, float gz, float scaleY) const
{
	Fvector P;
	P.x = -(gx * StepX() - HalfX());
	P.z = gz * StepZ() - HalfZ();
	P.y = ToLocalY(Sample(gx, gz), scaleY);
	return P;
}

void SHeightMap::LocalToGrid(const Fvector& local, float& gx, float& gz) const
{
	gx = (HalfX() - local.x) / StepX();
	gz = (local.z + HalfZ()) / StepZ();
}

float SHeightMap::Sample(float gx, float gz) const
{
	if (!IsValid())
	{
		return 0.f;
	}

	gx = clampr(gx, 0.f, float(Width - 1));
	gz = clampr(gz, 0.f, float(Height - 1));

	const int X0 = std::min(iFloor(gx), int(Width) - 2);
	const int Z0 = std::min(iFloor(gz), int(Height) - 2);
	const float Fx = gx - X0;
	const float Fz = gz - Z0;

	const float H00 = GetHeight(X0, Z0);
	const float H10 = GetHeight(X0 + 1, Z0);
	const float H01 = GetHeight(X0, Z0 + 1);
	const float H11 = GetHeight(X0 + 1, Z0 + 1);

	const float A = H00 + (H10 - H00) * Fx;
	const float B = H01 + (H11 - H01) * Fx;
	return A + (B - A) * Fz;
}

void SHeightMap::Create(u32 w, u32 h, float fill)
{
	Width = w;
	Height = h;
	Data.assign(size_t(w) * h, fill);
	MinH = MaxH = fill;
	RenderData.Clear();
}

void SHeightMap::Resample(u32 w, u32 h)
{
	if (!IsValid() || w < 2 || h < 2 || (w == Width && h == Height))
	{
		return;
	}

	xr_vector<float> NewData(size_t(w) * h);
	const float Sx = float(Width - 1) / float(w - 1);
	const float Sz = float(Height - 1) / float(h - 1);

	for (u32 z = 0; z < h; ++z)
	{
		for (u32 x = 0; x < w; ++x)
		{
			NewData[size_t(z) * w + x] = Sample(x * Sx, z * Sz);
		}
	}

	Width = w;
	Height = h;
	Data.swap(NewData);
	UpdateMinMax();
	RenderData.Clear();
}

void SHeightMap::UpdateMinMax()
{
	if (Data.empty())
	{
		MinH = MaxH = 0.f;
		return;
	}

	auto [Min, Max] = std::minmax_element(Data.begin(), Data.end());
	MinH = *Min;
	MaxH = *Max;
}

bool SHeightMap::LoadRAW(const char* filename)
{
	if (!FS.TryLoad(filename))
	{
		return false;
	}

	IReader* F = FS.r_open(filename);
	const bool Result = LoadSteam(F);
	FS.r_close(F);
	return Result;
}

bool SHeightMap::SaveSteam(IWriter* Writer) const
{
	if (!Writer || !IsValid())
	{
		return false;
	}

	xr_vector<u16> Raw(Data.size());
	for (size_t i = 0; i < Data.size(); ++i)
	{
		Raw[i] = u16(iFloor(clampr(Data[i], 0.f, 1.f) * 65535.f + 0.5f));
	}

	Writer->w(Raw.data(), u32(Raw.size() * sizeof(u16)));
	return true;
}

bool SHeightMap::LoadSteam(IReader* Reader)
{
	if (!Reader)
	{
		return false;
	}

	const size_t Samples = Reader->length() / sizeof(u16);
	const u32 Side = u32(sqrt(double(Samples)) + 0.5);
	if (Side < 2 || size_t(Side) * Side != Samples)
	{
		Msg("! Heightmap: invalid R16 stream size [%u bytes], expected square map", Reader->length());
		return false;
	}

	Width = Height = Side;
	Data.resize(Samples);

	const u16* Raw = (const u16*)Reader->pointer();
	for (size_t i = 0; i < Samples; ++i)
	{
		Data[i] = float(Raw[i]) / 65535.f;
	}

	UpdateMinMax();
	RenderData.Clear();
	return true;
}

void SHeightMap::SetPreviewTexture(xr_vector<u32>&& Pixels, u32 W, u32 H)
{
	if (Pixels.size() != size_t(W) * H)
	{
		Pixels.clear();
		W = H = 0;
	}

	PreviewTex = std::move(Pixels);
	PreviewTexW = W;
	PreviewTexH = H;
	MarkDirty();
}

u32 SHeightMap::SamplePreviewTexture(float u, float v) const
{
	if (PreviewTex.empty())
	{
		return 0xFFFFFFFF;
	}

	const float Fx = clampr(u, 0.f, 1.f) * float(PreviewTexW - 1);
	const float Fy = clampr(v, 0.f, 1.f) * float(PreviewTexH - 1);
	const int X0 = iFloor(Fx);
	const int Y0 = iFloor(Fy);
	const int X1 = std::min(X0 + 1, int(PreviewTexW) - 1);
	const int Y1 = std::min(Y0 + 1, int(PreviewTexH) - 1);
	const float Tx = Fx - X0;
	const float Ty = Fy - Y0;

	auto At = [&](int X, int Y) { return PreviewTex[size_t(Y) * PreviewTexW + X]; };
	const u32 C00 = At(X0, Y0), C10 = At(X1, Y0), C01 = At(X0, Y1), C11 = At(X1, Y1);

	auto Mix = [&](u32 (*Get)(u32)) -> u8
	{
		const float A = Get(C00) + (float(Get(C10)) - Get(C00)) * Tx;
		const float B = Get(C01) + (float(Get(C11)) - Get(C01)) * Tx;
		return u8(clampr(A + (B - A) * Ty, 0.f, 255.f));
	};

	return color_rgba(Mix(color_get_R), Mix(color_get_G), Mix(color_get_B), 255);
}

void SHeightMap::MarkDirty()
{
	RenderData.IsLayoutDirty = true;
}

void SHeightMap::MarkDirtyRect(int x0, int z0, int x1, int z1)
{
	SHeightMapRenderData& RD = RenderData;
	if (RD.IsLayoutDirty || RD.Chunks.empty())
	{
		return;
	}

	// Normals use neighbours, so expand by one sample
	const int CS = int(SHeightMapRenderData::CHUNK_SIZE);
	const int Cx0 = clampr((x0 - 1) / CS, 0, int(RD.ChunksX) - 1);
	const int Cx1 = clampr((x1 + 1) / CS, 0, int(RD.ChunksX) - 1);
	const int Cz0 = clampr((z0 - 1) / CS, 0, int(RD.ChunksZ) - 1);
	const int Cz1 = clampr((z1 + 1) / CS, 0, int(RD.ChunksZ) - 1);

	for (int cz = Cz0; cz <= Cz1; ++cz)
	{
		for (int cx = Cx0; cx <= Cx1; ++cx)
		{
			RD.Chunks[cz * RD.ChunksX + cx].IsDirty = true;
		}
	}
}



bool SHeightMap::RayPickLocal(float& distance, const Fvector& start, const Fvector& direction, float scaleY, Fvector* outPoint) const
{
	if (!IsValid())
	{
		return false;
	}

	Fvector Dir = direction;
	Dir.normalize_safe();

	Fbox Box;
	Box.min.set(-HalfX(), ToLocalY(MinH, scaleY) - 0.01f, -HalfZ());
	Box.max.set(HalfX(), ToLocalY(MaxH, scaleY) + 0.01f, HalfZ());

	float TStart = 0.f;
	if (!Box.contains(start))
	{
		Fvector Entry;
		if (Box.Pick2(start, Dir, Entry) == Fbox::rpNone)
		{
			return false;
		}
		TStart = start.distance_to(Entry);
	}

	Fvector BoxSize;
	Box.getsize(BoxSize);
	const float TEnd = std::min(distance, TStart + BoxSize.magnitude() + 0.1f);
	const float Step = 0.5f * std::min(StepX(), StepZ());

	auto IsHole = [&](float gx, float gz)
	{
		const int X = clampr(iFloor(gx), 0, int(Width) - 2);
		const int Z = clampr(iFloor(gz), 0, int(Height) - 2);
		return GetHeight(X, Z) <= 0.f || GetHeight(X + 1, Z) <= 0.f || GetHeight(X, Z + 1) <= 0.f || GetHeight(X + 1, Z + 1) <= 0.f;
	};

	// Returns ray height above surface (positive = above), or FLT_MAX outside/in a hole
	auto Above = [&](float t) -> float
	{
		Fvector P;
		P.mad(start, Dir, t);
		float Gx, Gz;
		LocalToGrid(P, Gx, Gz);
		if (Gx < 0.f || Gz < 0.f || Gx > float(Width - 1) || Gz > float(Height - 1) || IsHole(Gx, Gz))
		{
			return FLT_MAX;
		}
		return P.y - ToLocalY(Sample(Gx, Gz), scaleY);
	};

	float PrevT = TStart;
	for (float T = TStart; T <= TEnd; T += Step)
	{
		const float A = Above(T);
		if (A != FLT_MAX && A <= 0.f)
		{
			float Lo = PrevT;
			float Hi = T;
			for (int i = 0; i < 12; ++i)
			{
				const float Mid = (Lo + Hi) * 0.5f;
				const float M = Above(Mid);
				if (M != FLT_MAX && M <= 0.f)
				{
					Hi = Mid;
				}
				else
				{
					Lo = Mid;
				}
			}

			distance = Hi;
			if (outPoint)
			{
				outPoint->mad(start, Dir, Hi);
			}
			return true;
		}
		PrevT = T;
	}

	return false;
}

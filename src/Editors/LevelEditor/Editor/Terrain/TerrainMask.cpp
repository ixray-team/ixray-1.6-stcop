#include "stdafx.h"
#include "TerrainMask.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using namespace XRay::Editor::TerrainMask;

namespace
{
	constexpr u16 MASK_VERSION = 3; // 3: multi-material terrain

	const CLASS_ID CLSID_BmmD = MK_CLSID('B', 'm', 'm', 'D', 'o', 'l', 'd', ' ');

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

	u32 LerpColor(u32 A, u32 B, float T)
	{
		auto L = [&](u32 Ca, u32 Cb) { return u32(Ca + (float(Cb) - float(Ca)) * T); };
		return color_rgba(L(color_get_R(A), color_get_R(B)), L(color_get_G(A), color_get_G(B)), L(color_get_B(A), color_get_B(B)), 255);
	}

	// Flat color per material when its shader details are unknown
	u32 MaterialTint(u32 Index)
	{
		static const u32 Palette[] = {0xFF6E8A48, 0xFF8A7050, 0xFF6A6A70, 0xFFB8A070, 0xFF507A90, 0xFF905070, 0xFF90904A, 0xFF4A9080};
		return Palette[Index % (sizeof(Palette) / sizeof(Palette[0]))];
	}

	bool SameTexture(const shared_str& A, const shared_str& B)
	{
		const char* Sa = A.size() ? *A : "";
		const char* Sb = B.size() ? *B : "";
		return xr_stricmp(Sa, Sb) == 0;
	}

	const char* ChannelName(u32 C)
	{
		static const char* Names[] = {"R", "G", "B", "A"};
		return C < CHANNEL_COUNT ? Names[C] : "?";
	}
}

// ---------------------------------------------------------------------------------------------------------------------

bool XRay::Editor::TerrainMask::ReadShaderDetails(const char* ShaderName, shared_str Out[CHANNEL_COUNT])
{
	for (u32 i = 0; i < CHANNEL_COUNT; ++i)
	{
		Out[i] = nullptr;
	}

	if (!ShaderName || !ShaderName[0])
	{
		return false;
	}

	IBlender* B = DEV->_FindBlender(ShaderName);
	if (!B || B->getDescription().CLS != CLSID_BmmD)
	{
		return false;
	}

	// Parse the property stream of the blender: u32 type, stringZ name, data
	CMemoryWriter W;
	B->Save(W);
	IReader R(W.pointer(), W.size());
	R.advance(sizeof(B->getDescription()));

	static const char* Keys[CHANNEL_COUNT] = {"R2-R", "R2-G", "R2-B", "R2-A"};
	u32 Found = 0;
	string256 Name;
	while (R.elapsed() >= int(sizeof(u32)))
	{
		const u32 Type = R.r_u32();
		R.r_stringZ(Name, sizeof(Name));

		size_t Size = 0;
		switch (Type)
		{
		case xrPID_MARKER:
		case xrPID_MARKER_TEMPLATE:
			Size = 0;
			break;
		case xrPID_MATRIX:
		case xrPID_CONSTANT:
		case xrPID_TEXTURE:
		case xrPID_OBJECT:
		case xrPID_STRING:
			Size = sizeof(string64);
			break;
		case xrPID_INTEGER:
			Size = sizeof(xrP_Integer);
			break;
		case xrPID_FLOAT:
			Size = sizeof(xrP_Float);
			break;
		case xrPID_BOOL:
			Size = sizeof(xrP_BOOL);
			break;
		case xrPID_TOKEN:
		{
			xrP_TOKEN T;
			R.r(&T, sizeof(T));
			R.advance(T.Count * sizeof(xrP_TOKEN::Item));
			continue;
		}
		case xrPID_CLSID:
		{
			xrP_CLSID C;
			R.r(&C, sizeof(C));
			R.advance(C.Count * sizeof(CLASS_ID));
			continue;
		}
		default:
			return Found == CHANNEL_COUNT; // Unknown property: stop parsing
		}

		if (Type == xrPID_TEXTURE)
		{
			string64 Value;
			R.r(Value, sizeof(Value));
			Value[sizeof(Value) - 1] = 0;
			for (u32 i = 0; i < CHANNEL_COUNT; ++i)
			{
				if (!xr_strcmp(Name, Keys[i]))
				{
					Out[i] = Value;
					++Found;
				}
			}
			continue;
		}
		R.advance(Size);
	}

	return Found == CHANNEL_COUNT;
}

void SPreviewTexture::EnsureLoaded()
{
	if (Tried && Loaded == Texture)
	{
		return;
	}

	Tried = true;
	Loaded = Texture;
	Small.clear();
	AvgColor = 0xFF808080;

	if (!Texture.size() || !xr_strcmp(*Texture, "$null"))
	{
		return;
	}

	U32Vec Pixels;
	u32 W = 0, H = 0;
	if (!ImageLib.LoadTextureData(*Texture, Pixels, W, H) || W == 0 || H == 0)
	{
		Msg("! Terrain: can't load detail texture [%s]", *Texture);
		return;
	}

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

u32 SPreviewTexture::SampleTiled(float WorldX, float WorldZ, float TileSize) const
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

	const int N = int(PREVIEW_TEX_SIZE);
	const int X0 = iFloor(U) % N;
	const int Y0 = iFloor(V) % N;
	const int X1 = (X0 + 1) % N;
	const int Y1 = (Y0 + 1) % N;
	const float Fx = U - floorf(U);
	const float Fy = V - floorf(V);

	const u32 A = LerpColor(Small[Y0 * N + X0], Small[Y0 * N + X1], Fx);
	const u32 B = LerpColor(Small[Y1 * N + X0], Small[Y1 * N + X1], Fx);
	return LerpColor(A, B, Fy);
}

void SMaterial::EnsureDetails()
{
	if (DetailsFor.size() && DetailsFor == Shader)
	{
		for (auto& D : Details)
		{
			D.EnsureLoaded();
		}
		return;
	}

	DetailsFor = Shader.size() ? Shader : shared_str("");
	shared_str Names[CHANNEL_COUNT];
	DetailsValid = ReadShaderDetails(Shader.size() ? *Shader : "", Names);
	if (!DetailsValid && Shader.size())
	{
		Msg("! Terrain material [%s]: shader [%s] is not a BmmD level shader, details unknown", Name.size() ? *Name : "", *Shader);
	}

	for (u32 i = 0; i < CHANNEL_COUNT; ++i)
	{
		Details[i].Texture = Names[i];
		Details[i].Tried = false;
		Details[i].EnsureLoaded();
	}
}

void XRay::Editor::TerrainMask::PackWeights(const SCellWeights& C, u8* Out)
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
	for (u32 i = 0; i < CHANNEL_COUNT; ++i)
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

// ---------------------------------------------------------------------------------------------------------------------

void CTerrainMask::EnsureDefaultMaterial(const char* Shader, const char* GameMtl)
{
	// Classic X-Ray terrain: one level shader, the mask picks one of its four details.
	// So the default is one material per channel on the same shader: painting "G" renders the shader's R2-G,
	// and the materials can never seam with each other.
	static const char* Names[CHANNEL_COUNT] = {"layer R", "layer G", "layer B", "layer A"};

	if (Materials.empty())
	{
		for (u32 c = 0; c < CHANNEL_COUNT; ++c)
		{
			SMaterial M;
			M.Name = Names[c];
			M.Shader = Shader;
			M.GameMtl = GameMtl;
			M.Channel = c;
			Materials.push_back(M);
		}
		Dirty = true;
		return;
	}

	// Terrains made with the single "base" material: add the missing channels on the same shader
	if (Materials.size() == 1 && Materials[0].Name == shared_str("base"))
	{
		const SMaterial Base = Materials[0];
		Materials[0].Name = Names[std::min(Base.Channel, CHANNEL_COUNT - 1)];
		for (u32 c = 0; c < CHANNEL_COUNT; ++c)
		{
			if (c == Base.Channel)
			{
				continue;
			}
			SMaterial M;
			M.Name = Names[c];
			M.Shader = Base.Shader;
			M.GameMtl = Base.GameMtl;
			M.Channel = c;
			Materials.push_back(M);
		}
		Dirty = true;
	}
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
	PaintId.assign(N, NO_MATERIAL);
	RoadId.assign(N, NO_MATERIAL);
	MaterialId.assign(N, 0);
	Final.assign(N * 4, 0);
	SeamFlag.assign(N, 0);
	SurfaceColor.assign(N, 0xFF808080);
	SurfaceCoverage.assign(N, 0);
	Dirty = true;
	RoadsDirty = true;
}

bool CTerrainMask::HasPaint() const
{
	return std::any_of(PaintId.begin(), PaintId.end(), [](u8 C) { return C != NO_MATERIAL; });
}

bool CTerrainMask::HasSurfacePaint() const
{
	return std::any_of(SurfaceCoverage.begin(), SurfaceCoverage.end(), [](u8 C) { return C != 0; });
}

void CTerrainMask::ClearPaint()
{
	std::fill(PaintId.begin(), PaintId.end(), NO_MATERIAL);
	Dirty = true;
}

void CTerrainMask::ClearSurfacePaint()
{
	std::fill(SurfaceCoverage.begin(), SurfaceCoverage.end(), u8(0));
	Dirty = true;
}

void CTerrainMask::EnsureDetails()
{
	for (SMaterial& M : Materials)
	{
		M.EnsureDetails();
	}
}

u8 CTerrainMask::ComputeAuto(const SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X, int Z) const
{
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

	u8 Id = 0;
	for (u32 i = 1; i < u32(Materials.size()); ++i)
	{
		const SMaterial& M = Materials[i];
		if (!M.AutoEnabled)
		{
			continue;
		}

		float Wt = Band(W.y, M.HeightMin, M.HeightMax, M.HeightBlend) * Band(SlopeDeg, M.SlopeMin, M.SlopeMax, M.SlopeBlend);
		if (M.Noise > 0.f)
		{
			const float Freq = 1.f / std::max(M.NoiseScale, 0.1f);
			const float Nz = Fbm(W.x * Freq + i * 17.3f, W.z * Freq - i * 9.1f);
			Wt = Wt + (Nz - 0.5f) * M.Noise;
		}

		// Later materials win
		if (Wt > 0.5f)
		{
			Id = u8(i);
		}
	}
	return Id;
}

void CTerrainMask::Rebuild(const SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X0, int Z0, int X1, int Z1)
{
	if (Width < 2 || Height < 2 || Materials.empty())
	{
		return;
	}

	const int R = int(std::min(BlendRadius, 16u));
	const u32 MatCount = u32(Materials.size());

	// Material ids: rect + blend radius (the blur and seam test read neighbours)
	const int IX0 = std::max(X0 - R - 1, 0), IZ0 = std::max(Z0 - R - 1, 0);
	const int IX1 = std::min(X1 + R + 1, int(Width) - 1), IZ1 = std::min(Z1 + R + 1, int(Height) - 1);
	xr_parallel_for(IZ0, IZ1 + 1, [&](int z)
	{
		for (int x = IX0; x <= IX1; ++x)
		{
			const size_t Idx = size_t(z) * Width + x;
			u8 Id;
			if (RoadId[Idx] != NO_MATERIAL)
			{
				Id = RoadId[Idx];
			}
			else if (PaintId[Idx] != NO_MATERIAL)
			{
				Id = PaintId[Idx];
			}
			else
			{
				Id = ComputeAuto(HM, Xform, ScaleY, x, z);
			}
			MaterialId[Idx] = (Id < MatCount) ? Id : 0;
		}
	});

	// Compatibility of every material pair
	xr_vector<u8> Compat(size_t(MatCount) * MatCount, 1);
	for (u32 a = 0; a < MatCount; ++a)
	{
		for (u32 b = a + 1; b < MatCount; ++b)
		{
			const u8 Ok = Compatible(Materials[a], Materials[b], nullptr) ? 1 : 0;
			Compat[a * MatCount + b] = Compat[b * MatCount + a] = Ok;
		}
	}

	// Mask: blurred one-hot channel of the material, plus seam flags
	const int MX0 = std::max(X0 - R, 0), MZ0 = std::max(Z0 - R, 0);
	const int MX1 = std::min(X1 + R, int(Width) - 1), MZ1 = std::min(Z1 + R, int(Height) - 1);
	xr_parallel_for(MZ0, MZ1 + 1, [&](int z)
	{
		for (int x = MX0; x <= MX1; ++x)
		{
			SCellWeights C = {{0.f, 0.f, 0.f, 0.f}};
			float Total = 0.f;
			for (int dz = -R; dz <= R; ++dz)
			{
				const int Sz = clampr(z + dz, 0, int(Height) - 1);
				for (int dx = -R; dx <= R; ++dx)
				{
					const int Sx = clampr(x + dx, 0, int(Width) - 1);
					// Round-ish kernel with a soft falloff
					const float D = _sqrt(float(dx * dx + dz * dz)) / float(R + 1);
					if (D > 1.f)
					{
						continue;
					}
					const float Wt = 1.f - D * D;
					const u32 Ch = std::min(Materials[MaterialId[size_t(Sz) * Width + Sx]].Channel, CHANNEL_COUNT - 1);
					C.W[Ch] += Wt;
					Total += Wt;
				}
			}
			if (Total <= 0.f)
			{
				C.W[std::min(Materials[MaterialId[size_t(z) * Width + x]].Channel, CHANNEL_COUNT - 1)] = 1.f;
			}

			const size_t Idx = size_t(z) * Width + x;
			PackWeights(C, &Final[Idx * 4]);

			const u8 Me = MaterialId[Idx];
			u8 Seam = 0;
			const int Nx[4] = {x - 1, x + 1, x, x};
			const int Nz[4] = {z, z, z - 1, z + 1};
			for (int k = 0; k < 4 && !Seam; ++k)
			{
				if (Nx[k] < 0 || Nz[k] < 0 || Nx[k] >= int(Width) || Nz[k] >= int(Height))
				{
					continue;
				}
				const u8 Other = MaterialId[size_t(Nz[k]) * Width + Nx[k]];
				if (Other != Me && !Compat[size_t(Me) * MatCount + Other])
				{
					Seam = 1;
				}
			}
			SeamFlag[Idx] = Seam;
		}
	});
}

void CTerrainMask::Rebuild(const SHeightMap& HM, const Fmatrix& Xform, float ScaleY)
{
	Rebuild(HM, Xform, ScaleY, 0, 0, int(Width) - 1, int(Height) - 1);
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
	for (u32 i = 0; i < CHANNEL_COUNT; ++i)
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
	for (u32 i = 0; i < CHANNEL_COUNT; ++i)
	{
		const float Top = A.W[i] + (B.W[i] - A.W[i]) * Fx;
		const float Bot = C.W[i] + (D.W[i] - C.W[i]) * Fx;
		R.W[i] = Top + (Bot - Top) * Fz;
	}
	return R;
}

u8 CTerrainMask::MaterialAt(u32 X, u32 Z) const
{
	if (X >= Width || Z >= Height || MaterialId.empty())
	{
		return 0;
	}
	return MaterialId[size_t(Z) * Width + X];
}

u8 CTerrainMask::QuadMaterial(u32 X, u32 Z) const
{
	const u8 Ids[4] = {MaterialAt(X, Z), MaterialAt(X + 1, Z), MaterialAt(X, Z + 1), MaterialAt(X + 1, Z + 1)};
	u8 Best = Ids[0];
	int BestCount = 0;
	for (int i = 0; i < 4; ++i)
	{
		int Count = 0;
		for (int j = 0; j < 4; ++j)
		{
			Count += (Ids[j] == Ids[i]) ? 1 : 0;
		}
		if (Count > BestCount)
		{
			BestCount = Count;
			Best = Ids[i];
		}
	}
	return Best;
}

void CTerrainMask::BuildQuadMaterials(xr_vector<u8>& Out) const
{
	Out.clear();
	if (Width < 2 || Height < 2)
	{
		return;
	}
	Out.resize(size_t(Width - 1) * (Height - 1));
	for (u32 z = 0; z + 1 < Height; ++z)
	{
		for (u32 x = 0; x + 1 < Width; ++x)
		{
			Out[size_t(z) * (Width - 1) + x] = QuadMaterial(x, z);
		}
	}
}

void CTerrainMask::PaintCell(u32 X, u32 Z, u8 Material, bool Erase)
{
	if (X >= Width || Z >= Height)
	{
		return;
	}
	PaintId[size_t(Z) * Width + X] = Erase ? NO_MATERIAL : Material;
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

u32 CTerrainMask::PreviewColor(u32 X, u32 Z, float WorldX, float WorldZ) const
{
	const u8 Id = MaterialAt(X, Z);
	if (Id >= Materials.size())
	{
		return 0xFF808080;
	}

	const SMaterial& M = Materials[Id];
	u32 Base;
	if (M.DetailsValid)
	{
		// What the game renders: this polygon's shader details mixed by the mask
		const SCellWeights C = GetFinal(X, Z);
		float R = 0, G = 0, B = 0;
		for (u32 i = 0; i < CHANNEL_COUNT; ++i)
		{
			if (C.W[i] <= 0.f)
			{
				continue;
			}
			const u32 T = M.Details[i].SampleTiled(WorldX, WorldZ, M.TileSize);
			R += color_get_R(T) * C.W[i];
			G += color_get_G(T) * C.W[i];
			B += color_get_B(T) * C.W[i];
		}
		Base = color_rgba(u32(clampr(R, 0.f, 255.f)), u32(clampr(G, 0.f, 255.f)), u32(clampr(B, 0.f, 255.f)), 255);
	}
	else
	{
		Base = MaterialTint(Id);
	}

	Base = SurfaceAt(X, Z, Base);

	if (ShowSeams && SeamFlag.size() == size_t(Width) * Height && SeamFlag[size_t(Z) * Width + X])
	{
		Base = LerpColor(Base, 0xFFFF2020, 0.65f);
	}
	return Base;
}

u32 CTerrainMask::BaseColor(u32 X, u32 Z) const
{
	const u8 Id = MaterialAt(X, Z);
	if (Id >= Materials.size())
	{
		return 0xFF808080;
	}

	const SMaterial& M = Materials[Id];
	if (!M.DetailsValid)
	{
		return MaterialTint(Id);
	}

	const SCellWeights C = GetFinal(X, Z);
	float R = 0, G = 0, B = 0;
	for (u32 i = 0; i < CHANNEL_COUNT; ++i)
	{
		const u32 T = M.Details[i].AvgColor;
		R += color_get_R(T) * C.W[i];
		G += color_get_G(T) * C.W[i];
		B += color_get_B(T) * C.W[i];
	}
	return color_rgba(u32(clampr(R, 0.f, 255.f)), u32(clampr(G, 0.f, 255.f)), u32(clampr(B, 0.f, 255.f)), 255);
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

bool CTerrainMask::Compatible(const SMaterial& A, const SMaterial& B, xr_string* Why)
{
	if (!A.DetailsValid || !B.DetailsValid)
	{
		return true; // Unknown details: nothing to check
	}

	const u32 Ca = std::min(A.Channel, CHANNEL_COUNT - 1);
	const u32 Cb = std::min(B.Channel, CHANNEL_COUNT - 1);
	const char* Na = A.Name.size() ? *A.Name : "?";
	const char* Nb = B.Name.size() ? *B.Name : "?";

	if (Ca == Cb)
	{
		if (!SameTexture(A.Details[Ca].Texture, B.Details[Cb].Texture))
		{
			if (Why)
			{
				*Why = xr_string(Na) + " and " + Nb + " both render channel " + ChannelName(Ca) + " with different details";
			}
			return false;
		}
		return true;
	}

	// Each side must have the other's rendered detail in the other's channel
	if (!SameTexture(A.Details[Cb].Texture, B.Details[Cb].Texture))
	{
		if (Why)
		{
			*Why = xr_string(Na) + ": channel " + ChannelName(Cb) + " = [" + (A.Details[Cb].Texture.size() ? *A.Details[Cb].Texture : "") + "], " + Nb + " renders [" + (B.Details[Cb].Texture.size() ? *B.Details[Cb].Texture : "") + "]";
		}
		return false;
	}
	if (!SameTexture(B.Details[Ca].Texture, A.Details[Ca].Texture))
	{
		if (Why)
		{
			*Why = xr_string(Nb) + ": channel " + ChannelName(Ca) + " = [" + (B.Details[Ca].Texture.size() ? *B.Details[Ca].Texture : "") + "], " + Na + " renders [" + (A.Details[Ca].Texture.size() ? *A.Details[Ca].Texture : "") + "]";
		}
		return false;
	}
	return true;
}

void CTerrainMask::CollectSeams(xr_vector<SSeam>& Out) const
{
	Out.clear();
	const u32 MatCount = u32(Materials.size());
	if (Width < 2 || Height < 2 || MatCount < 2)
	{
		return;
	}

	xr_vector<u32> Counts(size_t(MatCount) * MatCount, 0);
	for (u32 z = 0; z < Height; ++z)
	{
		for (u32 x = 0; x < Width; ++x)
		{
			const u8 Me = MaterialId[size_t(z) * Width + x];
			if (x + 1 < Width)
			{
				const u8 O = MaterialId[size_t(z) * Width + x + 1];
				if (O != Me)
				{
					++Counts[size_t(std::min(Me, O)) * MatCount + std::max(Me, O)];
				}
			}
			if (z + 1 < Height)
			{
				const u8 O = MaterialId[size_t(z + 1) * Width + x];
				if (O != Me)
				{
					++Counts[size_t(std::min(Me, O)) * MatCount + std::max(Me, O)];
				}
			}
		}
	}

	for (u32 a = 0; a < MatCount; ++a)
	{
		for (u32 b = a + 1; b < MatCount; ++b)
		{
			const u32 Count = Counts[size_t(a) * MatCount + b];
			xr_string Why;
			if (Count && !Compatible(Materials[a], Materials[b], &Why))
			{
				Out.push_back({u8(a), u8(b), Count, Why});
			}
		}
	}
}

void CTerrainMask::Save(IWriter& F) const
{
	F.w_u16(MASK_VERSION);
	F.w_u32(BlendRadius);
	F.w_u8(ShowSeams ? 1 : 0);
	F.w_u32(u32(Materials.size()));
	for (const SMaterial& M : Materials)
	{
		F.w_stringZ(M.Name.size() ? *M.Name : "");
		F.w_stringZ(M.Shader.size() ? *M.Shader : "");
		F.w_stringZ(M.GameMtl.size() ? *M.GameMtl : "");
		F.w_u32(M.Channel);
		F.w_float(M.TileSize);
		F.w_u8(M.AutoEnabled ? 1 : 0);
		F.w_float(M.HeightMin);
		F.w_float(M.HeightMax);
		F.w_float(M.HeightBlend);
		F.w_float(M.SlopeMin);
		F.w_float(M.SlopeMax);
		F.w_float(M.SlopeBlend);
		F.w_float(M.Noise);
		F.w_float(M.NoiseScale);
	}

	F.w_u32(Width);
	F.w_u32(Height);

	const bool Paint = HasPaint();
	F.w_u8(Paint ? 1 : 0);
	if (Paint)
	{
		F.w(PaintId.data(), u32(PaintId.size()));
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

	Materials.clear();

	if (Version < 3)
	{
		// Old 4-layer format: skip the layers and the per-layer painting, keep the surface painting
		shared_str Str;
		for (u32 i = 0; i < 4; ++i)
		{
			F.r_stringZ(Str);
			F.r_float();
			F.r_u8();
			for (int k = 0; k < 8; ++k)
			{
				F.r_float();
			}
		}
		const u32 W = F.r_u32();
		const u32 H = F.r_u32();
		Width = Height = 0;
		Resize(W, H);
		if (F.r_u8() != 0)
		{
			F.advance(size_t(W) * H * 5);
		}
		if (Version >= 2 && F.r_u8() != 0)
		{
			F.r(SurfaceColor.data(), u32(SurfaceColor.size() * sizeof(u32)));
			F.r(SurfaceCoverage.data(), u32(SurfaceCoverage.size()));
		}
		return true;
	}

	BlendRadius = F.r_u32();
	ShowSeams = F.r_u8() != 0;
	const u32 Count = std::min(F.r_u32(), MAX_MATERIALS);
	Materials.resize(Count);
	for (SMaterial& M : Materials)
	{
		shared_str Str;
		F.r_stringZ(Str);
		M.Name = Str;
		F.r_stringZ(Str);
		M.Shader = Str;
		F.r_stringZ(Str);
		M.GameMtl = Str;
		M.Channel = std::min(F.r_u32(), CHANNEL_COUNT - 1);
		M.TileSize = F.r_float();
		M.AutoEnabled = F.r_u8() != 0;
		M.HeightMin = F.r_float();
		M.HeightMax = F.r_float();
		M.HeightBlend = F.r_float();
		M.SlopeMin = F.r_float();
		M.SlopeMax = F.r_float();
		M.SlopeBlend = F.r_float();
		M.Noise = F.r_float();
		M.NoiseScale = F.r_float();
	}

	const u32 W = F.r_u32();
	const u32 H = F.r_u32();
	Width = Height = 0;
	Resize(W, H);

	if (F.r_u8() != 0)
	{
		F.r(PaintId.data(), u32(PaintId.size()));
	}
	if (F.r_u8() != 0)
	{
		F.r(SurfaceColor.data(), u32(SurfaceColor.size() * sizeof(u32)));
		F.r(SurfaceCoverage.data(), u32(SurfaceCoverage.size()));
	}
	return true;
}

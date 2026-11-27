#include "StdAfx.h"
#include "SandboxTerrain.h"

#include <fstream>

namespace Sandbox
{

const char* AnalyticPresetName(EAnalyticPreset preset)
{
	switch (preset)
	{
	case EAnalyticPreset::Plane:		return "plane";
	case EAnalyticPreset::SineWaves:	return "sine";
	case EAnalyticPreset::Slope:		return "slope";
	case EAnalyticPreset::Crater:		return "crater";
	case EAnalyticPreset::Steps:		return "steps";
	default:							return "unknown";
	}
}

bool AnalyticPresetFromString(const char* name, EAnalyticPreset& out)
{
	if (name == nullptr)
		return false;
	for (u32 i = 0; i < 5; ++i)
	{
		const EAnalyticPreset preset = EAnalyticPreset(i);
		if (0 == _stricmp(name, AnalyticPresetName(preset)))
		{
			out = preset;
			return true;
		}
	}
	return false;
}

static float SampleAnalytic(const STerrainParams& params, float x, float z)
{
	switch (params.Analytic)
	{
	case EAnalyticPreset::Plane:
		return 0.f;
	case EAnalyticPreset::SineWaves:
		return params.Amplitude * sinf(x * params.Frequency * PI_MUL_2) * cosf(z * params.Frequency * PI_MUL_2);
	case EAnalyticPreset::Slope:
		return params.Amplitude * (x / params.Size);
	case EAnalyticPreset::Crater:
	{
		const float Sigma = params.Size * 0.15f;
		return -params.Amplitude * expf(-(x * x + z * z) / (2.f * Sigma * Sigma));
	}
	case EAnalyticPreset::Steps:
	{
		const float StepWidth = params.Size / 5.f;
		const float Index = clampr(floorf((x + params.Size * 0.5f) / StepWidth), 0.f, 4.f);
		return params.Amplitude * Index * 0.25f;
	}
	default:
		return 0.f;
	}
}

// Bilinear sample of a normalized [0, 1] height grid at (u, v) in [0, 1]².
static float SampleHeightGrid(const xr_vector<float>& heights, u32 width, u32 height, float u, float v)
{
	if (width == 0 || height == 0 || heights.size() < size_t(width) * height)
		return 0.f;

	const float Fx = clampr(u, 0.f, 1.f) * float(width - 1);
	const float Fz = clampr(v, 0.f, 1.f) * float(height - 1);
	const u32 X0 = u32(Fx);
	const u32 Z0 = u32(Fz);
	const u32 X1 = std::min(X0 + 1, width - 1);
	const u32 Z1 = std::min(Z0 + 1, height - 1);
	const float Tx = Fx - float(X0);
	const float Tz = Fz - float(Z0);

	const float H00 = heights[Z0 * width + X0];
	const float H10 = heights[Z0 * width + X1];
	const float H01 = heights[Z1 * width + X0];
	const float H11 = heights[Z1 * width + X1];
	const float Top = H00 + (H10 - H00) * Tx;
	const float Bottom = H01 + (H11 - H01) * Tx;
	return Top + (Bottom - Top) * Tz;
}

// Maps (x, z) on the [-Size/2, Size/2] patch onto the heightmap and scales by HeightScale.
static float SampleHeightmap(const STerrainParams& params, float x, float z)
{
	xr_vector<float> Heights;
	u32 Width = 0, Height = 0;
	if (!LoadHeightmap(params.HeightmapPath, Heights, Width, Height))
		return 0.f;

	const float U = x / params.Size + 0.5f;
	const float V = z / params.Size + 0.5f;
	return SampleHeightGrid(Heights, Width, Height, U, V) * params.HeightScale;
}

float SampleHeight(const STerrainParams& params, float x, float z)
{
	switch (params.Kind)
	{
	case ETerrainKind::Flat:
		return 0.f;
	case ETerrainKind::Analytic:
		return SampleAnalytic(params, x, z);
	case ETerrainKind::Heightmap:
		return SampleHeightmap(params, x, z);
	case ETerrainKind::PrebuiltLevel:
		// No procedural surface; the prebuilt level brings its own geometry.
		return 0.f;
	default:
		return 0.f;
	}
}

STerrainMesh BuildTerrainMesh(const STerrainParams& params)
{
	STerrainMesh Mesh;
	Mesh.Bounds.invalidate();

	if (params.Kind == ETerrainKind::PrebuiltLevel || params.Cell <= 0.f || params.Size <= 0.f)
		return Mesh;

	const int Cells = std::max(1, iFloor(params.Size / params.Cell + 0.5f));
	const int Side = Cells + 1;
	const float Half = float(Cells) * params.Cell * 0.5f;

	// Load the heightmap once instead of per-vertex re-reads through SampleHeight.
	xr_vector<float> Heights;
	u32 HeightsW = 0, HeightsH = 0;
	const bool UseHeightmap = params.Kind == ETerrainKind::Heightmap && LoadHeightmap(params.HeightmapPath, Heights, HeightsW, HeightsH);
	if (params.Kind == ETerrainKind::Heightmap && !UseHeightmap)
		Msg("! Sandbox::BuildTerrainMesh: heightmap '%s' unavailable, building flat terrain", params.HeightmapPath.c_str());

	Mesh.Verts.reserve(size_t(Side) * Side);
	for (int Z = 0; Z < Side; ++Z)
	{
		for (int X = 0; X < Side; ++X)
		{
			Fvector Position;
			Position.x = -Half + float(X) * params.Cell;
			Position.z = -Half + float(Z) * params.Cell;
			if (UseHeightmap)
			{
				// Same mapping as SampleHeight: patch [-Size/2, Size/2] -> [0, 1]².
				const float U = Position.x / params.Size + 0.5f;
				const float V = Position.z / params.Size + 0.5f;
				Position.y = SampleHeightGrid(Heights, HeightsW, HeightsH, U, V) * params.HeightScale;
			}
			else
			{
				Position.y = SampleHeight(params, Position.x, Position.z);
			}
			Mesh.Bounds.modify(Position);
			Mesh.Verts.push_back(Position);
		}
	}

	Mesh.Tris.reserve(size_t(Cells) * Cells * 2);
	for (int Z = 0; Z < Cells; ++Z)
	{
		for (int X = 0; X < Cells; ++X)
		{
			const u32 V00 = u32(Z * Side + X);
			const u32 V10 = V00 + 1;
			const u32 V01 = V00 + u32(Side);
			const u32 V11 = V01 + 1;

			// Winding gives both triangles an up-facing (+Y) front face.
			CDB::TRI Tri0 = {};
			Tri0.verts[0] = V00;
			Tri0.verts[1] = V01;
			Tri0.verts[2] = V10;
			Mesh.Tris.push_back(Tri0);

			CDB::TRI Tri1 = {};
			Tri1.verts[0] = V10;
			Tri1.verts[1] = V01;
			Tri1.verts[2] = V11;
			Mesh.Tris.push_back(Tri1);
		}
	}
	return Mesh;
}

bool LoadHeightmap(const xr_string& path, xr_vector<float>& outHeights, u32& outW, u32& outH)
{
	outHeights.clear();
	outW = 0;
	outH = 0;

	std::ifstream Stream(path.c_str(), std::ios::binary);
	if (!Stream.is_open())
	{
		Msg("! Sandbox::LoadHeightmap: cannot open '%s'", path.c_str());
		return false;
	}

	// Reads the next whitespace-delimited token, skipping '#' comments.
	const auto NextToken = [&Stream](xr_string& OutToken) -> bool
	{
		OutToken.clear();
		for (;;)
		{
			const int Ch = Stream.get();
			if (Ch == EOF)
				return false;
			if (Ch == '#')
			{
				Stream.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
				continue;
			}
			if (Ch > ' ')
			{
				OutToken += char(Ch);
				break;
			}
		}
		for (;;)
		{
			const int Ch = Stream.peek();
			if (Ch == EOF || Ch <= ' ' || Ch == '#')
				break;
			OutToken += char(Stream.get());
		}
		return true;
	};

	xr_string Magic;
	if (!NextToken(Magic) || (Magic != "P2" && Magic != "P5"))
	{
		Msg("! Sandbox::LoadHeightmap: '%s' is not a PGM (P2/P5) file", path.c_str());
		return false;
	}

	xr_string Token;
	u32 Width = 0, Height = 0, MaxValue = 0;
	if (!NextToken(Token))
		return false;
	Width = u32(atoi(Token.c_str()));
	if (!NextToken(Token))
		return false;
	Height = u32(atoi(Token.c_str()));
	if (!NextToken(Token))
		return false;
	MaxValue = u32(atoi(Token.c_str()));

	if (Width == 0 || Height == 0 || MaxValue == 0 || MaxValue > 65535)
	{
		Msg("! Sandbox::LoadHeightmap: bad header in '%s'", path.c_str());
		return false;
	}

	outHeights.reserve(size_t(Width) * Height);

	if (Magic == "P2")
	{
		for (u32 I = 0; I < Width * Height; ++I)
		{
			if (!NextToken(Token))
			{
				Msg("! Sandbox::LoadHeightmap: truncated ascii raster in '%s'", path.c_str());
				return false;
			}
			outHeights.push_back(float(atoi(Token.c_str())) / float(MaxValue));
		}
	}
	else
	{
		// Exactly one whitespace character separates the header from the raster.
		Stream.get();
		const bool TwoBytes = MaxValue > 255;
		for (u32 I = 0; I < Width * Height; ++I)
		{
			u32 Sample = 0;
			if (TwoBytes)
			{
				// 16-bit samples are big-endian in PGM.
				const int Hi = Stream.get();
				const int Lo = Stream.get();
				if (Hi == EOF || Lo == EOF)
				{
					Msg("! Sandbox::LoadHeightmap: truncated binary raster in '%s'", path.c_str());
					return false;
				}
				Sample = (u32(Hi) << 8) | u32(Lo);
			}
			else
			{
				const int Byte = Stream.get();
				if (Byte == EOF)
				{
					Msg("! Sandbox::LoadHeightmap: truncated binary raster in '%s'", path.c_str());
					return false;
				}
				Sample = u32(Byte);
			}
			outHeights.push_back(float(Sample) / float(MaxValue));
		}
	}

	outW = Width;
	outH = Height;
	return true;
}

} // namespace Sandbox

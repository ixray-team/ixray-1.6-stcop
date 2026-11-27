#pragma once

#include "../xrCore/Collision/xrCDB.h"

// Procedural terrain generation for sandbox levels.
// A terrain is a square patch centered on the origin in X/Z; the height
// function y = f(x, z) is defined analytically, loaded from a heightmap,
// or taken from a prebuilt level (no procedural surface at all).
namespace Sandbox
{
	enum class ETerrainKind
	{
		Flat,
		Analytic,
		Heightmap,
		PrebuiltLevel,
	};

	enum class EAnalyticPreset
	{
		Plane,
		SineWaves,
		Slope,
		Crater,
		Steps,
	};

	struct STerrainParams
	{
		ETerrainKind	Kind = ETerrainKind::Flat;
		float			Size = 200.f;		// meters, square patch centered on origin in X/Z
		float			Cell = 2.f;			// grid cell size
		EAnalyticPreset	Analytic = EAnalyticPreset::SineWaves;
		float			Amplitude = 5.f;
		float			Frequency = 0.05f;
		xr_string		HeightmapPath;		// PGM (P2/P5) file, 8/16-bit
		float			HeightScale = 20.f;
		xr_string		PrebuiltLevel;		// folder name under the levels dir for PrebuiltLevel
	};

	struct STerrainMesh
	{
		xr_vector<Fvector>	Verts;
		xr_vector<CDB::TRI>	Tris;
		Fbox				Bounds;
	};

	// y = f(x, z); Flat => 0, Analytic => preset formula, Heightmap => sampled
	// (heightmap sampling re-reads the file on every call: meant for spot
	// queries; the builders load it once and sample the cached grid).
	float			SampleHeight(const STerrainParams& params, float x, float z);
	// Regular grid, 2 triangles per cell, centered on origin.
	// CDB::TRI material = 0, suppress flags = 0.
	STerrainMesh	BuildTerrainMesh(const STerrainParams& params);
	// Hand-written PGM parser (P2 ascii / P5 binary, 8/16-bit), no new deps.
	// Heights are normalized to [0, 1] by the file's max value.
	bool			LoadHeightmap(const xr_string& path, xr_vector<float>& outHeights, u32& outW, u32& outH);

	const char*		AnalyticPresetName(EAnalyticPreset preset);
	bool			AnalyticPresetFromString(const char* name, EAnalyticPreset& out);
}

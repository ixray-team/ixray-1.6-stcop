#pragma once
#include "HeightMap.h"

namespace XRay::Editor::HeightmapUtils
{
	struct STerrainSurfaceTemplate
	{
		const char* Shader;
		const char* ShaderXRLC;
		const char* GameMtl;
		const char* Texture;
		const char* Name = nullptr; // Surface name (multi-material terrain)
	};

	void GenerateMeshByHeightmap(const SHeightMap& heightmap, CEditableObject* OutMesh, int ScaleY, const STerrainSurfaceTemplate& Surface);

	// Multi-material terrain: one surface per material. QuadMaterial holds a material index
	// per grid quad ((W - 1) * (H - 1)), indices out of range fall back to material 0.
	void GenerateMeshByHeightmap(const SHeightMap& heightmap, CEditableObject* OutMesh, int ScaleY,
		const xr_vector<STerrainSurfaceTemplate>& Materials, const xr_vector<u8>& QuadMaterial);
	void GenerateHeightmapByMesh(CEditableObject* Mesh, const xr_string& OutputFile);
};
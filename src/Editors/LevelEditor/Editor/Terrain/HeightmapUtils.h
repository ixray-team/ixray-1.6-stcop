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
	};

	void GenerateMeshByHeightmap(const SHeightMap& heightmap, CEditableObject* OutMesh, int ScaleY, const STerrainSurfaceTemplate& Surface);
	void GenerateHeightmapByMesh(CEditableObject* Mesh, const xr_string& OutputFile);
};
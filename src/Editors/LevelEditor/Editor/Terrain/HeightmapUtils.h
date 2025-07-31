#pragma once
#include "HeightMap.h"

namespace XRay::Editor::HeightmapUtils
{
	void GenerateMeshByHeightmap(const SHeightMap& heightmap, CEditableObject* OutMesh);
	void GenerateHeightmapByMesh(CEditableObject* Mesh, const xr_string& OutputFile);
};
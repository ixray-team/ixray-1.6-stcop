#pragma once
#include "HeightMap.h"

namespace XRay::Editor::HeightmapUtils
{
	void GenerateMeshByHeightmap(const SHeightMap& heightmap, CEditableObject* OutMesh, int ScaleY);
	void GenerateHeightmapByMesh(CEditableObject* Mesh, const xr_string& OutputFile);
};
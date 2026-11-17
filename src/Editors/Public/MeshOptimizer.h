#pragma once

#include <cstddef>
#include <cstdint>

// Thin wrapper over DirectXMesh. Its translation unit includes no engine headers,
// so the DXVK Windows type shims it pulls in on Linux never meet xrCore's ones.
namespace MeshOptimizer
{
	// Remap[NewFace] = OldFace for a post-transform vertex cache friendly face order
	bool OptimizeFacesLRU(const uint16_t* Indices, size_t FaceCount, uint32_t* Remap);

	// Remap[OldVertex] = NewVertex for a pre-transform vertex cache friendly vertex order
	bool OptimizeVertices(const uint16_t* Indices, size_t FaceCount, size_t VertexCount, uint32_t* Remap);
}

#include "MeshOptimizer.h"

#include <DirectXMesh.h>

namespace MeshOptimizer
{
	bool OptimizeFacesLRU(const uint16_t* Indices, size_t FaceCount, uint32_t* Remap)
	{
		return SUCCEEDED(DirectX::OptimizeFacesLRU(Indices, FaceCount, Remap));
	}

	bool OptimizeVertices(const uint16_t* Indices, size_t FaceCount, size_t VertexCount, uint32_t* Remap)
	{
		return SUCCEEDED(DirectX::OptimizeVertices(Indices, FaceCount, VertexCount, Remap));
	}
}
